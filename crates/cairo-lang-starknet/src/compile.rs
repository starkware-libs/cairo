use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::{Context, Result};
use cairo_lang_compiler::CompilerConfig;
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_defs::ids::TopLevelLanguageElementId;
use cairo_lang_diagnostics::ToOption;
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_sierra::debug_info::Annotations;
use cairo_lang_sierra_generator::canonical_id_replacer::CanonicalReplacer;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::program_generator::SierraProgramWithDebug;
use cairo_lang_sierra_generator::replace_ids::{SierraIdReplacer, replace_sierra_ids_in_program};
use cairo_lang_starknet_classes::allowed_libfuncs::ListSelector;
use cairo_lang_starknet_classes::contract_class::{
    ContractClass, ContractEntryPoint, ContractEntryPoints,
};
use itertools::{Itertools, chain};

use crate::abi::AbiBuilder;
use crate::aliased::Aliased;
use crate::contract::{
    ContractDeclaration, find_contracts, get_contract_abi_functions,
    get_selector_and_sierra_function,
};
use crate::plugin::consts::{CONSTRUCTOR_MODULE, EXTERNAL_MODULE, L1_HANDLER_MODULE};
use crate::starknet_plugin_suite;

#[cfg(test)]
#[path = "compile_test.rs"]
mod test;

/// Compile the contract given by path.
/// Errors if there is ambiguity.
pub fn compile_path(
    path: &Path,
    contract_path: Option<&str>,
    mut compiler_config: CompilerConfig<'_>,
) -> Result<ContractClass> {
    let mut db = RootDatabase::builder()
        .detect_corelib()
        .with_default_plugin_suite(starknet_plugin_suite())
        .build()?;

    let main_crate_ids = setup_project(&mut db, Path::new(&path))?;
    compiler_config.diagnostics_reporter =
        compiler_config.diagnostics_reporter.with_crates(&main_crate_ids);
    compile_contract_in_prepared_db(&db, contract_path, main_crate_ids, compiler_config)
}

/// Runs Starknet contract compiler on the specified contract.
/// If no contract was specified, verify that there is only one.
/// Otherwise, return an error.
pub fn compile_contract_in_prepared_db(
    db: &RootDatabase,
    contract_path: Option<&str>,
    main_crate_ids: Vec<CrateId>,
    mut compiler_config: CompilerConfig<'_>,
) -> Result<ContractClass> {
    let mut contracts = find_contracts(db, &main_crate_ids);

    // TODO(ilya): Add contract names.
    if let Some(contract_path) = contract_path {
        contracts.retain(|contract| contract.submodule_id.full_path(db) == contract_path);
    };
    let contract = match contracts.len() {
        0 => {
            // Report diagnostics as they might reveal the reason why no contract was found.
            compiler_config.diagnostics_reporter.ensure(db)?;
            anyhow::bail!("Contract not found.");
        }
        1 => &contracts[0],
        _ => {
            let contract_names =
                contracts.iter().map(|contract| contract.submodule_id.full_path(db)).join("\n  ");
            anyhow::bail!(
                "More than one contract found in the main crate: \n  {}\nUse --contract-path to \
                 specify which to compile.",
                contract_names
            );
        }
    };

    let contracts = vec![contract];
    let mut classes = compile_prepared_db(db, &contracts, compiler_config)?;
    assert_eq!(classes.len(), 1);
    Ok(classes.remove(0))
}

/// Runs Starknet contracts compiler.
///
/// # Arguments
/// * `db` - Preloaded compilation database.
/// * `contracts` - [`ContractDeclaration`]s to compile. Use [`find_contracts`] to find contracts in
///   `db`.
/// * `compiler_config` - The compiler configuration.
/// # Returns
/// * `Ok(Vec<ContractClass>)` - List of all compiled contract classes found in main crates.
/// * `Err(anyhow::Error)` - Compilation failed.
pub fn compile_prepared_db(
    db: &RootDatabase,
    contracts: &[&ContractDeclaration],
    mut compiler_config: CompilerConfig<'_>,
) -> Result<Vec<ContractClass>> {
    compiler_config.diagnostics_reporter.ensure(db)?;

    contracts
        .iter()
        .map(|contract| {
            compile_contract_with_prepared_and_checked_db(db, contract, &compiler_config)
        })
        .try_collect()
}

/// Compile declared Starknet contract.
///
/// The `contract` value **must** come from `db`, for example as a result of calling
/// [`find_contracts`]. Does not check diagnostics, it is expected that they are checked by caller
/// of this function.
fn compile_contract_with_prepared_and_checked_db(
    db: &RootDatabase,
    contract: &ContractDeclaration,
    compiler_config: &CompilerConfig<'_>,
) -> Result<ContractClass> {
    let SemanticEntryPoints { external, l1_handler, constructor } =
        extract_semantic_entrypoints(db, contract)?;
    let SierraProgramWithDebug { program: mut sierra_program, debug_info } = Arc::unwrap_or_clone(
        db.get_sierra_program_for_functions(
            chain!(&external, &l1_handler, &constructor).map(|f| f.value).collect(),
        )
        .to_option()
        .with_context(|| "Compilation failed without any diagnostics.")?,
    );

    if compiler_config.replace_ids {
        sierra_program = replace_sierra_ids_in_program(db, &sierra_program);
    }
    let replacer = CanonicalReplacer::from_program(&sierra_program);
    let sierra_program = replacer.apply(&sierra_program);

    let entry_points_by_type = ContractEntryPoints {
        external: get_entry_points(db, &external, &replacer)?,
        l1_handler: get_entry_points(db, &l1_handler, &replacer)?,
        // Later generation of ABI verifies that there is up to one constructor.
        constructor: get_entry_points(db, &constructor, &replacer)?,
    };

    let mut annotations = Annotations::default();

    if compiler_config.add_statements_functions {
        let statements_functions = debug_info.statements_locations.extract_statements_functions(db);
        annotations.extend(Annotations::from(statements_functions))
    };

    if compiler_config.add_statements_code_locations {
        let statements_functions =
            debug_info.statements_locations.extract_statements_source_code_locations(db);
        annotations.extend(Annotations::from(statements_functions))
    };

    let contract_class = ContractClass::new(
        &sierra_program,
        entry_points_by_type,
        Some(
            AbiBuilder::from_submodule(db, contract.submodule_id, Default::default())
                .ok()
                .with_context(|| "Unexpected error while generating ABI.")?
                .finalize()
                .with_context(|| "Could not create ABI from contract submodule")?,
        ),
        annotations,
    )?;
    contract_class.sanity_check();
    Ok(contract_class)
}

pub struct SemanticEntryPoints {
    pub external: Vec<Aliased<ConcreteFunctionWithBodyId>>,
    pub l1_handler: Vec<Aliased<ConcreteFunctionWithBodyId>>,
    pub constructor: Vec<Aliased<ConcreteFunctionWithBodyId>>,
}

/// Extracts functions from the contract.
pub fn extract_semantic_entrypoints(
    db: &dyn LoweringGroup,
    contract: &ContractDeclaration,
) -> core::result::Result<SemanticEntryPoints, anyhow::Error> {
    let external: Vec<_> = get_contract_abi_functions(db.upcast(), contract, EXTERNAL_MODULE)?
        .into_iter()
        .map(|f| f.map(|f| ConcreteFunctionWithBodyId::from_semantic(db, f)))
        .collect();
    let l1_handler: Vec<_> = get_contract_abi_functions(db.upcast(), contract, L1_HANDLER_MODULE)?
        .into_iter()
        .map(|f| f.map(|f| ConcreteFunctionWithBodyId::from_semantic(db, f)))
        .collect();
    let constructor: Vec<_> =
        get_contract_abi_functions(db.upcast(), contract, CONSTRUCTOR_MODULE)?
            .into_iter()
            .map(|f| f.map(|f| ConcreteFunctionWithBodyId::from_semantic(db, f)))
            .collect();
    if constructor.len() > 1 {
        anyhow::bail!("Expected at most one constructor.");
    }
    Ok(SemanticEntryPoints { external, l1_handler, constructor })
}

/// Returns the entry points given their IDs sorted by selectors.
fn get_entry_points(
    db: &RootDatabase,
    entry_point_functions: &[Aliased<ConcreteFunctionWithBodyId>],
    replacer: &CanonicalReplacer,
) -> Result<Vec<ContractEntryPoint>> {
    let mut entry_points = vec![];
    for function_with_body_id in entry_point_functions {
        let (selector, sierra_id) =
            get_selector_and_sierra_function(db, function_with_body_id, replacer);

        entry_points.push(ContractEntryPoint {
            selector: selector.to_biguint(),
            function_idx: sierra_id.id as usize,
        });
    }
    entry_points.sort_by(|a, b| a.selector.cmp(&b.selector));
    Ok(entry_points)
}

/// Compile Starknet crate (or specific contract in the crate).
pub fn starknet_compile(
    crate_path: PathBuf,
    contract_path: Option<String>,
    config: Option<CompilerConfig<'_>>,
    allowed_libfuncs_list: Option<ListSelector>,
) -> anyhow::Result<String> {
    let contract = compile_path(&crate_path, contract_path.as_deref(), config.unwrap_or_default())?;
    contract.validate_version_compatible(allowed_libfuncs_list.unwrap_or_default())?;
    serde_json::to_string_pretty(&contract).with_context(|| "Serialization failed.")
}
