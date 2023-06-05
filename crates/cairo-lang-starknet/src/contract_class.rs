use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::{Context, Result};
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_compiler::CompilerConfig;
use cairo_lang_defs::ids::TopLevelLanguageElementId;
use cairo_lang_diagnostics::ToOption;
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_sierra_generator::canonical_id_replacer::CanonicalReplacer;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::replace_ids::{replace_sierra_ids_in_program, SierraIdReplacer};
use cairo_lang_utils::bigint::{deserialize_big_uint, serialize_big_uint, BigUintAsHex};
use itertools::{chain, Itertools};
use num_bigint::BigUint;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::abi::{AbiBuilder, Contract};
use crate::allowed_libfuncs::{
    validate_compatible_sierra_version, AllowedLibfuncsError, ListSelector,
};
use crate::compiler_version::{self};
use crate::contract::{
    find_contracts, get_abi, get_module_functions, get_selector_and_sierra_function,
    ContractDeclaration,
};
use crate::felt252_serde::sierra_to_felt252s;
use crate::plugin::consts::{CONSTRUCTOR_MODULE, EXTERNAL_MODULE, L1_HANDLER_MODULE};
use crate::plugin::StarkNetPlugin;

#[cfg(test)]
#[path = "contract_class_test.rs"]
mod test;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum StarknetCompilationError {
    #[error("Invalid entry point.")]
    EntryPointError,
    #[error(transparent)]
    AllowedLibfuncsError(#[from] AllowedLibfuncsError),
}

/// Represents a contract in the Starknet network.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContractClass {
    pub sierra_program: Vec<BigUintAsHex>,
    pub sierra_program_debug_info: Option<cairo_lang_sierra::debug_info::DebugInfo>,
    pub contract_class_version: String,
    pub entry_points_by_type: ContractEntryPoints,
    pub abi: Option<Contract>,
}

const DEFAULT_CONTRACT_CLASS_VERSION: &str = "0.1.0";

#[derive(Clone, Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContractEntryPoints {
    #[serde(rename = "EXTERNAL")]
    pub external: Vec<ContractEntryPoint>,
    #[serde(rename = "L1_HANDLER")]
    pub l1_handler: Vec<ContractEntryPoint>,
    #[serde(rename = "CONSTRUCTOR")]
    pub constructor: Vec<ContractEntryPoint>,
}

#[derive(Clone, Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContractEntryPoint {
    /// A field element that encodes the signature of the called function.
    #[serde(serialize_with = "serialize_big_uint", deserialize_with = "deserialize_big_uint")]
    pub selector: BigUint,
    /// The idx of the user function declaration in the sierra program.
    pub function_idx: usize,
}

/// Compile the contract given by path.
/// Errors if there is ambiguity.
pub fn compile_path(
    path: &Path,
    contract_path: Option<&str>,
    compiler_config: CompilerConfig<'_>,
) -> Result<ContractClass> {
    let mut db = RootDatabase::builder()
        .detect_corelib()
        .with_semantic_plugin(Arc::new(StarkNetPlugin::default()))
        .build()?;

    let main_crate_ids = setup_project(&mut db, Path::new(&path))?;

    compile_contract_in_prepared_db(&db, contract_path, main_crate_ids, compiler_config)
}

/// Runs StarkNet contract compiler on the specified contract.
/// If no contract was specified, verify that there is only one.
/// Otherwise, return an error.
pub(crate) fn compile_contract_in_prepared_db(
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
    let mut sierra_program = db
        .get_sierra_program_for_functions(
            chain!(&external, &l1_handler, &constructor).cloned().collect(),
        )
        .to_option()
        .with_context(|| "Compilation failed without any diagnostics.")?;

    if compiler_config.replace_ids {
        sierra_program = Arc::new(replace_sierra_ids_in_program(db, &sierra_program));
    }
    let replacer = CanonicalReplacer::from_program(&sierra_program);
    let sierra_program = replacer.apply(&sierra_program);

    let entry_points_by_type = ContractEntryPoints {
        external: get_entry_points(db, &external, &replacer)?,
        l1_handler: get_entry_points(db, &l1_handler, &replacer)?,
        /// TODO(orizi): Validate there is at most one constructor.
        constructor: get_entry_points(db, &constructor, &replacer)?,
    };
    let contract_class = ContractClass {
        sierra_program: sierra_to_felt252s(
            compiler_version::current_sierra_version_id(),
            compiler_version::current_compiler_version_id(),
            &sierra_program,
        )?,
        sierra_program_debug_info: Some(cairo_lang_sierra::debug_info::DebugInfo::extract(
            &sierra_program,
        )),
        contract_class_version: DEFAULT_CONTRACT_CLASS_VERSION.to_string(),
        entry_points_by_type,
        abi: Some(AbiBuilder::from_trait(db, get_abi(db, contract)?).with_context(|| "ABI error")?),
    };
    Ok(contract_class)
}

pub struct SemanticEntryPoints {
    pub external: Vec<ConcreteFunctionWithBodyId>,
    pub l1_handler: Vec<ConcreteFunctionWithBodyId>,
    pub constructor: Vec<ConcreteFunctionWithBodyId>,
}

/// Extracts functions from the contract.
pub fn extract_semantic_entrypoints(
    db: &dyn SierraGenGroup,
    contract: &ContractDeclaration,
) -> core::result::Result<SemanticEntryPoints, anyhow::Error> {
    let external: Vec<_> = get_module_functions(db.upcast(), contract, EXTERNAL_MODULE)?
        .into_iter()
        .flat_map(|f| ConcreteFunctionWithBodyId::from_no_generics_free(db.upcast(), f))
        .collect();
    let l1_handler: Vec<_> = get_module_functions(db.upcast(), contract, L1_HANDLER_MODULE)?
        .into_iter()
        .flat_map(|f| ConcreteFunctionWithBodyId::from_no_generics_free(db.upcast(), f))
        .collect();
    let constructor: Vec<_> = get_module_functions(db.upcast(), contract, CONSTRUCTOR_MODULE)?
        .into_iter()
        .flat_map(|f| ConcreteFunctionWithBodyId::from_no_generics_free(db.upcast(), f))
        .collect();
    if constructor.len() > 1 {
        anyhow::bail!("Expected at most one constructor.");
    }
    Ok(SemanticEntryPoints { external, l1_handler, constructor })
}

/// Returns the entry points given their IDs sorted by selectors.
fn get_entry_points(
    db: &RootDatabase,
    entry_point_functions: &[ConcreteFunctionWithBodyId],
    replacer: &CanonicalReplacer,
) -> Result<Vec<ContractEntryPoint>> {
    let mut entry_points = vec![];
    for function_with_body_id in entry_point_functions {
        let (selector, sierra_id) =
            get_selector_and_sierra_function(db, *function_with_body_id, replacer);

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
    let contract = compile_path(
        &crate_path,
        contract_path.as_deref(),
        if let Some(config) = config { config } else { CompilerConfig::default() },
    )?;
    validate_compatible_sierra_version(
        &contract,
        if let Some(allowed_libfuncs_list) = allowed_libfuncs_list {
            allowed_libfuncs_list
        } else {
            ListSelector::default()
        },
    )?;
    serde_json::to_string_pretty(&contract).with_context(|| "Serialization failed.")
}
