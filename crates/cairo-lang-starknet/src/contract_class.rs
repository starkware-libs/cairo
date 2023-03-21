use std::path::Path;
use std::sync::Arc;

use anyhow::{ensure, Context, Result};
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_compiler::CompilerConfig;
use cairo_lang_diagnostics::ToOption;
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::{ConcreteFunctionWithBodyId, FunctionLongId};
use cairo_lang_sierra_generator::canonical_id_replacer::CanonicalReplacer;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::replace_ids::{replace_sierra_ids_in_program, SierraIdReplacer};
use cairo_lang_utils::bigint::{deserialize_big_uint, serialize_big_uint, BigUintAsHex};
use itertools::{chain, Itertools};
use num_bigint::BigUint;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::abi::{AbiBuilder, Contract};
use crate::allowed_libfuncs::AllowedLibfuncsError;
use crate::contract::{
    find_contracts, get_abi, get_module_functions, starknet_keccak, ContractDeclaration,
};
use crate::db::StarknetRootDatabaseBuilderEx;
use crate::felt252_serde::sierra_to_felt252s;
use crate::plugin::consts::{CONSTRUCTOR_MODULE, EXTERNAL_MODULE, L1_HANDLER_MODULE};
use crate::sierra_version::{self};

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
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContractClass {
    pub sierra_program: Vec<BigUintAsHex>,
    pub sierra_program_debug_info: Option<cairo_lang_sierra::debug_info::DebugInfo>,
    pub contract_class_version: String,
    pub entry_points_by_type: ContractEntryPoints,
    pub abi: Option<Contract>,
}

const DEFAULT_CONTRACT_CLASS_VERSION: &str = "0.1.0";

#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContractEntryPoints {
    #[serde(rename = "EXTERNAL")]
    pub external: Vec<ContractEntryPoint>,
    #[serde(rename = "L1_HANDLER")]
    pub l1_handler: Vec<ContractEntryPoint>,
    #[serde(rename = "CONSTRUCTOR")]
    pub constructor: Vec<ContractEntryPoint>,
}

#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContractEntryPoint {
    /// A field element that encodes the signature of the called function.
    #[serde(serialize_with = "serialize_big_uint", deserialize_with = "deserialize_big_uint")]
    pub selector: BigUint,
    /// The idx of the user function declaration in the sierra program.
    pub function_idx: usize,
}

/// Compile the contract given by path.
///
/// Errors if no contracts or more than 1 are found.
pub fn compile_path(path: &Path, compiler_config: CompilerConfig<'_>) -> Result<ContractClass> {
    let mut db = RootDatabase::builder().detect_corelib().with_starknet().build()?;

    let main_crate_ids = setup_project(&mut db, Path::new(&path))?;

    compile_only_contract_in_prepared_db(&mut db, main_crate_ids, compiler_config)
}

/// Runs StarkNet contract compiler on the only contract defined in main crates.
///
/// This function will return an error if no, or more than 1 contract is found.
fn compile_only_contract_in_prepared_db(
    db: &mut RootDatabase,
    main_crate_ids: Vec<CrateId>,
    compiler_config: CompilerConfig<'_>,
) -> Result<ContractClass> {
    let contracts = find_contracts(db, &main_crate_ids);
    ensure!(!contracts.is_empty(), "Contract not found.");
    // TODO(ilya): Add contract names.
    ensure!(contracts.len() == 1, "Compilation unit must include only one contract.");

    let contracts = contracts.iter().collect::<Vec<_>>();
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
    db: &mut RootDatabase,
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
    db: &mut RootDatabase,
    contract: &ContractDeclaration,
    compiler_config: &CompilerConfig<'_>,
) -> Result<ContractClass> {
    let external_functions: Vec<_> = get_module_functions(db, contract, EXTERNAL_MODULE)?
        .into_iter()
        .flat_map(|f| ConcreteFunctionWithBodyId::from_no_generics_free(db, f))
        .collect();
    let l1_handler_functions: Vec<_> = get_module_functions(db, contract, L1_HANDLER_MODULE)?
        .into_iter()
        .flat_map(|f| ConcreteFunctionWithBodyId::from_no_generics_free(db, f))
        .collect();
    let constructor_functions: Vec<_> = get_module_functions(db, contract, CONSTRUCTOR_MODULE)?
        .into_iter()
        .flat_map(|f| ConcreteFunctionWithBodyId::from_no_generics_free(db, f))
        .collect();
    let mut sierra_program = db
        .get_sierra_program_for_functions(
            chain!(&external_functions, &l1_handler_functions, &constructor_functions)
                .cloned()
                .collect(),
        )
        .to_option()
        .with_context(|| "Compilation failed without any diagnostics.")?;

    if compiler_config.replace_ids {
        sierra_program = Arc::new(replace_sierra_ids_in_program(db, &sierra_program));
    }
    let replacer = CanonicalReplacer::from_program(&sierra_program);
    let sierra_program = replacer.apply(&sierra_program);

    let entry_points_by_type = ContractEntryPoints {
        external: get_entry_points(db, &external_functions, &replacer)?,
        l1_handler: get_entry_points(db, &l1_handler_functions, &replacer)?,
        /// TODO(orizi): Validate there is at most one constructor.
        constructor: get_entry_points(db, &constructor_functions, &replacer)?,
    };
    let contract_class = ContractClass {
        sierra_program: sierra_to_felt252s(
            sierra_version::VersionId::current_version_id(),
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

/// Returns the entry points given their IDs sorted by selectors.
fn get_entry_points(
    db: &mut RootDatabase,
    entry_point_functions: &[ConcreteFunctionWithBodyId],
    replacer: &CanonicalReplacer,
) -> Result<Vec<ContractEntryPoint>> {
    let mut entry_points = vec![];
    for function_with_body_id in entry_point_functions {
        let function_id = db.intern_function(FunctionLongId {
            function: function_with_body_id
                .concrete(db)
                .to_option()
                .with_context(|| "Function error.")?,
        });

        let sierra_id = db.intern_sierra_function(function_id);

        entry_points.push(ContractEntryPoint {
            selector: starknet_keccak(
                function_with_body_id.function_with_body_id(db).name(db).as_bytes(),
            ),
            function_idx: replacer.replace_function_id(&sierra_id).id as usize,
        });
    }
    entry_points.sort_by(|a, b| a.selector.cmp(&b.selector));
    Ok(entry_points)
}
