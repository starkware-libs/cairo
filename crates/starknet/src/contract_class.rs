use std::path::Path;
use std::sync::Arc;

use anyhow::Context;
use compiler::db::RootDatabase;
use compiler::diagnostics::check_and_eprint_diagnostics;
use compiler::project::setup_project;
use defs::db::DefsGroup;
use defs::ids::{FreeFunctionId, GenericFunctionId};
use diagnostics::ToOption;
use itertools::Itertools;
use lowering::db::LoweringGroup;
use num_bigint::BigUint;
use plugins::get_default_plugins;
use semantic::corelib::get_core_ty_by_name;
use semantic::db::SemanticGroup;
use semantic::{ConcreteFunction, FunctionLongId};
use serde::{Deserialize, Serialize};
use sierra::{self};
use sierra_generator::canonical_id_replacer::CanonicalReplacer;
use sierra_generator::db::SierraGenGroup;
use sierra_generator::replace_ids::{replace_sierra_ids_in_program, SierraIdReplacer};
use thiserror::Error;

use crate::abi;
use crate::casm_contract_class::{deserialize_big_uint, serialize_big_uint};
use crate::contract::{find_contracts, get_external_functions, starknet_keccak};
use crate::plugin::StarkNetPlugin;

#[cfg(test)]
#[path = "contract_class_test.rs"]
mod test;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum StarknetCompilationError {
    #[error("Invalid entry point.")]
    EntryPointError,
}

/// Represents a contract in the StarkNet network.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContractClass {
    pub sierra_program: sierra::program::Program,
    pub entry_points_by_type: ContractEntryPoints,
    pub abi: abi::Contract,
}

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
    // The idx of the user function declaration in the sierra program.
    pub function_idx: usize,
}

// Compile the contract given by path.
// If `replace_ids` is true, replaces sierra ids with human readable ones.
pub fn compile_path(path: &Path, replace_ids: bool) -> anyhow::Result<ContractClass> {
    let mut db_val = RootDatabase::default();
    let db = &mut db_val;

    let main_crate_ids = setup_project(db, Path::new(&path))?;

    // Override implicit precedence for compatibility with the StarkNet OS.
    db.set_implicit_precedence(Arc::new(
        ["RangeCheck", "Pedersen", "GasBuiltin"]
            .iter()
            .map(|name| get_core_ty_by_name(db, name.into(), vec![]))
            .collect_vec(),
    ));

    let mut plugins = get_default_plugins();
    plugins.push(Arc::new(StarkNetPlugin {}));
    db.set_macro_plugins(plugins);

    if check_and_eprint_diagnostics(db) {
        anyhow::bail!("Failed to compile: {}", path.display());
    }

    let contracts = find_contracts(db, &main_crate_ids);
    let contract = match &contracts[..] {
        [contract] => contract,
        [] => anyhow::bail!("Contract not found."),
        _ => {
            // TODO(ilya): Add contract names.
            anyhow::bail!("Compilation unit must include only one contract.",)
        }
    };

    let external_functions = get_external_functions(db, contract)?;
    let sierra_program = db
        .get_sierra_program_for_functions(external_functions.clone())
        .to_option()
        .with_context(|| "Compilation failed without any diagnostics.")?;

    let replacer = CanonicalReplacer::from_program(&sierra_program);
    let sierra_program = if replace_ids {
        replace_sierra_ids_in_program(db, &sierra_program)
    } else {
        replacer.apply(&sierra_program)
    };

    let entry_points_by_type = get_entry_points(db, &external_functions, &replacer)?;
    // TODO(ilya): fix abi.
    let abi = abi::Contract::default();
    Ok(ContractClass { sierra_program, entry_points_by_type, abi })
}

/// Return the entry points given a trait and a module_id where they are implemented.
fn get_entry_points(
    db: &mut RootDatabase,
    external_functions: &[FreeFunctionId],
    replacer: &CanonicalReplacer,
) -> Result<ContractEntryPoints, anyhow::Error> {
    let mut entry_points_by_type = ContractEntryPoints::default();
    for free_func_id in external_functions {
        let func_id = db.intern_function(FunctionLongId {
            function: ConcreteFunction {
                generic_function: GenericFunctionId::Free(*free_func_id),
                generic_args: vec![],
            },
        });

        let sierra_id = db.intern_sierra_function(func_id);

        entry_points_by_type.external.push(ContractEntryPoint {
            selector: starknet_keccak(free_func_id.name(db).as_bytes()),
            function_idx: replacer.replace_function_id(&sierra_id).id as usize,
        });
    }
    Ok(entry_points_by_type)
}
