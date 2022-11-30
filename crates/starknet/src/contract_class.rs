use std::path::Path;
use std::sync::Arc;

use anyhow::Context;
use compiler::db::RootDatabase;
use compiler::diagnostics::check_and_eprint_diagnostics;
use compiler::project::setup_project;
use defs::db::DefsGroup;
use defs::ids::{GenericFunctionId, LanguageElementId, ModuleId, ModuleItemId, TraitId};
use itertools::join;
use num_bigint::BigUint;
use plugins::get_default_plugins;
use semantic::db::SemanticGroup;
use semantic::{ConcreteFunction, FunctionLongId};
use serde::{Deserialize, Serialize};
use sierra::{self};
use sierra_generator::canonical_id_replacer::CanonicalReplacer;
use sierra_generator::db::SierraGenGroup;
use sierra_generator::replace_ids::{replace_sierra_ids_in_program, SierraIdReplacer};
use thiserror::Error;
use utils::try_extract_matches;

use crate::abi;
use crate::casm_contract_class::{deserialize_big_uint, serialize_big_uint};
use crate::contract::{find_contract_structs, resolve_contract_impls, starknet_keccak};
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
    // The function in the sierra program.
    pub function_id: u64,
}

// Compile the contract given by path.
// If `replace_ids` is true, replaces sierra ids with human readable ones.
pub fn compile_path(path: &Path, replace_ids: bool) -> anyhow::Result<ContractClass> {
    let mut db_val = RootDatabase::default();
    let db = &mut db_val;

    let main_crate_ids = setup_project(db, Path::new(&path))?;

    let mut plugins = get_default_plugins();
    plugins.push(Arc::new(StarkNetPlugin {}));
    db.set_macro_plugins(plugins);

    if check_and_eprint_diagnostics(db) {
        anyhow::bail!("Failed to compile: {}", path.display());
    }

    let contracts = find_contract_structs(db);
    let contract = match &contracts[..] {
        [contract] => contract,
        [] => anyhow::bail!("Contract not found."),
        _ => {
            anyhow::bail!(
                "Compilation unit must include only one contract. found: {}.",
                join(contracts.iter().map(|contract| contract.struct_id.name(db)), ", ")
            )
        }
    };

    let concrete_impl_id = match resolve_contract_impls(db, contract)?[..] {
        [concrete_impl_id] => concrete_impl_id,
        [] => anyhow::bail!("A contract must have at least one impl."),
        _ => {
            anyhow::bail!("Only contracts with a single impl are currently supported.")
        }
    };

    let impl_id = db.lookup_intern_concrete_impl(concrete_impl_id).impl_id;

    let concrete_trait_id =
        db.impl_trait(impl_id).with_context(|| "Failed to get contract trait.")?;
    let trait_id = db.lookup_intern_concrete_trait(concrete_trait_id).trait_id;

    let sierra_program = db
        .get_sierra_program(main_crate_ids)
        .with_context(|| "Compilation failed without any diagnostics.")?;

    let replacer = CanonicalReplacer::from_program(&sierra_program);
    let sierra_program = if replace_ids {
        replace_sierra_ids_in_program(db, &sierra_program)
    } else {
        replacer.apply(&sierra_program)
    };

    let entry_points_by_type = get_entry_points(db, impl_id.module(db), trait_id, &replacer)?;

    Ok(ContractClass {
        sierra_program,
        entry_points_by_type,
        abi: abi::Contract::from_trait(db, trait_id)
            .with_context(|| "Failed to extract contract ABI.")?,
    })
}

/// Return the entry points given a trait and a module_id where they are implemented.
fn get_entry_points(
    db: &mut RootDatabase,
    impl_module_id: ModuleId,
    trait_id: TraitId,
    replacer: &CanonicalReplacer,
) -> Result<ContractEntryPoints, anyhow::Error> {
    let trait_functions = db.trait_functions(trait_id).unwrap();
    let mut entry_points_by_type = ContractEntryPoints::default();
    for function_name in trait_functions.keys() {
        let item = db
            .module_item_by_name(impl_module_id, function_name.clone())
            .with_context(|| format!("The `{}` entry point was not found.", function_name))?;

        let free_func_id = try_extract_matches!(item, ModuleItemId::FreeFunction)
            .with_context(|| format!("Expected `{}` to be a function.", function_name))?;

        let func_id = db.intern_function(FunctionLongId {
            function: ConcreteFunction {
                generic_function: GenericFunctionId::Free(free_func_id),
                generic_args: vec![],
            },
        });

        let sierra_id = db.intern_sierra_function(func_id);

        entry_points_by_type.external.push(ContractEntryPoint {
            selector: starknet_keccak(function_name.as_bytes()),
            function_id: replacer.replace_function_id(&sierra_id).id,
        });
    }
    Ok(entry_points_by_type)
}
