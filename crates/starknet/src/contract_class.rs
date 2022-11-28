use std::path::Path;
use std::sync::Arc;

use anyhow::Context;
use itertools::join;
use num_bigint::BigUint;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use compiler::{CompilerConfig, CompilerDatabase, Setup};
use defs::db::DefsGroup;
use plugins::get_default_plugins;
use semantic::db::SemanticGroup;
use sierra::{self};

use crate::abi;
use crate::casm_contract_class::{deserialize_big_uint, serialize_big_uint};
use crate::contract::{find_contract_structs, resolve_contract_impls};
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
    pub function_id: usize,
}

/// Compile the contract given by path.
/// If `replace_ids` is true, replaces sierra ids with human-readable ones.
pub fn compile_path(path: &Path, replace_ids: bool) -> anyhow::Result<ContractClass> {
    let mut compiler = CompilerDatabase::new(CompilerConfig {
        replace_ids,
        ..CompilerConfig::default()
    });

    let mut plugins = get_default_plugins();
    plugins.push(Arc::new(StarkNetPlugin {}));
    compiler.upcast_mut().set_macro_plugins(plugins);

    let sierra_program = compiler.compile(Setup::Path(path.into()))?;

    let contracts = find_contract_structs(compiler.upcast());
    let contract = match &contracts[..] {
        [contract] => contract,
        [] => anyhow::bail!("Contract not found."),
        _ => {
            anyhow::bail!(
                "Compilation unit must include only one contract. found: {}.",
                join(contracts.iter().map(|contract| contract.struct_id.name(compiler.upcast())), ", ")
            )
        }
    };

    let concrete_impl_id = match resolve_contract_impls(compiler.upcast(), contract)?[..] {
        [concrete_impl_id] => concrete_impl_id,
        [] => anyhow::bail!("A contract must have at least one impl."),
        _ => {
            anyhow::bail!("Only contracts with a single impl are currently supported.")
        }
    };

    let concrete_trait_id = compiler.upcast()
        .impl_trait(compiler.upcast().lookup_intern_concrete_impl(concrete_impl_id).impl_id)
        .with_context(|| "Failed to get contract trait.")?;
    let trait_id = compiler.upcast().lookup_intern_concrete_trait(concrete_trait_id).trait_id;

    // TODO(ilya): Get abi and entry points from the code.
    Ok(ContractClass {
        sierra_program: (*sierra_program).clone(),
        entry_points_by_type: ContractEntryPoints::default(),
        abi: abi::Contract::from_trait(compiler.upcast(), trait_id)
            .with_context(|| "Failed to extract contract ABI.")?,
    })
}
