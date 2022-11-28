use std::path::Path;

use num_bigint::BigUint;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use compiler::{CompileArgs, CompileProjectConfig, CompileResult};
use compiler::diagnostics::eprint_diagnostic;
use sierra::{self};

use crate::abi;
use crate::casm_contract_class::{deserialize_big_uint, serialize_big_uint};

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
    let CompileResult { sierra_program, .. } = compiler::compile(CompileArgs {
        compile: CompileProjectConfig::LoadFromPath { path: path.into() },
        on_diagnostic: Some(&mut eprint_diagnostic),
        replace_ids,
    })?;

    // TODO(ilya): Get abi and entry points from the code.
    Ok(ContractClass {
        sierra_program: (*sierra_program).clone(),
        entry_points_by_type: ContractEntryPoints::default(),
        abi: abi::Contract::default(),
    })
}
