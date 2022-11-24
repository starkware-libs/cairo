use std::collections::HashMap;

use num_bigint::{BigInt, BigUint};
use num_traits::Num;
use serde::ser::Serializer;
use serde::{Deserialize, Deserializer, Serialize};
use sierra_gas::{calc_gas_info, CostError};
use sierra_to_casm::compiler::CompilationError;
use sierra_to_casm::metadata::Metadata;
use thiserror::Error;

use crate::contract_class::{ContractClass, ContractEntryPoints};

#[derive(Error, Debug, Eq, PartialEq)]
pub enum StarknetSierraCompilationError {
    #[error(transparent)]
    CompilationError(#[from] CompilationError),
    #[error(transparent)]
    CostError(#[from] CostError),
}

/// Represents a contract in the StarkNet network.
#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CasmContractClass {
    pub bytecode: Vec<BigInt>,
    pub hints: Vec<(usize, Vec<String>)>,
    pub entry_points_by_type: ContractEntryPoints,
}

impl CasmContractClass {
    pub fn from_contract_class(
        contract_class: ContractClass,
    ) -> Result<Self, StarknetSierraCompilationError> {
        let program = contract_class.sierra_program;
        let gas_info = calc_gas_info(&program)?;

        let gas_usage_check = true;
        let cairo_program = sierra_to_casm::compiler::compile(
            &program,
            &Metadata { function_ap_change: HashMap::new(), gas_info },
            gas_usage_check,
        )?;

        let mut bytecode = vec![];
        let mut hints = vec![];
        for instruction in cairo_program.instructions {
            if !instruction.hints.is_empty() {
                hints.push((
                    bytecode.len(),
                    instruction.hints.iter().map(|hint| hint.to_string()).collect(),
                ))
            }
            bytecode.extend(instruction.assemble().encode());
        }

        // TODO(ilya): Fix entry points.

        Ok(Self { bytecode, hints, entry_points_by_type: ContractEntryPoints::default() })
    }
}

#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContractEntryPoint {
    /// A field element that encodes the signature of the called function.
    #[serde(serialize_with = "serialize_big_uint", deserialize_with = "deserialize_big_uint")]
    pub selector: BigUint,
    /// The offset of the instruction that should be called within the contract bytecode.
    pub offset: usize,
}

pub fn serialize_big_uint<S>(num: &BigUint, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_str(&format!("{:#x}", num))
}

pub fn deserialize_big_uint<'a, D>(deserializer: D) -> Result<BigUint, D::Error>
where
    D: Deserializer<'a>,
{
    let s = &String::deserialize(deserializer)?;
    match s.strip_prefix("0x") {
        Some(num_no_prefix) => BigUint::from_str_radix(num_no_prefix, 16)
            .map_err(|error| serde::de::Error::custom(format!("{}", error))),
        None => Err(serde::de::Error::custom(format!("{s} does not start with `0x` is missing."))),
    }
}
