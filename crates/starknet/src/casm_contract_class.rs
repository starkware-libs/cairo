use std::collections::HashMap;

use num_bigint::BigInt;
use serde::{Deserialize, Serialize};
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
