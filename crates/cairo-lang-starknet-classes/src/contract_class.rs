use cairo_lang_sierra as sierra;
use cairo_lang_utils::bigint::{deserialize_big_uint, serialize_big_uint, BigUintAsHex};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use num_bigint::BigUint;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use thiserror::Error;

use crate::abi::Contract;
use crate::allowed_libfuncs::{lookup_allowed_libfuncs_list, AllowedLibfuncsError, ListSelector};
use crate::compiler_version::{current_compiler_version_id, current_sierra_version_id};
use crate::felt252_serde::{sierra_from_felt252s, sierra_to_felt252s, Felt252SerdeError};

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
    pub sierra_program_debug_info: Option<sierra::debug_info::DebugInfo>,
    pub contract_class_version: String,
    pub entry_points_by_type: ContractEntryPoints,
    pub abi: Option<Contract>,
}
impl ContractClass {
    /// Extracts the contract class from the given contract declaration.
    pub fn new(
        program: &sierra::program::Program,
        entry_points_by_type: ContractEntryPoints,
        abi: Option<Contract>,
        annotations: OrderedHashMap<String, Value>,
    ) -> Result<Self, Felt252SerdeError> {
        let mut sierra_program_debug_info = sierra::debug_info::DebugInfo::extract(program);
        sierra_program_debug_info.annotations.extend(annotations);

        Ok(Self {
            sierra_program: sierra_to_felt252s(
                current_sierra_version_id(),
                current_compiler_version_id(),
                program,
            )?,
            sierra_program_debug_info: Some(sierra_program_debug_info),
            contract_class_version: DEFAULT_CONTRACT_CLASS_VERSION.into(),
            entry_points_by_type,
            abi,
        })
    }

    /// Extracts Sierra program from the ContractClass and populates it with debug info if
    /// available.
    pub fn extract_sierra_program(&self) -> Result<sierra::program::Program, Felt252SerdeError> {
        let (_, _, mut sierra_program) = sierra_from_felt252s(&self.sierra_program)?;
        if let Some(info) = &self.sierra_program_debug_info {
            info.populate(&mut sierra_program);
        }
        Ok(sierra_program)
    }

    /// Sanity checks the contract class.
    /// Currently only checks that if ABI exists, its counts match the entry points counts.
    pub fn sanity_check(&self) {
        if let Some(abi) = &self.abi {
            abi.sanity_check(
                self.entry_points_by_type.external.len(),
                self.entry_points_by_type.l1_handler.len(),
                self.entry_points_by_type.constructor.len(),
            );
        }
    }

    /// Checks that all the used libfuncs in the contract class are allowed in the contract class
    /// sierra version.
    pub fn validate_version_compatible(
        self: &ContractClass,
        list_selector: ListSelector,
    ) -> Result<(), AllowedLibfuncsError> {
        let list_name = list_selector.to_string();
        let allowed_libfuncs = lookup_allowed_libfuncs_list(list_selector)?;
        let (_, _, sierra_program) = sierra_from_felt252s(&self.sierra_program)
            .map_err(|_| AllowedLibfuncsError::SierraProgramError)?;
        for libfunc in sierra_program.libfunc_declarations.iter() {
            if !allowed_libfuncs.allowed_libfuncs.contains(&libfunc.long_id.generic_id) {
                return Err(AllowedLibfuncsError::UnsupportedLibfunc {
                    invalid_libfunc: libfunc.long_id.generic_id.to_string(),
                    allowed_libfuncs_list_name: list_name,
                });
            }
        }
        Ok(())
    }
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
