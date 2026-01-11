use cairo_lang_sierra as sierra;
use cairo_lang_utils::bigint::{BigUintAsHex, deserialize_big_uint, serialize_big_uint};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use num_bigint::BigUint;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use starknet_types_core::felt::Felt as Felt252;
use thiserror::Error;

use crate::abi::Contract;
use crate::allowed_libfuncs::{AllowedLibfuncsError, ListSelector, lookup_allowed_libfuncs_list};
use crate::compiler_version::{VersionId, current_compiler_version_id, current_sierra_version_id};
use crate::felt252_serde::{
    Felt252SerdeError, sierra_from_felt252s, sierra_to_felt252s, version_id_from_felt252s,
};

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

    /// Extracts Sierra program from the ContractClass into `ExtractedSierraProgram` and populates
    /// it with debug info if `populate_debug_info` is true, and the data is available.
    pub fn extract_sierra_program(
        &self,
        populate_debug_info: bool,
    ) -> Result<ExtractedSierraProgram, Felt252SerdeError> {
        let prime = Felt252::prime();
        for felt252 in &self.sierra_program {
            if felt252.value >= prime {
                return Err(Felt252SerdeError::InvalidInputForDeserialization);
            }
        }
        let (sierra_version, compiler_version, mut program) =
            sierra_from_felt252s(&self.sierra_program)?;
        if populate_debug_info && let Some(info) = &self.sierra_program_debug_info {
            info.populate(&mut program);
        }
        Ok(ExtractedSierraProgram { program, sierra_version, compiler_version })
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
}

/// The Sierra program extracted from a contract class.
pub struct ExtractedSierraProgram {
    /// The actual Sierra program.
    pub program: sierra::program::Program,
    /// The Sierra version used for the program.
    pub sierra_version: VersionId,
    /// The compiler version used for the program.
    pub compiler_version: VersionId,
}
impl ExtractedSierraProgram {
    /// Checks that all the used libfuncs in the contract class are allowed in the contract class
    /// Sierra version.
    pub fn validate_version_compatible(
        &self,
        list_selector: ListSelector,
    ) -> Result<(), AllowedLibfuncsError> {
        let list_name = list_selector.to_string();
        let allowed_libfuncs = lookup_allowed_libfuncs_list(list_selector)?;
        for libfunc in &self.program.libfunc_declarations {
            match allowed_libfuncs.allowed_libfuncs.get(&libfunc.long_id.generic_id) {
                Some(None) => {}
                Some(Some(required)) if self.sierra_version.supports(*required) => {}
                Some(Some(required)) => {
                    return Err(AllowedLibfuncsError::UnsupportedLibfuncAtVersion {
                        invalid_libfunc: libfunc.long_id.generic_id.to_string(),
                        required_version: *required,
                        class_version: self.sierra_version,
                    });
                }
                None => {
                    return Err(AllowedLibfuncsError::UnsupportedLibfunc {
                        invalid_libfunc: libfunc.long_id.generic_id.to_string(),
                        allowed_libfuncs_list_name: list_name,
                    });
                }
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
    /// The index of the user function declaration in the Sierra program.
    pub function_idx: usize,
}

/// Deserializes the versions from the header of a Sierra program represented as a slice of
/// felt252s.
///
/// Returns (sierra_version_id, compiler_version_id).
/// See [crate::compiler_version].
pub fn version_id_from_serialized_sierra_program(
    sierra_program: &[BigUintAsHex],
) -> Result<(VersionId, VersionId), Felt252SerdeError> {
    let (sierra_version_id, compiler_version_id, _) = version_id_from_felt252s(sierra_program)?;
    Ok((sierra_version_id, compiler_version_id))
}
