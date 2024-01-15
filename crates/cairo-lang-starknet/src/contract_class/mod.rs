#[cfg(not(feature = "std"))]
use alloc::{string::String, vec::Vec};

use cairo_lang_utils::bigint::BigUintAsHex;
#[cfg(feature = "serde")]
use cairo_lang_utils::bigint::{deserialize_big_uint, serialize_big_uint};
use num_bigint::BigUint;

use crate::abi::Contract;

#[cfg(feature = "std")]
mod compile;
#[cfg(feature = "std")]
pub use compile::*;

#[cfg(test)]
#[path = "serde_test.rs"]
mod test;

/// Represents a contract in the Starknet network.
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ContractClass {
    pub sierra_program: Vec<BigUintAsHex>,
    #[cfg(feature = "std")]
    pub sierra_program_debug_info: Option<cairo_lang_sierra::debug_info::DebugInfo>,
    pub contract_class_version: String,
    pub entry_points_by_type: ContractEntryPoints,
    pub abi: Option<Contract>,
}
impl ContractClass {
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

#[derive(Clone, Default, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ContractEntryPoints {
    #[cfg_attr(feature = "serde", serde(rename = "EXTERNAL"))]
    pub external: Vec<ContractEntryPoint>,
    #[cfg_attr(feature = "serde", serde(rename = "L1_HANDLER"))]
    pub l1_handler: Vec<ContractEntryPoint>,
    #[cfg_attr(feature = "serde", serde(rename = "CONSTRUCTOR"))]
    pub constructor: Vec<ContractEntryPoint>,
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ContractEntryPoint {
    /// A field element that encodes the signature of the called function.
    #[cfg_attr(
        feature = "serde",
        serde(serialize_with = "serialize_big_uint", deserialize_with = "deserialize_big_uint")
    )]
    pub selector: BigUint,
    /// The idx of the user function declaration in the sierra program.
    pub function_idx: usize,
}
