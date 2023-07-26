#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(not(feature = "std"))]
use alloc::{string::String, vec::Vec};

use cairo_lang_casm::hints::Hint;
use cairo_lang_utils::bigint::BigUintAsHex;
#[cfg(feature = "serde")]
use cairo_lang_utils::bigint::{deserialize_big_uint, serialize_big_uint};
use num_bigint::BigUint;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Represents a contract in the Starknet network.
#[derive(Clone, Default, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct CasmContractClass {
    #[cfg_attr(
        feature = "serde",
        serde(serialize_with = "serialize_big_uint", deserialize_with = "deserialize_big_uint")
    )]
    pub prime: BigUint,
    pub compiler_version: String,
    pub bytecode: Vec<BigUintAsHex>,
    pub hints: Vec<(usize, Vec<Hint>)>,

    // Optional pythonic hints in a format that can be executed by the python vm.
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Option::is_none"))]
    pub pythonic_hints: Option<Vec<(usize, Vec<String>)>>,
    pub entry_points_by_type: CasmContractEntryPoints,
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct CasmContractEntryPoints {
    #[cfg_attr(feature = "serde", serde(rename = "EXTERNAL"))]
    pub external: Vec<CasmContractEntryPoint>,
    #[cfg_attr(feature = "serde", serde(rename = "L1_HANDLER"))]
    pub l1_handler: Vec<CasmContractEntryPoint>,
    #[cfg_attr(feature = "serde", serde(rename = "CONSTRUCTOR"))]
    pub constructor: Vec<CasmContractEntryPoint>,
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct CasmContractEntryPoint {
    /// A field element that encodes the signature of the called function.
    #[cfg_attr(
        feature = "serde",
        serde(serialize_with = "serialize_big_uint", deserialize_with = "deserialize_big_uint")
    )]
    pub selector: BigUint,
    /// The offset of the instruction that should be called within the contract bytecode.
    pub offset: usize,
    // list of builtins.
    pub builtins: Vec<String>,
}
