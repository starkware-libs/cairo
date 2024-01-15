use cairo_felt::Felt252;
use cairo_lang_casm::hints::Hint;
use cairo_lang_utils::bigint::BigUintAsHex;
use itertools::Itertools;
use num_bigint::BigUint;
use starknet_crypto::{poseidon_hash_many, FieldElement};

#[cfg(not(feature = "std"))]
use alloc::{string::String, vec, vec::Vec};

#[cfg(feature = "std")]
mod from_contract_class;
#[cfg(feature = "std")]
pub use from_contract_class::*;

/// Represents a contract in the Starknet network.
#[derive(Clone, Default, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct CasmContractClass {
    #[cfg_attr(
        feature = "serde",
        serde(
            serialize_with = "cairo_lang_utils::bigint::serialize_big_uint",
            deserialize_with = "cairo_lang_utils::bigint::deserialize_big_uint"
        )
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
impl CasmContractClass {
    /// Returns the hash value for the compiled contract class.
    pub fn compiled_class_hash(&self) -> Felt252 {
        // Compute hashes on each component separately.
        let external_funcs_hash = self.entry_points_hash(&self.entry_points_by_type.external);
        let l1_handlers_hash = self.entry_points_hash(&self.entry_points_by_type.l1_handler);
        let constructors_hash = self.entry_points_hash(&self.entry_points_by_type.constructor);
        let bytecode_hash = poseidon_hash_many(
            &self
                .bytecode
                .iter()
                .map(|big_uint| {
                    FieldElement::from_byte_slice_be(&big_uint.value.to_bytes_be()).unwrap()
                })
                .collect_vec(),
        );

        // Compute total hash by hashing each component on top of the previous one.
        Felt252::from_bytes_be(
            &poseidon_hash_many(&[
                FieldElement::from_byte_slice_be(b"COMPILED_CLASS_V1").unwrap(),
                external_funcs_hash,
                l1_handlers_hash,
                constructors_hash,
                bytecode_hash,
            ])
            .to_bytes_be(),
        )
    }
    /// Returns the hash for a set of entry points.
    fn entry_points_hash(&self, entry_points: &[CasmContractEntryPoint]) -> FieldElement {
        let mut entry_point_hash_elements = vec![];
        for entry_point in entry_points {
            entry_point_hash_elements.push(
                FieldElement::from_byte_slice_be(&entry_point.selector.to_bytes_be()).unwrap(),
            );
            entry_point_hash_elements.push(FieldElement::from(entry_point.offset));
            entry_point_hash_elements.push(poseidon_hash_many(
                &entry_point
                    .builtins
                    .iter()
                    .map(|builtin| FieldElement::from_byte_slice_be(builtin.as_bytes()).unwrap())
                    .collect_vec(),
            ));
        }
        poseidon_hash_many(&entry_point_hash_elements)
    }
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct CasmContractEntryPoint {
    /// A field element that encodes the signature of the called function.
    #[cfg_attr(
        feature = "serde",
        serde(
            serialize_with = "cairo_lang_utils::bigint::serialize_big_uint",
            deserialize_with = "cairo_lang_utils::bigint::deserialize_big_uint"
        )
    )]
    pub selector: BigUint,
    /// The offset of the instruction that should be called within the contract bytecode.
    pub offset: usize,
    // list of builtins.
    pub builtins: Vec<String>,
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct CasmContractEntryPoints {
    #[cfg_attr(feature = "serde", serde(rename = "EXTERNAL"))]
    pub external: Vec<CasmContractEntryPoint>,
    #[cfg_attr(feature = "serde", serde(rename = "L1_HANDLER"))]
    pub l1_handler: Vec<CasmContractEntryPoint>,
    #[cfg_attr(feature = "serde", serde(rename = "CONSTRUCTOR"))]
    pub constructor: Vec<CasmContractEntryPoint>,
}
