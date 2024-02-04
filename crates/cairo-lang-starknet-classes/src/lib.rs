#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

pub mod abi;
#[cfg(feature = "std")]
pub mod allowed_libfuncs;
pub mod casm_contract_class;
pub mod compiler_version;
pub mod contract_class;
pub mod keccak;

#[cfg(feature = "std")]
pub mod felt252_serde;
#[cfg(feature = "std")]
mod felt252_vec_compression;
