//! Starknet classes definitions.
//!
//! Starknet is a smart contract platform that enables developers to build and deploy smart
//! contracts on a permissionless Layer 2 network, secured by Ethereum using validity proofs.
//!
//! Learn more at [starkware.io](http://starknet.io/).

pub mod abi;
pub mod allowed_libfuncs;
pub mod casm_contract_class;
pub mod compiler_version;
pub mod contract_class;
mod contract_segmentation;
mod felt252_serde;
mod felt252_vec_compression;
pub mod keccak;

pub use contract_segmentation::NestedIntList;

#[cfg(test)]
mod test_utils;
