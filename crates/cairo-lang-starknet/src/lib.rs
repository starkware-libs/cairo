//! Starknet capabilities and utilities on top of Cairo.
//!
//! Starknet is a smart contract platform that enables developers to build and deploy smart
//! contracts on a permissionless Layer 2 network, secured by Ethereum using validity proofs.
//!
//! Learn more at [starkware.io](http://starknet.io/).
pub mod abi;
mod allowed_libfuncs;
pub mod casm_contract_class;
pub mod contract;
pub mod contract_class;
pub mod db;
mod felt_serde;
pub mod plugin;
mod sierra_version;

#[cfg(test)]
mod test_utils;
