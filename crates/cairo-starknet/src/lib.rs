//! Starknet capabilities and utilities on top of Cairo.
//!
//! StarkNet is a smart contract platform that enables developers to build and deploy smart
//! contracts on a permissionless Layer 2 network, secured by Ethereum using validity proofs.
//!
//! Learn more at [starkware.io](http://starknet.io/).
pub mod abi;
pub mod casm_contract_class;
pub mod contract;
pub mod contract_class;
pub mod db;
#[allow(dead_code)]
mod felt_serde;
pub mod plugin;

#[cfg(test)]
mod test_utils;
