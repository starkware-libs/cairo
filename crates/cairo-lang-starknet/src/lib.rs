//! Starknet capabilities and utilities on top of Cairo.
//!
//! Starknet is a smart contract platform that enables developers to build and deploy smart
//! contracts on a permissionless Layer 2 network, secured by Ethereum using validity proofs.
//!
//! Learn more at [starkware.io](http://starknet.io/).

#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(feature = "std")]
use cairo_lang_semantic::plugin::PluginSuite;

pub mod abi;
pub mod casm_contract_class;
pub mod compiler_version;
pub mod contract_class;

#[cfg(feature = "std")]
mod aliased;
#[cfg(feature = "std")]
pub mod allowed_libfuncs;
#[cfg(feature = "std")]
mod analyzer;
#[cfg(feature = "std")]
pub mod contract;
#[cfg(feature = "std")]
mod felt252_serde;
#[cfg(feature = "std")]
mod felt252_vec_compression;
#[cfg(feature = "std")]
pub mod inline_macros;
#[cfg(feature = "std")]
pub mod plugin;

/// Get the suite of plugins for compilation with StarkNet.
#[cfg(feature = "std")]
pub fn starknet_plugin_suite() -> PluginSuite {
    let mut suite = PluginSuite::default();
    suite
        .add_plugin::<plugin::StarkNetPlugin>()
        .add_inline_macro_plugin::<inline_macros::selector::SelectorMacro>()
        .add_inline_macro_plugin::<inline_macros::get_dep_component::GetDepComponentMacro>()
        .add_inline_macro_plugin::<inline_macros::get_dep_component::GetDepComponentMutMacro>()
        .add_analyzer_plugin::<analyzer::ABIAnalyzer>();
    suite
}

#[cfg(all(test, feature = "std"))]
mod test_utils;
