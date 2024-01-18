//! Starknet capabilities and utilities on top of Cairo.
//!
//! Starknet is a smart contract platform that enables developers to build and deploy smart
//! contracts on a permissionless Layer 2 network, secured by Ethereum using validity proofs.
//!
//! Learn more at [starkware.io](http://starknet.io/).

use cairo_lang_semantic::plugin::PluginSuite;

pub mod abi_builder;
pub mod compile_contract_class;

mod aliased;
pub mod allowed_libfuncs;
mod analyzer;
pub mod casm_contract_class_from_contract_class;
pub mod contract;
mod felt252_serde;
mod felt252_vec_compression;
pub mod inline_macros;
pub mod plugin;

// Rexports for more practicality downstream
pub use cairo_lang_starknet_types::{abi, casm_contract_class, compiler_version, contract_class};

/// Get the suite of plugins for compilation with StarkNet.
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

#[cfg(all(test, feature = "serde"))]
#[path = "contract_class_serde_tests.rs"]
mod contract_class_serde_tests;
#[cfg(test)]
mod test_utils;
