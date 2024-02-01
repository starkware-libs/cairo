//! Starknet capabilities and utilities on top of Cairo.
//!
//! Starknet is a smart contract platform that enables developers to build and deploy smart
//! contracts on a permissionless Layer 2 network, secured by Ethereum using validity proofs.
//!
//! Learn more at [starkware.io](http://starknet.io/).

use cairo_lang_semantic::plugin::PluginSuite;

pub mod abi_builder;
pub mod compile_contract_class;
pub mod contract;
pub mod inline_macros;
pub mod plugin;

mod aliased;
mod analyzer;

// Rexports for more practicality downstream
pub use cairo_lang_starknet_classes::*;

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

// Those modules test logic defined in `cairo-lang-starknet-types`.
// Because those tests require `cairo_lang_starknet::test_utils`, which requires
// `cairo_lang_starknet::test_utils`, it can only be tested here, and not in
// `cairo-lang-starknet-types`.
#[cfg(all(test, feature = "serde"))]
mod casm_contract_class_from_contract_class_test;
#[cfg(all(test, feature = "serde"))]
mod contract_class_serde_test;
#[cfg(test)]
mod felt252_serde_test;

#[cfg(test)]
mod test_utils;
