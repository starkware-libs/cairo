//! Starknet capabilities and utilities on top of Cairo.
//!
//! Starknet is a smart contract platform that enables developers to build and deploy smart
//! contracts on a permissionless Layer 2 network, secured by Ethereum using validity proofs.
//!
//! Learn more at [starkware.io](http://starknet.io/).

use cairo_lang_semantic::plugin::PluginSuite;

pub mod abi;
mod aliased;
mod analyzer;
pub mod compile;
pub mod contract;
pub mod inline_macros;
pub mod plugin;

/// Get the suite of plugins for compilation with Starknet.
pub fn starknet_plugin_suite() -> PluginSuite {
    let mut suite = PluginSuite::default();
    suite
        .add_plugin::<plugin::StarknetPlugin>()
        .add_plugin::<plugin::storage_interfaces::StorageInterfacesPlugin>()
        .add_inline_macro_plugin::<inline_macros::selector::SelectorMacro>()
        .add_inline_macro_plugin::<inline_macros::get_dep_component::GetDepComponentMacro>()
        .add_inline_macro_plugin::<inline_macros::get_dep_component::GetDepComponentMutMacro>()
        .add_analyzer_plugin::<analyzer::ABIAnalyzer>()
        .add_analyzer_plugin::<analyzer::StorageAnalyzer>();
    suite
}

#[cfg(test)]
mod test_utils;
