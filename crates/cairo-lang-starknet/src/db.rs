use std::sync::Arc;

use cairo_lang_compiler::db::RootDatabaseBuilder;
use cairo_lang_plugins::get_default_plugins;

use crate::plugin::StarkNetPlugin;

pub fn get_starknet_precedence() -> Vec<String> {
    // Override implicit precedence for compatibility with the Starknet OS.
    let precedence = [
        "Pedersen",
        "RangeCheck",
        "Bitwise",
        "EcOp",
        "Poseidon",
        "SegmentArena",
        "GasBuiltin",
        "System",
    ];
    precedence.iter().map(ToString::to_string).collect()
}

pub trait StarknetRootDatabaseBuilderEx {
    /// Include Starknet Plugins.
    fn with_starknet_plugins(&mut self) -> &mut Self;
    /// Tunes a compiler database to Starknet (e.g. Starknet plugin).
    fn with_starknet(&mut self) -> &mut Self;
}

impl StarknetRootDatabaseBuilderEx for RootDatabaseBuilder {
    fn with_starknet_plugins(&mut self) -> &mut Self {
        let mut plugins = get_default_plugins();
        plugins.push(Arc::new(StarkNetPlugin::default()));

        self.with_plugins(plugins)
    }

    fn with_starknet(&mut self) -> &mut Self {
        let precedence = get_starknet_precedence();
        self.with_implicit_precedence(&precedence).with_starknet_plugins()
    }
}
