use std::sync::Arc;

use cairo_lang_compiler::db::RootDatabaseBuilder;
use cairo_lang_plugins::get_default_plugins;

use crate::plugin::StarkNetPlugin;

pub trait StarknetRootDatabaseBuilderEx {
    /// Tunes a compiler database to Starknet (e.g. Starknet plugin).
    fn with_starknet(&mut self) -> &mut Self;
}

impl StarknetRootDatabaseBuilderEx for RootDatabaseBuilder {
    fn with_starknet(&mut self) -> &mut Self {
        // Override implicit precedence for compatibility with the Starknet OS.
        let precedence = [
            "Pedersen",
            "RangeCheck",
            "Bitwise",
            "EcOp",
            // TODO(lior): Uncomment the line below once Poseidon is supported.
            //   "Poseidon",
            "SegmentArena",
            "GasBuiltin",
            "System",
        ];

        let mut plugins = get_default_plugins();
        plugins.push(Arc::new(StarkNetPlugin));

        self.with_implicit_precedence(&precedence).with_plugins(plugins)
    }
}
