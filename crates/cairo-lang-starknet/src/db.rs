use std::sync::Arc;

use cairo_lang_compiler::db::RootDatabaseBuilder;
use cairo_lang_plugins::config::ConfigPlugin;
use cairo_lang_plugins::derive::DerivePlugin;
use cairo_lang_plugins::panicable::PanicablePlugin;
use cairo_lang_semantic::plugin::SemanticPlugin;

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

        self.with_implicit_precedence(&precedence).with_plugin(Arc::new(StarkNetPlugin {}))
    }
}
