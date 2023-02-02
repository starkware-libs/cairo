use std::sync::Arc;

use cairo_lang_compiler::db::RootDatabaseBuilder;
use cairo_lang_plugins::get_default_plugins;

use crate::plugin::StarkNetPlugin;

pub trait StarknetRootDatabaseBuilderEx {
    /// Tunes a compiler database to StarkNet (e.g. StarkNet plugin).
    fn with_starknet(&mut self) -> &mut Self;
}

impl StarknetRootDatabaseBuilderEx for RootDatabaseBuilder {
    fn with_starknet(&mut self) -> &mut Self {
        // Override implicit precedence for compatibility with the StarkNet OS.
        let precedence = ["Pedersen", "RangeCheck", "Bitwise", "EcOp", "GasBuiltin", "System"];

        let mut plugins = get_default_plugins();
        plugins.push(Arc::new(StarkNetPlugin {}));

        self.with_implicit_precedence(&precedence).with_plugins(plugins)
    }
}
