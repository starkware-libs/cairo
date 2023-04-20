use std::sync::Arc;

use cairo_lang_compiler::db::RootDatabaseBuilder;
use cairo_lang_plugins::get_default_plugins;

use crate::plugin::StarkNetPlugin;

pub trait StarknetRootDatabaseBuilderEx {
    // TODO(mkaput): This does not bring any added value now, remove it.
    /// Tunes a compiler database to Starknet (e.g. Starknet plugin).
    fn with_starknet(&mut self) -> &mut Self;
}

impl StarknetRootDatabaseBuilderEx for RootDatabaseBuilder {
    fn with_starknet(&mut self) -> &mut Self {
        let mut plugins = get_default_plugins();
        plugins.push(Arc::new(StarkNetPlugin::default()));

        self.with_plugins(plugins)
    }
}
