use std::sync::Arc;

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_plugins::get_default_plugins;

use crate::plugin::StarkNetPlugin;

/// Returns a compiler database tuned to Starknet (e.g. Starknet plugin).
pub fn get_starknet_database() -> RootDatabase {
    // Override implicit precedence for compatibility with the StarkNet OS.
    let precedence = vec!["Pedersen", "RangeCheck", "Bitwise", "EcOp", "GasBuiltin", "System"];

    let mut plugins = get_default_plugins();
    plugins.push(Arc::new(StarkNetPlugin {}));

    let mut builder = RootDatabase::builder();
    builder.with_dev_corelib().unwrap().with_implicit_precedence(precedence).with_plugins(plugins);
    builder.build()
}
