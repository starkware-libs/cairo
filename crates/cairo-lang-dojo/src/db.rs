use std::sync::Arc;

use cairo_lang_compiler::db::{RootDatabase, RootDatabaseBuilder};
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_plugins::get_default_plugins;
use cairo_lang_semantic::corelib::get_core_ty_by_name;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_starknet::plugin::StarkNetPlugin;
use itertools::Itertools;

use crate::plugin::DojoPlugin;

pub trait DojoRootDatabaseBuilderEx {
    /// Tunes a compiler database to Dojo (e.g. Dojo plugin).
    fn with_dojo(&mut self) -> &mut Self;

    /// Tunes a compiler database to Dojo and Starknet
    fn with_dojo_and_starknet(&mut self) -> &mut Self;
}

impl DojoRootDatabaseBuilderEx for RootDatabaseBuilder {
    fn with_dojo(&mut self) -> &mut Self {

        let mut plugins = get_default_plugins();
        plugins.push(Arc::new(DojoPlugin {}));

        self.with_plugins(plugins)
    }

    fn with_dojo_and_starknet(&mut self) -> &mut Self {

        let precedence = vec!["Pedersen", "RangeCheck", "Bitwise", "EcOp", "GasBuiltin", "System"];

        let mut plugins = get_default_plugins();
        plugins.push(Arc::new(DojoPlugin {}));
        plugins.push(Arc::new(StarkNetPlugin {}));

        self.with_implicit_precedence(precedence).with_plugins(plugins)
    }
}
/// Returns a compiler database tuned to Dojo (e.g. Dojo plugin).
pub fn get_dojo_database() -> RootDatabase {
    let mut db_val = RootDatabase::default();
    let db = &mut db_val;

    // Override implicit precedence for compatibility with the Starknet OS.
    db.set_implicit_precedence(Arc::new(
        ["Pedersen", "RangeCheck", "Bitwise", "GasBuiltin", "System"]
            .iter()
            .map(|name| get_core_ty_by_name(db, name.into(), vec![]))
            .collect_vec(),
    ));

    let mut plugins = get_default_plugins();
    plugins.push(Arc::new(DojoPlugin {}));
    db.set_semantic_plugins(plugins);
    db_val
}

pub trait StarknetRootDatabaseBuilderEx {
    /// Tunes a compiler database to StarkNet (e.g. StarkNet plugin).
    fn with_starknet(&mut self) -> &mut Self;
}

impl StarknetRootDatabaseBuilderEx for RootDatabaseBuilder {
    fn with_starknet(&mut self) -> &mut Self {
        // Override implicit precedence for compatibility with the StarkNet OS.
        let precedence = vec!["Pedersen", "RangeCheck", "Bitwise", "EcOp", "GasBuiltin", "System"];

        let mut plugins = get_default_plugins();
        plugins.push(Arc::new(StarkNetPlugin {}));

        self.with_implicit_precedence(precedence).with_plugins(plugins)
    }
}