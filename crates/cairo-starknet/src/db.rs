use std::sync::Arc;

use cairo_compiler::db::RootDatabase;
use cairo_lowering::db::LoweringGroup;
use cairo_plugins::get_default_plugins;
use cairo_semantic::corelib::get_core_ty_by_name;
use cairo_semantic::db::SemanticGroup;
use itertools::Itertools;

use crate::plugin::StarkNetPlugin;

pub fn get_database() -> RootDatabase {
    let mut db_val = RootDatabase::default();
    let db = &mut db_val;

    // Override implicit precedence for compatibility with the StarkNet OS.
    db.set_implicit_precedence(Arc::new(
        ["Pedersen", "RangeCheck", "Bitwise", "GasBuiltin", "System"]
            .iter()
            .map(|name| get_core_ty_by_name(db, name.into(), vec![]))
            .collect_vec(),
    ));

    let mut plugins = get_default_plugins();
    plugins.push(Arc::new(StarkNetPlugin {}));
    db.set_semantic_plugins(plugins);
    db_val
}
