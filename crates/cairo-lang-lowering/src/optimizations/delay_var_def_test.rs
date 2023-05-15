use std::ops::Deref;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_plugins::get_default_plugins;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::delay_var_def;
use crate::db::LoweringGroup;
use crate::fmt::LoweredFormatter;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::optimizations::remappings::optimize_remappings;
use crate::reorganize_blocks::reorganize_blocks;
use crate::test_utils::LoweringDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    delay_var_def,
    "src/optimizations/test_data",
    {
        move_literals :"move_literals",
    },
    test_delay_var_def
);

fn test_delay_var_def(inputs: &OrderedHashMap<String, String>) -> OrderedHashMap<String, String> {
    let db = &mut LoweringDatabaseForTesting::default();
    db.set_semantic_plugins(get_default_plugins());
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
    )
    .split();
    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);

    let mut before =
        db.priv_concrete_function_with_body_lowered_flat(function_id).unwrap().deref().clone();

    let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();
    optimize_remappings(&mut before);
    reorganize_blocks(&mut before);

    let mut after = before.clone();
    delay_var_def(&mut after);

    OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        (
            "before".into(),
            format!("{:?}", before.debug(&LoweredFormatter { db, variables: &before.variables })),
        ),
        (
            "after".into(),
            format!("{:?}", after.debug(&LoweredFormatter { db, variables: &after.variables })),
        ),
        ("lowering_diagnostics".into(), lowering_diagnostics.format(db)),
    ])
}
