use std::ops::Deref;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_plugins::get_default_plugins;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::optimize_matches;
use crate::db::LoweringGroup;
use crate::fmt::LoweredFormatter;
use crate::inline::apply_inlining;
use crate::optimizations::remappings::optimize_remappings;
use crate::panic::lower_panics;
use crate::reorganize_blocks::reorganize_blocks;
use crate::test_utils::LoweringDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    match_optimizer,
    "src/optimizations/test_data",
    {
        option :"option",
    },
    test_match_optimizer
);

fn test_match_optimizer(inputs: &OrderedHashMap<String, String>) -> OrderedHashMap<String, String> {
    let db = &mut LoweringDatabaseForTesting::default();
    db.set_semantic_plugins(get_default_plugins());
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
    )
    .split();

    let mut before = db
        .priv_concrete_function_with_body_lowered_flat(test_function.concrete_function_id)
        .unwrap()
        .deref()
        .clone();

    let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();

    apply_inlining(db, test_function.function_id, &mut before).unwrap();
    before = lower_panics(db, test_function.concrete_function_id, &before).unwrap();
    reorganize_blocks(&mut before);
    optimize_remappings(&mut before);

    let mut after = before.clone();
    optimize_matches(&mut after);

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
