use cairo_debug::DebugWithDb;
use cairo_plugins::get_default_plugins;
use cairo_semantic::db::SemanticGroup;
use cairo_semantic::test_utils::setup_test_function;
use cairo_utils::ordered_hash_map::OrderedHashMap;

use crate::fmt::LoweredFormatter;
use crate::lower::lower;
use crate::test_utils::LoweringDatabaseForTesting;

cairo_test_utils::test_file_test!(
    lowering,
    "src/test_data",
    {
        assignment :"assignment",
        call :"call",
        enums :"enums",
        error_propagate :"error_propagate",
        extern_ :"extern",
        arm_pattern_destructure :"arm_pattern_destructure",
        if_ :"if",
        match_ :"match",
        panic :"panic",
        struct_ :"struct",
        tests :"tests",
        tuple :"tuple",
    },
    test_function_lowering
);

fn test_function_lowering(
    inputs: &OrderedHashMap<String, String>,
) -> OrderedHashMap<String, String> {
    let db = &mut LoweringDatabaseForTesting::default();
    db.set_semantic_plugins(get_default_plugins());
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
    )
    .split();
    let lowered = lower(db, test_function.function_id).unwrap();

    let lowered_formatter = LoweredFormatter { db, lowered: &lowered };
    OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("lowering_diagnostics".into(), lowered.diagnostics.format(db)),
        ("lowering_format".into(), format!("{:?}", lowered.debug(&lowered_formatter))),
    ])
}
