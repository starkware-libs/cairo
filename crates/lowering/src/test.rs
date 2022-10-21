use debug::DebugWithDb;
use semantic::db::SemanticGroup;
use semantic::test_utils::{setup_test_function, SemanticDatabaseForTesting};
use utils::ordered_hash_map::OrderedHashMap;

use crate::fmt::LoweredFormatter;
use crate::lower::lower;

utils::test_file_test!(
    lowering_test,
    [
        "src/test_data/assignment",
        "src/test_data/call",
        "src/test_data/enums",
        "src/test_data/match",
        "src/test_data/tests",
    ],
    SemanticDatabaseForTesting,
    test_function_lowering
);

fn test_function_lowering(
    db: &mut (dyn SemanticGroup + 'static),
    inputs: &OrderedHashMap<String, String>,
) -> OrderedHashMap<String, String> {
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
