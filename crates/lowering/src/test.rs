use debug::DebugWithDb;
use semantic::test_utils::setup_test_function;
use utils::ordered_hash_map::OrderedHashMap;

use crate::fmt::LoweredFormatter;
use crate::lower::lower;
use crate::test_utils::LoweringDatabaseForTesting;

test_utils::test_file_test!(
    lowering_test,
    [
        "src/test_data/assignment",
        "src/test_data/call",
        "src/test_data/enums",
        "src/test_data/error_propagate",
        "src/test_data/extern",
        "src/test_data/arm_pattern_destructure",
        "src/test_data/if",
        "src/test_data/match",
        "src/test_data/panic",
        "src/test_data/struct",
        "src/test_data/tests",
        "src/test_data/tuple",
    ],
    LoweringDatabaseForTesting,
    test_function_lowering
);

fn test_function_lowering(
    db: &mut LoweringDatabaseForTesting,
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
