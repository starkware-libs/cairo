use std::ops::Deref;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::concretize::concretize_lowered;
use crate::db::LoweringGroup;
use crate::fmt::LoweredFormatter;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::optimizations::scrub_units::scrub_units;
use crate::test_utils::LoweringDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    scrub_units,
    "src/optimizations/test_data",
    {
        scrub_units: "scrub_units"
    },
    test_scrub_units
);

fn test_scrub_units(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
    )
    .split();

    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);

    let mut before = db
        .function_with_body_lowering(function_id.function_with_body_id(db))
        .unwrap()
        .deref()
        .clone();
    concretize_lowered(db, &mut before, &function_id.substitution(db).unwrap()).unwrap();

    let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();
    let mut after = before.clone();
    scrub_units(db, &mut after);

    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        (
            "before".into(),
            format!("{:?}", before.debug(&LoweredFormatter::new(db, &before.variables))),
        ),
        (
            "after".into(),
            format!("{:?}", after.debug(&LoweredFormatter::new(db, &after.variables))),
        ),
        ("lowering_diagnostics".into(), lowering_diagnostics.format(db)),
    ]))
}
