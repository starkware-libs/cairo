use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::LoweringStage;
use crate::db::LoweringGroup;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::optimizations::scrub_units::scrub_units;
use crate::test_utils::{LoweringDatabaseForTesting, formatted_lowered};

cairo_lang_test_utils::test_file_test!(
    scrub_units,
    "src/optimizations/test_data",
    {
        scrub_units: "scrub_units"
    },
    test_scrub_units
);

/// Not using `test_runner::run_lowering_phases_test`, as `scrub_units` is a hardcoded step of
/// `lowered_body` rather than an `OptimizationPhase`, so there is no phase to hand the runner.
fn test_scrub_units(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, semantic_diagnostics) = setup_test_function(db, inputs).split();

    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);

    let before = db.lowered_body(function_id, LoweringStage::Monomorphized).unwrap();

    let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();
    let mut after = before.clone();
    scrub_units(db, &mut after);

    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("before".into(), formatted_lowered(db, Some(before))),
        ("after".into(), formatted_lowered(db, Some(&after))),
        ("lowering_diagnostics".into(), lowering_diagnostics.format(db)),
    ]))
}
