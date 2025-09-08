use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::flag::Flag;
use cairo_lang_filesystem::ids::FlagLongId;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::LoweringStage;
use crate::db::LoweringGroup;
use crate::fmt::LoweredFormatter;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::optimizations::strategy::OptimizationPhase;
use crate::test_utils::LoweringDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    scrub_units,
    "src/optimizations/test_data",
    {
        early_unsafe_panic: "early_unsafe_panic"
    },
    test_early_unsafe_panic
);

fn test_early_unsafe_panic(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::new();
    let unsafe_panic_flag_id = FlagLongId("unsafe_panic".into());
    db.set_flag(unsafe_panic_flag_id, Some(Arc::new(Flag::UnsafePanic(true))));
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
    )
    .split();

    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);

    let before = db.lowered_body(function_id, LoweringStage::PreOptimizations).unwrap().clone();

    let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();
    let mut after = before.clone();
    OptimizationPhase::EarlyUnsafePanic.apply(db, function_id, &mut after).unwrap();

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
