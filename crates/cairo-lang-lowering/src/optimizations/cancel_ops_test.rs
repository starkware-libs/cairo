use cairo_lang_debug::DebugWithDb;
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
    test_cancel_ops,
    "src/optimizations/test_data",
    {
        cancel_ops: "cancel_ops",
    },
    test_cancel_ops
);

fn test_cancel_ops(
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

    if !semantic_diagnostics.is_empty() {
        return TestRunnerResult::success(OrderedHashMap::from([(
            "semantic_diagnostics".into(),
            semantic_diagnostics,
        )]));
    }

    let mut before = db.lowered_body(function_id, LoweringStage::PreOptimizations).unwrap().clone();
    let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();
    OptimizationPhase::ApplyInlining { enable_const_folding: true }
        .apply(db, function_id, &mut before)
        .unwrap();
    OptimizationPhase::ReorganizeBlocks.apply(db, function_id, &mut before).unwrap();
    OptimizationPhase::ReorderStatements.apply(db, function_id, &mut before).unwrap();

    let mut after = before.clone();
    OptimizationPhase::CancelOps.apply(db, function_id, &mut after).unwrap();

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
