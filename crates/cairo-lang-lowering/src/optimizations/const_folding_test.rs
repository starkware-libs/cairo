use cairo_lang_debug::DebugWithDb;
use cairo_lang_semantic::db::SemanticGroup;
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
    const_folding,
    "src/optimizations/test_data",
    {
        const_folding: "const_folding",
    },
    test_match_optimizer
);

fn test_match_optimizer(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        &inputs["function"],
        &inputs["function_name"],
        &inputs["module_code"],
    )
    .split();
    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);
    let mut before = db
        .lowered_body(function_id, LoweringStage::PreOptimizations)
        .unwrap_or_else(|_| {
            let semantic_diags = db
                .module_semantic_diagnostics(test_function.module_id)
                .unwrap_or_default()
                .format(db);
            let lowering_diags = db
                .module_lowering_diagnostics(test_function.module_id)
                .unwrap_or_default()
                .format(db);

            panic!(
                "Failed to get lowered body for function {function_id:?}.\nSemantic diagnostics: \
                 {semantic_diags}\nLowering diagnostics: {lowering_diags}",
            )
        })
        .clone();
    OptimizationPhase::ApplyInlining { enable_const_folding: false }
        .apply(db, function_id, &mut before)
        .unwrap();
    OptimizationPhase::ReorganizeBlocks.apply(db, function_id, &mut before).unwrap();
    OptimizationPhase::CancelOps.apply(db, function_id, &mut before).unwrap();
    OptimizationPhase::ReorganizeBlocks.apply(db, function_id, &mut before).unwrap();
    let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();

    let mut after = before.clone();
    OptimizationPhase::ConstFolding.apply(db, function_id, &mut after).unwrap();

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
