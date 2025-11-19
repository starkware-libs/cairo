use cairo_lang_debug::DebugWithDb;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::logging;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::LoweringStage;
use crate::db::LoweringGroup;
use crate::fmt::LoweredFormatter;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::optimizations::reboxing::{apply_reboxing_candidates, find_reboxing_candidates};
use crate::optimizations::strategy::OptimizationPhase;
use crate::test_utils::LoweringDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    reboxing_analysis,
    "src/optimizations/test_data",
    {
        reboxing: "reboxing",
    },
    test_reboxing_analysis
);

fn test_reboxing_analysis(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    logging::init_logging(logging::level::TRACE);
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs.get("module_code").map(|s| s.as_str()).unwrap_or(""),
    )
    .split();

    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);
    if let Ok(mut before) = db.lowered_body(function_id, LoweringStage::Monomorphized).cloned() {
        OptimizationPhase::ApplyInlining { enable_const_folding: true }
            .apply(db, function_id, &mut before)
            .unwrap();
        OptimizationPhase::ReorganizeBlocks.apply(db, function_id, &mut before).unwrap();

        OptimizationPhase::ReorderStatements.apply(db, function_id, &mut before).unwrap();
        let mut after = before.clone();

        let formatter = LoweredFormatter::new(db, &after.variables);
        trace!("Lowering input to Reboxing:\n{:?}", after.debug(&formatter));

        let candidates = find_reboxing_candidates(db, &after);

        let candidates_str = candidates
            .iter()
            .map(|v| format!("v{}", v.reboxed_var.index()))
            .collect::<Vec<_>>()
            .join(", ");

        // Apply reboxing optimizations to create "after" state
        apply_reboxing_candidates(db, &mut after, &candidates);

        TestRunnerResult::success(OrderedHashMap::from([
            ("candidates".into(), candidates_str),
            (
                "before".into(),
                format!("{:?}", before.debug(&LoweredFormatter::new(db, &before.variables))),
            ),
            (
                "after".into(),
                format!("{:?}", after.debug(&LoweredFormatter::new(db, &after.variables))),
            ),
        ]))
    } else {
        TestRunnerResult::success(OrderedHashMap::from([(
            "semantic_diagnostics".into(),
            semantic_diagnostics,
        )]))
    }
}
