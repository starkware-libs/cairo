use std::ops::Deref;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::dedup_blocks;
use crate::LoweringStage;
use crate::db::LoweringGroup;
use crate::fmt::LoweredFormatter;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::optimizations::strategy::OptimizationPhase;
use crate::test_utils::LoweringDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    dedup_blocks,
    "src/optimizations/test_data",
    {
        dedup_blocks: "dedup_blocks",
    },
    test_dedup_blocks
);

fn test_dedup_blocks(
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

    let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();
    let mut before =
        db.lowered_body(function_id, LoweringStage::PreOptimizations).unwrap().deref().clone();
    OptimizationPhase::ApplyInlining.apply(db, function_id, &mut before).unwrap();
    OptimizationPhase::ReorganizeBlocks.apply(db, function_id, &mut before).unwrap();
    OptimizationPhase::ReorderStatements.apply(db, function_id, &mut before).unwrap();
    OptimizationPhase::OptimizeMatches.apply(db, function_id, &mut before).unwrap();
    OptimizationPhase::ReorganizeBlocks.apply(db, function_id, &mut before).unwrap();
    OptimizationPhase::ReorderStatements.apply(db, function_id, &mut before).unwrap();

    let mut after = before.clone();
    dedup_blocks(&mut after);

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
