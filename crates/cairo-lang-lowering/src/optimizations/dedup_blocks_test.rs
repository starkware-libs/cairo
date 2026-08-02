use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::LoweringStage;
use crate::optimizations::strategy::OptimizationPhase;
use crate::test_runner::run_lowering_phases_test;

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
    run_lowering_phases_test(
        inputs,
        LoweringStage::PreOptimizations,
        &[
            OptimizationPhase::ApplyInlining { enable_const_folding: true },
            OptimizationPhase::ReorganizeBlocks,
            OptimizationPhase::ReorderStatements,
            OptimizationPhase::OptimizeMatches,
            OptimizationPhase::ReorganizeBlocks,
            OptimizationPhase::ReorderStatements,
        ],
        &[OptimizationPhase::DedupBlocks],
    )
}
