use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::LoweringStage;
use crate::optimizations::strategy::OptimizationPhase;
use crate::test_runner::run_lowering_phases_test;

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
    run_lowering_phases_test(
        inputs,
        LoweringStage::PreOptimizations,
        &[
            OptimizationPhase::ApplyInlining { enable_const_folding: false },
            OptimizationPhase::ReorganizeBlocks,
            OptimizationPhase::VariableForwarding,
            OptimizationPhase::ReorganizeBlocks,
        ],
        &[OptimizationPhase::ConstFolding],
    )
}
