use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::LoweringStage;
use crate::optimizations::strategy::OptimizationPhase;
use crate::test_runner::run_lowering_phases_test;

cairo_lang_test_utils::test_file_test!(
    branch_inversion,
    "src/optimizations/test_data",
    {
        branch_inversion: "branch_inversion",
    },
    test_branch_inversion
);

fn test_branch_inversion(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    run_lowering_phases_test(
        inputs,
        LoweringStage::Monomorphized,
        &[
            OptimizationPhase::ApplyInlining { enable_const_folding: true },
            OptimizationPhase::ReorganizeBlocks,
        ],
        &[OptimizationPhase::BranchInversion],
    )
}
