use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::LoweringStage;
use crate::optimizations::strategy::OptimizationPhase;
use crate::test_runner::run_lowering_phases_test;

cairo_lang_test_utils::test_file_test!(
    reorder_statements,
    "src/optimizations/test_data",
    {
        reorder_statements: "reorder_statements",
    },
    test_reorder_statements
);

fn test_reorder_statements(
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
        &[OptimizationPhase::ReorderStatements],
    )
}
