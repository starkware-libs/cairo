use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::LoweringStage;
use crate::optimizations::reboxing::find_reboxing_candidates;
use crate::optimizations::strategy::OptimizationPhase;
use crate::test_runner::run_lowering_phases_test_with_extra_outputs;

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
    run_lowering_phases_test_with_extra_outputs(
        inputs,
        LoweringStage::PreOptimizations,
        &[
            OptimizationPhase::ApplyInlining { enable_const_folding: true },
            OptimizationPhase::ReorganizeBlocks,
            OptimizationPhase::ReorderStatements,
        ],
        &[OptimizationPhase::Reboxing],
        // The candidates are the ones `OptimizationPhase::Reboxing` acted on, so they have to be
        // recomputed from the body as it was *before* the phase ran.
        |before, _after| {
            let candidates = find_reboxing_candidates(before)
                .iter()
                .map(|v| format!("v{}", v.reboxed_var.index()))
                .collect::<Vec<_>>()
                .join(", ");
            vec![("candidates".into(), candidates)]
        },
    )
}
