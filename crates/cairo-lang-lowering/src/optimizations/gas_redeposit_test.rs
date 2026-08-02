use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::LoweringStage;
use crate::optimizations::strategy::OptimizationPhase;
use crate::test_runner::run_lowering_phases_test;

cairo_lang_test_utils::test_file_test!(
    gas_redeposit,
    "src/optimizations/test_data",
    {
        gas_redeposit: "gas_redeposit",
    },
    test_gas_redeposit
);

fn test_gas_redeposit(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    run_lowering_phases_test(
        inputs,
        LoweringStage::PreOptimizations,
        &[],
        &[OptimizationPhase::GasRedeposit],
    )
}
