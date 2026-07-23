use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::LoweringStage;
use crate::db::init_lowering_group;
use crate::ids::ConcreteFunctionWithBodyLongId;
use crate::optimizations::config::{OptimizationConfig, Optimizations};
use crate::optimizations::strategy::OptimizationPhase;
use crate::test_runner::run_lowering_phases_test_with_db;
use crate::test_utils::LoweringDatabaseForTesting;
use crate::utils::InliningStrategy;

cairo_lang_test_utils::test_file_test!(
    trim_unused_params,
    "src/optimizations/test_data",
    {
        trim_unused_params: "trim_unused_params",
    },
    test_trim_unused_params
);

fn test_trim_unused_params(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    // The tests observe the trimming through the calls to specialized functions in the test
    // function's body, so inlining is disabled to keep the callees as calls, and the size
    // estimation is stubbed so that specializing a call on a const argument is always considered
    // worthwhile.
    // Note that setting these inputs requires a fresh database, as the shared database of
    // `LoweringDatabaseForTesting::default` cannot be mutated.
    let mut db = LoweringDatabaseForTesting::new();
    init_lowering_group(
        &mut db,
        Optimizations::Enabled(OptimizationConfig {
            moveable_functions: vec![],
            inlining_strategy: InliningStrategy::InlineSmallFunctions(0),
            skip_const_folding: false,
        }),
        Some(|db, function_id| {
            Ok(match function_id.long(db) {
                ConcreteFunctionWithBodyLongId::Specialized(_) => 1,
                _ => 100,
            })
        }),
    );
    run_lowering_phases_test_with_db(
        db,
        inputs,
        LoweringStage::PostBaseline,
        &[],
        &[OptimizationPhase::TrimUnusedParams, OptimizationPhase::Validate],
    )
}
