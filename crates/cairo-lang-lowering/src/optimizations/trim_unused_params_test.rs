use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use salsa::Setter;

use crate::LoweringStage;
use crate::db::lowering_group_input;
use crate::optimizations::config::{OptimizationConfig, Optimizations};
use crate::optimizations::strategy::OptimizationPhase;
use crate::test_runner::run_multi_function_lowering_phases_test_with_db;
use crate::test_utils::LoweringDatabaseForTesting;
use crate::utils::InliningStrategy;

cairo_lang_test_utils::test_file_test!(
    trim_unused_params,
    "src/optimizations/test_data",
    {
        trim_unused_params: "trim_unused_params",
    },
    test_trim_unused_params,
    ["no_inlining", "print_specialized"]
);

fn test_trim_unused_params(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    // Disabling inlining keeps small callees as calls, so that trimming of specialized
    // functions can be tested without inflating them beyond the inlining threshold.
    // Note that setting the optimizations input requires a fresh database, as the shared
    // database of `LoweringDatabaseForTesting::default` cannot be mutated.
    let db = if args.get("no_inlining").is_some_and(|v| v == "true") {
        let mut db = LoweringDatabaseForTesting::new();
        lowering_group_input(&db).set_optimizations(&mut db).to(Some(Optimizations::Enabled(
            OptimizationConfig {
                moveable_functions: vec![],
                inlining_strategy: InliningStrategy::InlineSmallFunctions(0),
                skip_const_folding: false,
            },
        )));
        db
    } else {
        LoweringDatabaseForTesting::default()
    };
    run_multi_function_lowering_phases_test_with_db(
        db,
        inputs,
        LoweringStage::PostBaseline,
        &[],
        &[OptimizationPhase::TrimUnusedParams, OptimizationPhase::Validate],
        args.get("print_specialized").is_some_and(|v| v == "true"),
    )
}
