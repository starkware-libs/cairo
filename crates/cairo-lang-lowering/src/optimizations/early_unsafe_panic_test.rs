use cairo_lang_filesystem::flag::{Flag, FlagsGroup};
use cairo_lang_filesystem::ids::FlagLongId;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::LoweringStage;
use crate::optimizations::strategy::OptimizationPhase;
use crate::test_runner::run_lowering_phases_test_with_db;
use crate::test_utils::LoweringDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    early_unsafe_panic,
    "src/optimizations/test_data",
    {
        early_unsafe_panic: "early_unsafe_panic"
    },
    test_early_unsafe_panic
);

fn test_early_unsafe_panic(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    // The flag has to be set before the test function is lowered, so this test cannot run on the
    // shared default database.
    let mut db = LoweringDatabaseForTesting::new();
    db.set_flag(FlagLongId(Flag::UNSAFE_PANIC.into()), Some(Flag::UnsafePanic(true)));

    run_lowering_phases_test_with_db(
        db,
        inputs,
        // `EarlyUnsafePanic` runs in the final optimization strategy, i.e. on `PostBaseline`
        // lowering (after inlining). Use that stage so side-effecting externs like `debug::print`
        // are visible.
        LoweringStage::PostBaseline,
        &[],
        &[OptimizationPhase::EarlyUnsafePanic],
    )
}
