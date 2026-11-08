//! A shared golden-file test runner for `before`/`after` lowering-phase tests.
//!
//! The stage and phase lists are passed as plain Rust arguments from each thin `*_test.rs` binding
//! — deliberately not as golden-file tags, so that an unknown phase is a compile error and
//! struct-carrying variants such as [`OptimizationPhase::ApplyInlining`] stay expressible as-is.
//!
//! Output tags: `semantic_diagnostics`, `before`, `after`, `lowering_diagnostics`. On semantic
//! diagnostics only the first is emitted; a semantically-valid function that fails to lower gets
//! placeholder bodies, with the reason in `lowering_diagnostics`.

use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::db::LoweringGroup;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::optimizations::strategy::{ApplyOptimization, OptimizationPhase};
use crate::test_utils::{LoweringDatabaseForTesting, formatted_lowered};
use crate::{Lowered, LoweringStage};

/// Runs a `before`/`after` golden test: lowers the test function up to `stage`, applies
/// `before_phases` to get the `before` body, and `after_phases` on a clone of it to get `after`.
pub fn run_lowering_phases_test<'db>(
    inputs: &OrderedHashMap<String, String>,
    stage: LoweringStage,
    before_phases: &[OptimizationPhase<'db>],
    after_phases: &[OptimizationPhase<'db>],
) -> TestRunnerResult {
    run_lowering_phases_test_with_db(
        LoweringDatabaseForTesting::default(),
        inputs,
        stage,
        before_phases,
        after_phases,
    )
}

/// Same as [`run_lowering_phases_test`], but runs on the given database, for tests that must
/// configure it (e.g. set a flag) before the test function is set up.
pub fn run_lowering_phases_test_with_db<'db>(
    db: LoweringDatabaseForTesting,
    inputs: &OrderedHashMap<String, String>,
    stage: LoweringStage,
    before_phases: &[OptimizationPhase<'db>],
    after_phases: &[OptimizationPhase<'db>],
) -> TestRunnerResult {
    run_test(db, inputs, stage, before_phases, after_phases, |_, _| vec![])
}

/// Same as [`run_lowering_phases_test`], with additional output tags derived from the `before` and
/// `after` bodies. `extra_outputs` is not called if the function failed to lower.
pub fn run_lowering_phases_test_with_extra_outputs<'db>(
    inputs: &OrderedHashMap<String, String>,
    stage: LoweringStage,
    before_phases: &[OptimizationPhase<'db>],
    after_phases: &[OptimizationPhase<'db>],
    extra_outputs: impl FnOnce(&Lowered<'_>, &Lowered<'_>) -> Vec<(String, String)>,
) -> TestRunnerResult {
    run_test(
        LoweringDatabaseForTesting::default(),
        inputs,
        stage,
        before_phases,
        after_phases,
        extra_outputs,
    )
}

/// The shared body of all the entry points above.
fn run_test<'db>(
    mut db: LoweringDatabaseForTesting,
    inputs: &OrderedHashMap<String, String>,
    stage: LoweringStage,
    before_phases: &[OptimizationPhase<'db>],
    after_phases: &[OptimizationPhase<'db>],
    extra_outputs: impl FnOnce(&Lowered<'_>, &Lowered<'_>) -> Vec<(String, String)>,
) -> TestRunnerResult {
    let db = &mut db;
    let (test_function, semantic_diagnostics) = setup_test_function(db, inputs).split();
    if !semantic_diagnostics.is_empty() {
        return TestRunnerResult::success(OrderedHashMap::from([(
            "semantic_diagnostics".into(),
            semantic_diagnostics,
        )]));
    }

    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);
    let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();

    let (before, after) = match db.lowered_body(function_id, stage) {
        Ok(lowered) => {
            let mut before = lowered.clone();
            before_phases.apply(db, function_id, &mut before).unwrap();
            let mut after = before.clone();
            after_phases.apply(db, function_id, &mut after).unwrap();
            (Some(before), Some(after))
        }
        Err(_) => (None, None),
    };

    let mut outputs = OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("before".into(), formatted_lowered(db, before.as_ref())),
        ("after".into(), formatted_lowered(db, after.as_ref())),
        ("lowering_diagnostics".into(), lowering_diagnostics.format(db)),
    ]);
    if let (Some(before), Some(after)) = (&before, &after) {
        outputs.extend(extra_outputs(before, after));
    }
    TestRunnerResult::success(outputs)
}
