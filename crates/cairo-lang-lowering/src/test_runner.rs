//! A single shared golden-file test runner for "before / after" lowering tests.
//!
//! Almost every optimization test in this crate has the exact same shape: lower a test function up
//! to some [`LoweringStage`], apply a fixed list of [`OptimizationPhase`]s to reach the state we
//! want to show as `before`, clone it, apply the phase(s) under test to get `after`, and dump both.
//! [`run_lowering_phases_test`] is that shape, factored out once. Every per-optimization
//! `*_test.rs` file is then a thin binding: the `test_file_test!` invocation that ties a golden
//! file to a test function, plus a one-call function body supplying the three things that actually
//! differ between optimizations.
//!
//! # Configuration lives in Rust, not in the golden files
//!
//! The stage and the phase lists are passed as plain Rust arguments from the thin binding. There is
//! deliberately no `before_phases: ...` input tag and no string-to-[`OptimizationPhase`] parser:
//! * `test_runner_name(key: value)` args are split on every top-level comma with no bracket
//!   awareness (see `run_test_file` in `cairo-lang-test-utils`), so a phase *list* cannot be
//!   encoded there without inventing an escaping scheme.
//! * A parser would have to be kept in sync with [`OptimizationPhase`] by hand, and would turn a
//!   compile error ("this phase does not exist") into a runtime panic.
//! * Rust arguments keep struct-carrying variants such as [`OptimizationPhase::ApplyInlining`]
//!   expressible as-is.
//!
//! Each thin binding also keeps its existing test function name: `run_test_file` hard-asserts that
//! a golden case's `test_runner_name` tag equals the bound function's name, so keeping the names
//! means no golden tag has to be touched when a test is migrated to this runner.
//!
//! # Output schema
//!
//! The runner always emits the same four output tags, in this order:
//! `semantic_diagnostics`, `before`, `after`, `lowering_diagnostics`.
//!
//! # Failure policy
//!
//! Two distinct failure modes, handled uniformly for all callers:
//! * Non-empty semantic diagnostics: return immediately with only the `semantic_diagnostics` tag.
//!   Nothing was lowered, so `before`/`after` would be meaningless. This matches what
//!   `split_structs_test.rs`/`variable_forwarding_test.rs` did before the unification. Emitting
//!   fewer tags than the golden file contains is safe: check mode only verifies the tags the runner
//!   emitted, and fix mode only overwrites those tags, leaving the rest of the file intact.
//! * A function that fails to lower even though it is semantically valid (e.g. the
//!   `inline_diagnostics` golden file, where inlining a recursive function is an error): `before`
//!   and `after` both get [`formatted_lowered`]'s placeholder text and the real explanation shows
//!   up in `lowering_diagnostics`. Panicking here would make those cases untestable.
//!
//! Everything past those two points is a compiler-internal invariant, so it stays an `unwrap`.
//!
//! # Extension points
//!
//! Two tests need slightly more than the triple above, so the runner has exactly two knobs, each
//! exposed as a sibling entry point that adds a single parameter. There is deliberately no
//! entry point combining the two: `scripts/clippy.sh` runs with `-D unused`, so every knob has to
//! land with a caller, and no test needs both.
//!
//! * [`run_lowering_phases_test_with_extra_outputs`] takes an `extra_outputs` hook, called with
//!   (`before`, `after`) once both bodies exist, whose entries are appended to the output map. Its
//!   caller is `reboxing_test.rs`, which emits an additional `candidates` tag listing the variables
//!   reboxing picked up. Both bodies are handed to the hook even though that caller only reads
//!   `before` — reboxing's candidate list is computed from the body as it was *before* the phase
//!   ran. The hook does not receive the database: nothing needs it today, and a parameter with no
//!   consumer is exactly what `-D unused` exists to prevent. The hook is skipped entirely when the
//!   function fails to lower, since there are no bodies to derive outputs from. Appending is
//!   order-safe — fix mode clones the parsed test and inserts the runner's tags into it, so the
//!   position of tags already present in a golden file is preserved and a new tag is simply
//!   appended at the end.
//! * [`run_lowering_phases_test_with_db`] takes the [`LoweringDatabaseForTesting`] to run on, by
//!   value. Its caller is `early_unsafe_panic_test.rs`, which must set `Flag::UnsafePanic` on the
//!   database *before* the test function is set up and therefore cannot use the shared
//!   [`LoweringDatabaseForTesting::default()`] snapshot. Taking an already-configured database
//!   rather than a setup callback keeps the knob a value, not a closure, and lets a caller reuse
//!   the existing [`LoweringDatabaseForTesting::with_no_gas()`]-style constructors.
//!
//! # Non-goals
//!
//! These tests are intentionally *not* migrated to this runner:
//! * `src/test.rs`, `src/borrow_check/test.rs`, `src/analysis/test.rs`,
//!   `src/analysis/equality_analysis_test.rs`, `src/cache/test.rs`, `src/lower/generated_test.rs`,
//!   `src/lower/specialized_test.rs`, `src/lower/flow_control/graph_test.rs` and
//!   `src/lower/block_builder_test.rs` all have a single-output shape (one lowering dump, or a
//!   subsystem-specific result) rather than a `before`/`after` pair, and several take their own
//!   golden-file args. Forcing them through this runner would mean widening it until it no longer
//!   describes anything.
//! * `src/optimizations/scrub_units_test.rs` stays bespoke: `scrub_units` is a hardcoded step
//!   inside `lowered_body`'s `PreOptimizations` transition and has no [`OptimizationPhase`]
//!   variant. Adding a production enum variant solely so a test can name it would put a phase in
//!   the strategy vocabulary that no strategy may legally contain.

use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::db::LoweringGroup;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::optimizations::strategy::{ApplyOptimization, OptimizationPhase};
use crate::test_utils::{LoweringDatabaseForTesting, formatted_lowered};
use crate::{Lowered, LoweringStage};

/// Runs a `before`/`after` golden test for a list of optimization phases.
///
/// Lowers the test function described by `inputs` up to `stage`, applies `before_phases` to get the
/// `before` body, and applies `after_phases` to a clone of it to get the `after` body.
///
/// See the [module documentation](self) for the output schema, the failure policy, and why the
/// configuration is passed as Rust arguments instead of golden-file tags.
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

/// Same as [`run_lowering_phases_test`], but runs on the given database.
///
/// For tests that must configure the database (e.g. set a [`cairo_lang_filesystem::flag::Flag`])
/// before the test function is set up, and so cannot use the shared default snapshot.
pub fn run_lowering_phases_test_with_db<'db>(
    db: LoweringDatabaseForTesting,
    inputs: &OrderedHashMap<String, String>,
    stage: LoweringStage,
    before_phases: &[OptimizationPhase<'db>],
    after_phases: &[OptimizationPhase<'db>],
) -> TestRunnerResult {
    run_test(db, inputs, stage, before_phases, after_phases, |_, _| vec![])
}

/// Same as [`run_lowering_phases_test`], with additional output tags derived from the two bodies.
///
/// `extra_outputs` is called with the `before` and `after` bodies, and its entries are appended
/// after the standard four tags. It is not called at all if the function failed to lower.
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
