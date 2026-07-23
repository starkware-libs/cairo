//! A shared golden-file test runner for `before`/`after` lowering-phase tests.
//!
//! The stage and phase lists are passed as plain Rust arguments from each thin `*_test.rs` binding
//! — deliberately not as golden-file tags, so that an unknown phase is a compile error and
//! struct-carrying variants such as [`OptimizationPhase::ApplyInlining`] stay expressible as-is.
//!
//! Output tags: `semantic_diagnostics`, `before`, `after`, `lowering_diagnostics`. On semantic
//! diagnostics only the first is emitted; a semantically-valid function that fails to lower gets
//! placeholder bodies, with the reason in `lowering_diagnostics`.

use std::fmt::Write;

use cairo_lang_defs::ids::TopLevelLanguageElementId;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::chain;

use crate::db::LoweringGroup;
use crate::ids::{
    ConcreteFunctionWithBodyId, ConcreteFunctionWithBodyLongId, GeneratedFunction,
    GeneratedFunctionKey,
};
use crate::optimizations::strategy::{ApplyOptimization, OptimizationPhase};
use crate::test_utils::{LoweringDatabaseForTesting, formatted_lowered};
use crate::{Lowered, LoweringStage, Statement};

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

/// Same as [`run_lowering_phases_test_with_db`], but applies the phases to - and prints - the
/// test function together with the compiler-generated functions of its multi-lowering (loop
/// functions etc.), and, when `include_specialized_callees` is set, the specialized functions
/// called by any of them.
///
/// The `before`/`after` tags hold the lowerings of all these functions, each under a
/// `<description>` header; a function the phases did not change is elided in `after` to
/// `<description> (unchanged)`.
pub fn run_multi_function_lowering_phases_test_with_db<'db>(
    db: LoweringDatabaseForTesting,
    inputs: &OrderedHashMap<String, String>,
    stage: LoweringStage,
    before_phases: &[OptimizationPhase<'db>],
    after_phases: &[OptimizationPhase<'db>],
    include_specialized_callees: bool,
) -> TestRunnerResult {
    let db = &db;
    let (test_function, semantic_diagnostics) = setup_test_function(db, inputs).split();
    if !semantic_diagnostics.is_empty() {
        return TestRunnerResult::success(OrderedHashMap::from([(
            "semantic_diagnostics".into(),
            semantic_diagnostics,
        )]));
    }

    let main_id = ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);
    let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();

    let multi_lowering =
        db.priv_function_with_body_multi_lowering(test_function.function_id).unwrap();
    let mut function_ids: Vec<(String, ConcreteFunctionWithBodyId<'_>)> = chain!(
        [("Main:".to_string(), main_id)],
        multi_lowering.generated_lowerings.keys().map(|key| {
            let description = match key {
                GeneratedFunctionKey::Loop(_) => "Generated loop:".to_string(),
                GeneratedFunctionKey::TraitFunc(func, _) => {
                    format!("Generated {}:", func.full_path(db))
                }
            };
            let generated_id = ConcreteFunctionWithBodyLongId::Generated(GeneratedFunction {
                parent: test_function.concrete_function_id,
                key: *key,
            })
            .intern(db);
            (description, generated_id)
        })
    )
    .collect();
    if include_specialized_callees {
        let mut specialized_ids = vec![];
        for (_, function_id) in &function_ids {
            let Ok(lowered) = db.lowered_body(*function_id, stage) else {
                continue;
            };
            for (_, block) in lowered.blocks.iter() {
                for stmt in &block.statements {
                    if let Statement::Call(call_stmt) = stmt
                        && let Ok(Some(callee)) = call_stmt.function.body(db)
                        && matches!(callee.long(db), ConcreteFunctionWithBodyLongId::Specialized(_))
                        && !specialized_ids.iter().any(|(_, id)| *id == callee)
                    {
                        specialized_ids
                            .push((format!("Specialized {:?}:", callee.full_path(db)), callee));
                    }
                }
            }
        }
        function_ids.extend(specialized_ids);
    }

    let mut before = String::new();
    let mut after = String::new();
    for (description, function_id) in function_ids {
        let lowered = db.lowered_body(function_id, stage).ok();
        let (before_str, after_str) = match lowered {
            Some(lowered) => {
                let mut lowered = lowered.clone();
                before_phases.apply(db, function_id, &mut lowered).unwrap();
                let before_str = formatted_lowered(db, Some(&lowered));
                after_phases.apply(db, function_id, &mut lowered).unwrap();
                (before_str, formatted_lowered(db, Some(&lowered)))
            }
            None => (formatted_lowered(db, None), formatted_lowered(db, None)),
        };
        writeln!(before, "{description}\n{before_str}").unwrap();
        if after_str == before_str {
            writeln!(after, "{description} (unchanged)\n").unwrap();
        } else {
            writeln!(after, "{description}\n{after_str}").unwrap();
        }
    }

    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("before".into(), before),
        ("after".into(), after),
        ("lowering_diagnostics".into(), lowering_diagnostics.format(db)),
    ]))
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
