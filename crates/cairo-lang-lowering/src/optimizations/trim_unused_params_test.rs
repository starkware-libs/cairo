use std::fmt::Write;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::TopLevelLanguageElementId;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::chain;
use salsa::Setter;

use crate::db::{LoweringGroup, lowering_group_input};
use crate::fmt::LoweredFormatter;
use crate::ids::{
    ConcreteFunctionWithBodyId, ConcreteFunctionWithBodyLongId, GeneratedFunction,
    GeneratedFunctionKey,
};
use crate::optimizations::config::{OptimizationConfig, Optimizations};
use crate::optimizations::strategy::{ApplyOptimization, OptimizationPhase};
use crate::test_utils::LoweringDatabaseForTesting;
use crate::utils::InliningStrategy;
use crate::{LoweringStage, Statement};

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
    let db = &mut if args.get("no_inlining").is_some_and(|v| v == "true") {
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
    let (test_function, semantic_diagnostics) = setup_test_function(db, inputs).split();
    let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();

    let main_id = ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);
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
    // When requested, additionally print the specialized functions called by the above functions,
    // as they may be trimmed as well.
    if args.get("print_specialized").is_some_and(|v| v == "true") {
        let mut specialized_ids = vec![];
        for (_, function_id) in &function_ids {
            let Ok(lowered) = db.lowered_body(*function_id, LoweringStage::PostBaseline) else {
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
        let mut lowered =
            db.lowered_body(function_id, LoweringStage::PostBaseline).unwrap().clone();
        let before_str =
            format!("{:?}", lowered.debug(&LoweredFormatter::new(db, &lowered.variables)));
        writeln!(before, "{description}\n{before_str}").unwrap();
        [OptimizationPhase::TrimUnusedParams, OptimizationPhase::Validate]
            .apply(db, function_id, &mut lowered)
            .unwrap();
        let after_str =
            format!("{:?}", lowered.debug(&LoweredFormatter::new(db, &lowered.variables)));
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
