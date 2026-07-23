use std::fmt::Write;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::TopLevelLanguageElementId;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::chain;

use crate::LoweringStage;
use crate::db::LoweringGroup;
use crate::fmt::LoweredFormatter;
use crate::ids::{
    ConcreteFunctionWithBodyId, ConcreteFunctionWithBodyLongId, GeneratedFunction,
    GeneratedFunctionKey,
};
use crate::optimizations::strategy::{ApplyOptimization, OptimizationPhase};
use crate::test_utils::LoweringDatabaseForTesting;

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
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, semantic_diagnostics) = setup_test_function(db, inputs).split();
    let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();

    let main_id = ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);
    let multi_lowering =
        db.priv_function_with_body_multi_lowering(test_function.function_id).unwrap();
    let function_ids = chain!(
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
    );

    let mut before = String::new();
    let mut after = String::new();
    for (description, function_id) in function_ids {
        let mut lowered =
            db.lowered_body(function_id, LoweringStage::PreOptimizations).unwrap().clone();
        writeln!(
            before,
            "{description}\n{:?}",
            lowered.debug(&LoweredFormatter::new(db, &lowered.variables))
        )
        .unwrap();
        [OptimizationPhase::TrimUnusedParams, OptimizationPhase::Validate]
            .apply(db, function_id, &mut lowered)
            .unwrap();
        writeln!(
            after,
            "{description}\n{:?}",
            lowered.debug(&LoweredFormatter::new(db, &lowered.variables))
        )
        .unwrap();
    }

    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("before".into(), before),
        ("after".into(), after),
        ("lowering_diagnostics".into(), lowering_diagnostics.format(db)),
    ]))
}
