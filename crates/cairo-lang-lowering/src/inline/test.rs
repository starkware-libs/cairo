use std::ops::Deref;

use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::LoweringStage;
use crate::db::LoweringGroup;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::optimizations::strategy::OptimizationPhase;
use crate::test_utils::{LoweringDatabaseForTesting, formatted_lowered};

cairo_lang_test_utils::test_file_test!(
    inlining,
    "src/inline/test_data",
    {
        inline: "inline",
        inline_diagnostics: "inline_diagnostics",
    },
    test_function_inlining
);

fn test_function_inlining(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
    )
    .split();
    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);

    let before = db.lowered_body(function_id, LoweringStage::PreOptimizations).ok();
    let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();
    let after = if let Some(before) = &before {
        let mut after = before.deref().clone();
        OptimizationPhase::ApplyInlining { enable_const_folding: false }
            .apply(db, function_id, &mut after)
            .unwrap();
        Some(after)
    } else {
        None
    };

    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("before".into(), formatted_lowered(db, before.as_deref())),
        ("after".into(), formatted_lowered(db, after.as_ref())),
        ("lowering_diagnostics".into(), lowering_diagnostics.format(db)),
    ]))
}
