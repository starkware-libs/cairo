use cairo_lang_debug::DebugWithDb;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use salsa::Setter;

use crate::db::{LoweringGroup, lowering_group_input};
use crate::fmt::LoweredFormatter;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::optimizations::config::{OptimizationConfig, Optimizations};
use crate::test_utils::LoweringDatabaseForTesting;
use crate::utils::InliningStrategy;
use crate::{LoweringStage, Statement};

cairo_lang_test_utils::test_file_test!(
    specialized,
    "src/lower/test_data",
    {
        specialized: "specialized",
    },
    test_specialized_function
);

/// Test specialized functions generations.
/// Currently the specialization arguments are hardcoded in the test.
fn test_specialized_function(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::new();
    lowering_group_input(db).set_optimizations(db).to(Some(Optimizations::Enabled(
        OptimizationConfig {
            moveable_functions: vec![],
            inlining_strategy: InliningStrategy::InlineSmallFunctions(0),
            skip_const_folding: false,
        },
    )));
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
    )
    .split();

    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);

    let lowered_caller = db.lowered_body(function_id, LoweringStage::Final).unwrap_or_else(|_| {
        panic!("Got diagnostics for the caller {semantic_diagnostics}.");
    });
    let Some(Statement::Call(call)) = lowered_caller
        .blocks
        .iter()
        .flat_map(|(_, b)| b.statements.iter())
        .rfind(|statement| matches!(statement, Statement::Call(_)))
    else {
        panic!("Could not find the last call in the caller function.");
    };
    let Ok(Some(specialized_id)) = call.function.body(db) else {
        panic!("Expected function body, got: {}", call.function.full_path(db));
    };
    let lowered_specialized =
        db.lowered_body(specialized_id, LoweringStage::Monomorphized).unwrap();
    let lowered_formatter = LoweredFormatter::new(db, &lowered_caller.variables);
    let lowered_caller = format!("{:?}", lowered_caller.debug(&lowered_formatter));
    let lowered_formatter = LoweredFormatter::new(db, &lowered_specialized.variables);
    let lowered_specialized = format!("{:?}", lowered_specialized.debug(&lowered_formatter));

    let lowering_diagnostics =
        db.module_lowering_diagnostics(test_function.module_id).unwrap_or_default();

    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("lowering_diagnostics".into(), lowering_diagnostics.format(db)),
        ("caller_lowering".into(), lowered_caller),
        ("specialized_lowering".into(), lowered_specialized),
    ]))
}
