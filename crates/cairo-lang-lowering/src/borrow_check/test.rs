use cairo_lang_debug::DebugWithDb;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::db::LoweringGroup;
use crate::fmt::LoweredFormatter;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::test_utils::LoweringDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    borrow_check,
    "src/borrow_check/test_data",
    {
        borrow_check :"borrow_check",
    },
    test_borrow_check
);

fn test_borrow_check(
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

    let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();
    let lowering = if let Ok(lowered) =
        db.priv_function_with_body_lowering(function_id.function_with_body_id(db))
    {
        format!(
            "{:?}",
            lowered.debug(&LoweredFormatter {
                db,
                variables: &lowered.variables,
                include_usage_location: true
            })
        )
    } else {
        "".into()
    };

    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("lowering".into(), lowering),
        ("lowering_diagnostics".into(), lowering_diagnostics.format(db)),
    ]))
}
