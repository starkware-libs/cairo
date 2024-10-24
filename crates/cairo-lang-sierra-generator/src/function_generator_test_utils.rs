use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::db::SierraGenGroup;
use crate::replace_ids::replace_sierra_ids;
use crate::test_utils::SierraGenDatabaseForTesting;

/// Compiles a single function to Sierra and checks the generated code.
pub fn test_function_generator(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    // Tests have recursions for revoking AP. Automatic addition of 'withdraw_gas` calls would add
    // unnecessary complication to them.
    let db = &SierraGenDatabaseForTesting::without_add_withdraw_gas();

    // Parse code and create semantic model.
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
    )
    .split();

    // Verify that there are no diagnostics.
    let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id);

    // Compile the function.
    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);
    let function = db.function_with_body_sierra(function_id);
    let sierra_code: String = function.map_or("None".into(), |func| {
        func.body
            .iter()
            .map(|x| replace_sierra_ids(db, x).statement.to_string(db))
            .collect::<Vec<String>>()
            .join("\n")
    });

    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        (
            "lowering_diagnostics".into(),
            lowering_diagnostics.map_or("".into(), |diagnostics| diagnostics.format(db)),
        ),
        ("sierra_code".into(), sierra_code),
    ]))
}
