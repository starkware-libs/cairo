use cairo_lang_diagnostics::get_location_marks;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::get_direct_or_file_content;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::db::SierraGenGroup;
use crate::replace_ids::replace_sierra_ids;
use crate::statements_locations::containing_function_identifier_for_tests;
use crate::test_utils::SierraGenDatabaseForTesting;

/// Compiles a single function to Sierra and checks the generated code, together with the
/// StableLocation of each statement.
pub fn test_sierra_locations(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &SierraGenDatabaseForTesting::without_add_withdraw_gas();
    let (_path, module_code) = get_direct_or_file_content(&inputs["module_code"]);
    // Parse code and create semantic model.
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        &module_code,
    )
    .split();

    let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id);

    // Compile the function.
    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);
    let function = db.function_with_body_sierra(function_id);
    let mut sierra_code: String = "".into();
    if semantic_diagnostics.is_empty() && lowering_diagnostics.is_ok() {
        for stmt in function.unwrap().body.iter() {
            sierra_code
                .push_str(&format!("{}\n", replace_sierra_ids(db, stmt).statement.to_string(db),));
            // TODO(Gil): Improve the location string.
            let location_str = if let Some(location) = stmt.location {
                format!(
                    "Originating location:\n{}\nIn function: {}",
                    get_location_marks(db, &location.diagnostic_location(db)),
                    containing_function_identifier_for_tests(db, location)
                )
            } else {
                "".to_string()
            };
            sierra_code.push_str(format!("{}\n", location_str).as_str());
        }
    }

    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        (
            "lowering_diagnostics".into(),
            lowering_diagnostics.map_or("".into(), |diagnostics| diagnostics.format(db)),
        ),
        ("sierra_code".into(), sierra_code),
    ]))
}

cairo_lang_test_utils::test_file_test!(
    sierra_location_test,
    "src/statement_location_test_data",
    {
        simple: "simple",
    },
    test_sierra_locations
);
