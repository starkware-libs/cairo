use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_filesystem::location_marks::get_location_marks;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::ids::{ConcreteFunctionWithBodyId, LocationId};
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::get_direct_or_file_content;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::db::SierraGenGroup;
use crate::replace_ids::replace_sierra_ids;
use crate::statements_locations::maybe_containing_function_identifier_for_tests;
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
    let append_stable_loc_str = |buf: &mut String, loc: StableLocation<'_>| {
        buf.push_str(&get_location_marks(db, &loc.span_in_file(db), true));
        buf.push('\n');
        if let Some(function) = maybe_containing_function_identifier_for_tests(db, loc) {
            buf.push_str(&format!("In function: {function}\n",));
        }
    };
    let append_loc_str = |buf: &mut String, loc: LocationId<'_>| {
        let loc = loc.long(db);
        buf.push_str("Originating location:\n");
        append_stable_loc_str(buf, loc.stable_location);
        for loc in &loc.inline_locations {
            buf.push_str("Inlined at:\n");
            append_stable_loc_str(buf, *loc);
        }
    };
    let mut sierra_code: String = "".into();
    let mut var_locations: String = "".into();
    if semantic_diagnostics.is_empty() && lowering_diagnostics.is_ok() {
        let func = function.unwrap();
        for stmt in &func.body {
            sierra_code
                .push_str(&format!("{}\n", replace_sierra_ids(db, stmt).statement.to_string(db),));
            if let Some(loc) = stmt.location {
                append_loc_str(&mut sierra_code, loc);
            }
        }
        for (var, loc) in &func.variable_locations {
            var_locations.push_str(&format!("{var}:\n"));
            append_loc_str(&mut var_locations, *loc);
        }
    }

    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        (
            "lowering_diagnostics".into(),
            lowering_diagnostics.map_or("".into(), |diagnostics| diagnostics.format(db)),
        ),
        ("sierra_code".into(), sierra_code),
        ("var_locations".into(), var_locations),
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
