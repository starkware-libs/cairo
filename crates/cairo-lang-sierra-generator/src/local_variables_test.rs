use cairo_lang_debug::DebugWithDb;
use cairo_lang_lowering as lowering;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;

use super::find_local_variables;
use crate::function_generator_test_utils::test_function_generator;
use crate::test_utils::SierraGenDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    find_local_variables,
    "src/local_variables_test_data",
    {
        block: "block",
        construct_enum: "construct_enum",
        match_enum: "match_enum",
        match_extern: "match_extern",
        simple: "simple",
        struct_: "struct",
    },
    check_find_local_variables
);

fn check_find_local_variables(
    inputs: &OrderedHashMap<String, String>,
) -> OrderedHashMap<String, String> {
    let db = &mut SierraGenDatabaseForTesting::default();
    // Parse code and create semantic model.
    let test_function = setup_test_function(
        db,
        inputs["function_code"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
    )
    .unwrap();

    db.module_lowering_diagnostics(test_function.module_id)
        .unwrap()
        .expect_with_db(db, "Unexpected diagnostics.");

    let lowered_function =
        &*db.concrete_function_with_body_lowered(test_function.concrete_function_id).unwrap();

    let lowered_formatter =
        lowering::fmt::LoweredFormatter { db, variables: &lowered_function.variables };
    let lowered_str = format!("{:?}", lowered_function.debug(&lowered_formatter));

    let local_variables_str = find_local_variables(db, lowered_function)
        .map_or("None".into(), |x| {
            x.iter().map(|var_id| format!("{:?}", var_id.debug(&lowered_formatter))).join(", ")
        });

    OrderedHashMap::from([
        ("lowering_format".into(), lowered_str),
        ("local_variables".into(), local_variables_str),
    ])
}

cairo_lang_test_utils::test_file_test!(
    e2e,
    "src/local_variables_test_data",
    {e2e: "e2e"},
    test_function_generator
);
