use cairo_lang_debug::DebugWithDb;
use cairo_lang_lowering as lowering;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;

use super::find_variable_lifetime;
use crate::local_variables::find_local_variables;
use crate::test_utils::SierraGenDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    variable_lifetime,
    "src/lifetime_test_data",
    {
        block: "block",
        early_return: "early_return",
        enum_: "enum",
        locals: "locals",
        simple: "simple",
        struct_: "struct",
        match_: "match",
    },
    check_variable_lifetime
);

fn check_variable_lifetime(
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

    let local_variables = find_local_variables(db, lowered_function).unwrap();
    let find_variable_lifetime_res = find_variable_lifetime(lowered_function, &local_variables)
        .expect("find_variable_lifetime failed unexpectedly");
    let last_use_str = find_variable_lifetime_res
        .last_use
        .iter()
        .map(|location| format!("{location:?}"))
        .join("\n");
    let drop_str = find_variable_lifetime_res
        .drops
        .iter()
        .map(|(location, vars)| {
            format!(
                "{location:?}: {}",
                vars.iter().map(|var_id| format!("{:?}", var_id)).join(", ")
            )
        })
        .join("\n");

    OrderedHashMap::from([
        ("lowering_format".into(), lowered_str),
        ("last_use".into(), last_use_str),
        ("drops".into(), drop_str),
    ])
}
