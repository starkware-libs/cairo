use debug::DebugWithDb;
use itertools::Itertools;
use lowering::db::LoweringGroup;
use pretty_assertions::assert_eq;
use semantic::test_utils::setup_test_function;
use utils::ordered_hash_map::OrderedHashMap;

use super::find_local_variables;
use crate::function_generator_test_utils::test_function_generator;
use crate::test_utils::SierraGenDatabaseForTesting;

test_utils::test_file_test!(
    local_variables,
    [
        "src/local_variables_test_data/block",
        "src/local_variables_test_data/construct_enum",
        "src/local_variables_test_data/match_enum",
        "src/local_variables_test_data/match_extern",
        "src/local_variables_test_data/simple",
        "src/local_variables_test_data/struct",
    ],
    SierraGenDatabaseForTesting,
    check_find_local_variables
);

fn check_find_local_variables(
    db: &mut SierraGenDatabaseForTesting,
    inputs: &OrderedHashMap<String, String>,
) -> OrderedHashMap<String, String> {
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

    let lowered_function = &*db.free_function_lowered(test_function.function_id).unwrap();

    let lowered_formatter = lowering::fmt::LoweredFormatter { db, lowered: lowered_function };
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

test_utils::test_file_test!(
    local_variables_e2e_test,
    ["src/local_variables_test_data/e2e",],
    SierraGenDatabaseForTesting,
    test_function_generator
);
