use cairo_lang_debug::DebugWithDb;
use cairo_lang_lowering as lowering;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;
use lowering::ids::ConcreteFunctionWithBodyId;

use super::AnalyzeApChangesResult;
use crate::function_generator_test_utils::test_function_generator;
use crate::test_utils::SierraGenDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    find_local_variables,
    "src/local_variables_test_data",
    {
        block: "block",
        construct_enum: "construct_enum",
        inline: "inline",
        match_enum: "match_enum",
        match_extern: "match_extern",
        simple: "simple",
        snapshot: "snapshot",
        struct_: "struct",
    },
    check_find_local_variables
);

fn check_find_local_variables(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    // Tests have recursions for revoking AP. Automatic addition of 'withdraw_gas` calls would add
    // unnecessary complication to them.
    let db = &SierraGenDatabaseForTesting::without_add_withdraw_gas();

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

    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);
    let lowered_function = &*db.final_concrete_function_with_body_lowered(function_id).unwrap();

    let lowered_formatter = lowering::fmt::LoweredFormatter::new(db, &lowered_function.variables);
    let lowered_str = format!("{:?}", lowered_function.debug(&lowered_formatter));

    let AnalyzeApChangesResult { known_ap_change: _, local_variables, .. } =
        super::analyze_ap_changes(db, lowered_function).unwrap();

    let local_variables_str = local_variables
        .iter()
        .map(|var_id| format!("{:?}", var_id.debug(&lowered_formatter)))
        .join(", ");

    TestRunnerResult::success(OrderedHashMap::from([
        ("lowering_format".into(), lowered_str),
        ("local_variables".into(), local_variables_str),
    ]))
}

cairo_lang_test_utils::test_file_test!(
    e2e,
    "src/local_variables_test_data",
    {e2e: "e2e"},
    test_function_generator
);
