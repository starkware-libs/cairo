use cairo_lang_debug::DebugWithDb;
use cairo_lang_lowering as lowering;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;
use lowering::ids::ConcreteFunctionWithBodyId;

use super::find_variable_lifetime;
use crate::local_variables::{analyze_ap_changes, AnalyzeApChangesResult};
use crate::test_utils::SierraGenDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    variable_lifetime,
    "src/lifetime_test_data",
    {
        block: "block",
        early_return: "early_return",
        enum_: "enum",
        inline: "inline",
        locals: "locals",
        simple: "simple",
        snapshot: "snapshot",
        struct_: "struct",
        match_: "match",
    },
    check_variable_lifetime
);

fn check_variable_lifetime(
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
        analyze_ap_changes(db, lowered_function).unwrap();
    let find_variable_lifetime_res = find_variable_lifetime(lowered_function, &local_variables)
        .expect("find_variable_lifetime failed unexpectedly");
    let last_use_str = find_variable_lifetime_res
        .last_use
        .iter()
        .map(|location| {
            let block = &lowered_function.blocks[location.statement_location.0];
            let statements = &block.statements;
            let var_id = if location.statement_location.1 == statements.len() {
                match &block.end {
                    lowering::FlatBlockEnd::Goto(_, remapping) => {
                        remapping.values().nth(location.idx).unwrap().var_id
                    }
                    lowering::FlatBlockEnd::Return(returns, _location) => {
                        returns[location.idx].var_id
                    }
                    lowering::FlatBlockEnd::Panic(_) => {
                        unreachable!("Panics should have been stripped in a previous phase.")
                    }
                    lowering::FlatBlockEnd::NotSet => unreachable!(),
                    lowering::FlatBlockEnd::Match { info } => info.inputs()[location.idx].var_id,
                }
            } else {
                statements[location.statement_location.1].inputs()[location.idx].var_id
            };
            format!("v{}: {location:?}", var_id.index())
        })
        .join("\n");
    let drop_str = find_variable_lifetime_res
        .drops
        .iter()
        .map(|(location, vars)| {
            format!("{location:?}: {}", vars.iter().map(|var_id| format!("{var_id:?}")).join(", "))
        })
        .join("\n");

    TestRunnerResult::success(OrderedHashMap::from([
        ("lowering_format".into(), lowered_str),
        ("last_use".into(), last_use_str),
        ("drops".into(), drop_str),
    ]))
}
