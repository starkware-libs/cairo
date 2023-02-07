use cairo_lang_debug::DebugWithDb;
use cairo_lang_lowering as lowering;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use itertools::Itertools;
use lowering::VariableId;

use super::{inner_find_local_variables, FindLocalsContext, LocalVariablesState};
use crate::function_generator_test_utils::test_function_generator;
use crate::test_utils::SierraGenDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    find_local_variables,
    "src/local_variables_test_data",
    {
        block: "block",
        construct_enum: "construct_enum",
        // TODO(ilya): Enable inline test.
        //  inline: "inline",
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

    let mut res = OrderedHashSet::<VariableId>::default();
    let mut ctx =
        FindLocalsContext { db, lowered_function, block_infos: OrderedHashMap::default() };
    inner_find_local_variables(
        &mut ctx,
        lowered_function.root.unwrap(),
        LocalVariablesState::default(),
        &mut res,
    )
    .unwrap();

    let local_variables_str =
        res.iter().map(|var_id| format!("{:?}", var_id.debug(&lowered_formatter))).join(", ");

    let block_infos_str = ctx
        .block_infos
        .iter()
        .map(|(block_id, info)| {
            format!("blk{}: known_ap_change: {}.", block_id.0, info.known_ap_change)
        })
        .join("\n");

    OrderedHashMap::from([
        ("lowering_format".into(), lowered_str),
        ("local_variables".into(), local_variables_str),
        ("block_infos".into(), block_infos_str),
    ])
}

cairo_lang_test_utils::test_file_test!(
    e2e,
    "src/local_variables_test_data",
    {e2e: "e2e"},
    test_function_generator
);
