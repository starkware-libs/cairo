use cairo_lang_debug::DebugWithDb;
use cairo_lang_lowering as lowering;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::BlockId;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use lowering::fmt::LoweredFormatter;

use super::generate_block_code;
use crate::expr_generator_context::ExprGeneratorContext;
use crate::lifetime::find_variable_lifetime;
use crate::replace_ids::replace_sierra_ids;
use crate::test_utils::SierraGenDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    block_generator,
    "src/block_generator_test_data",
    {
        function_call: "function_call",
        inline: "inline",
        literals: "literals",
        match_: "match",
        early_return: "early_return",
    },
    block_generator_test
);

fn block_generator_test(inputs: &OrderedHashMap<String, String>) -> OrderedHashMap<String, String> {
    let db = &mut SierraGenDatabaseForTesting::default();
    // Parse code and create semantic model.
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
    )
    .split();

    // Lower code.
    let lowering_diagnostics =
        db.function_with_body_lowering_diagnostics(test_function.function_id).unwrap();
    let lowered =
        db.concrete_function_with_body_lowered(test_function.concrete_function_id).unwrap();

    if lowered.blocks.is_empty() {
        return OrderedHashMap::from([
            ("semantic_diagnostics".into(), semantic_diagnostics),
            ("lowering_diagnostics".into(), lowering_diagnostics.format(db)),
            ("sierra_gen_diagnostics".into(), "".into()),
            ("sierra_code".into(), "".into()),
        ]);
    };

    // Generate (pre-)Sierra statements.
    let lifetime = find_variable_lifetime(&lowered, &OrderedHashSet::default())
        .expect("Failed to retrieve lifetime information.");
    let mut expr_generator_context =
        ExprGeneratorContext::new(db, &lowered, test_function.concrete_function_id, &lifetime);

    let mut expected_sierra_code = String::default();
    let mut block_id = BlockId::root();

    loop {
        let (statements, _) = generate_block_code(&mut expr_generator_context, block_id).unwrap();

        for statement in &statements {
            expected_sierra_code.push_str(&replace_sierra_ids(db, statement).to_string());
            expected_sierra_code.push('\n');
        }

        match &lowered.blocks[block_id].end {
            lowering::FlatBlockEnd::Fallthrough(target_block_id, _) => block_id = *target_block_id,
            lowering::FlatBlockEnd::Return(_)
            | lowering::FlatBlockEnd::Goto(_, _)
            | lowering::FlatBlockEnd::Match { .. }
            | lowering::FlatBlockEnd::NotSet => {
                break;
            }
        }
    }

    let lowered_formatter = LoweredFormatter { db, variables: &lowered.variables };
    OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("lowering_diagnostics".into(), lowering_diagnostics.format(db)),
        ("lowering_flat".into(), format!("{:?}", lowered.debug(&lowered_formatter))),
        ("sierra_code".into(), expected_sierra_code),
    ])
}
