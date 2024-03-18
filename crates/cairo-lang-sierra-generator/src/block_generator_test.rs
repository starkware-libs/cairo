use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_filesystem::db::FilesGroupEx;
use cairo_lang_filesystem::flag::Flag;
use cairo_lang_filesystem::ids::FlagId;
use cairo_lang_lowering as lowering;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::BlockId;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::UpcastMut;
use lowering::fmt::LoweredFormatter;
use lowering::ids::ConcreteFunctionWithBodyId;

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
        serialization: "serialization",
        early_return: "early_return",
        panic: "panic",
    },
    block_generator_test
);

fn block_generator_test(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut SierraGenDatabaseForTesting::new_empty();

    // Tests have recursions for revoking AP. Automatic addition of 'withdraw_gas` calls would add
    // unnecessary complication to them.
    let add_withdraw_gas_flag_id = FlagId::new(db.upcast_mut(), "add_withdraw_gas");
    db.set_flag(add_withdraw_gas_flag_id, Some(Arc::new(Flag::AddWithdrawGas(false))));

    // Parse code and create semantic model.
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
    )
    .split();

    // Lower code.
    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);
    let lowering_diagnostics =
        db.function_with_body_lowering_diagnostics(function_id.function_with_body_id(db)).unwrap();

    let lowered = match db.final_concrete_function_with_body_lowered(function_id) {
        Ok(lowered) if !lowered.blocks.is_empty() => lowered,
        _ => {
            return TestRunnerResult::success(OrderedHashMap::from([
                ("semantic_diagnostics".into(), semantic_diagnostics),
                ("lowering_diagnostics".into(), lowering_diagnostics.format(db)),
                ("sierra_gen_diagnostics".into(), "".into()),
                ("sierra_code".into(), "".into()),
            ]));
        }
    };

    // Generate (pre-)Sierra statements.
    let lifetime = find_variable_lifetime(&lowered, &OrderedHashSet::default())
        .expect("Failed to retrieve lifetime information.");
    let mut expr_generator_context = ExprGeneratorContext::new(
        db,
        &lowered,
        function_id,
        &lifetime,
        crate::ap_tracking::ApTrackingConfiguration::default(),
    );

    let mut expected_sierra_code = String::default();

    generate_block_code(&mut expr_generator_context, BlockId::root()).unwrap();
    for statement in expr_generator_context.statements() {
        expected_sierra_code.push_str(&replace_sierra_ids(db, &statement).statement.to_string(db));
        expected_sierra_code.push('\n');
    }

    let lowered_formatter = LoweredFormatter::new(db, &lowered.variables);
    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("lowering_diagnostics".into(), lowering_diagnostics.format(db)),
        ("lowering_flat".into(), format!("{:?}", lowered.debug(&lowered_formatter))),
        ("sierra_code".into(), expected_sierra_code),
    ]))
}
