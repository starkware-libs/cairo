use cairo_diagnostics::DiagnosticsBuilder;
use cairo_lowering::lower::lower;
use cairo_semantic::test_utils::setup_test_function;
use cairo_utils::ordered_hash_map::OrderedHashMap;
use cairo_utils::ordered_hash_set::OrderedHashSet;

use super::generate_block_code;
use crate::expr_generator_context::ExprGeneratorContext;
use crate::lifetime::find_variable_lifetime;
use crate::replace_ids::replace_sierra_ids;
use crate::test_utils::SierraGenDatabaseForTesting;
use crate::SierraGeneratorDiagnostic;

cairo_test_utils::test_file_test!(
    block_generator,
    "src/block_generator_test_data",
    {
        function_call: "function_call",
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
    let lowered = lower(db, test_function.function_id).unwrap();

    if lowered.root.is_err() {
        return OrderedHashMap::from([
            ("semantic_diagnostics".into(), semantic_diagnostics),
            ("lowering_diagnostics".into(), lowered.diagnostics.format(db)),
            ("sierra_gen_diagnostics".into(), "".into()),
            ("sierra_code".into(), "".into()),
        ]);
    }

    let block_id = lowered.root.unwrap();
    let block = &lowered.blocks[block_id];

    // Generate (pre-)Sierra statements.
    let mut diagnostics = DiagnosticsBuilder::<SierraGeneratorDiagnostic>::default();
    let lifetime = find_variable_lifetime(&lowered, &OrderedHashSet::default())
        .expect("Failed to retrieve lifetime information.");
    let mut expr_generator_context = ExprGeneratorContext::new(
        db,
        &lowered,
        test_function.function_id,
        &lifetime,
        &mut diagnostics,
    );
    let statements_opt = generate_block_code(&mut expr_generator_context, block_id, block);
    let expected_sierra_code = statements_opt.map_or("None".into(), |statements| {
        statements
            .iter()
            .map(|x| replace_sierra_ids(db, x).to_string())
            .collect::<Vec<String>>()
            .join("\n")
    });

    OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("lowering_diagnostics".into(), lowered.diagnostics.format(db)),
        ("sierra_gen_diagnostics".into(), diagnostics.build().format(db)),
        ("sierra_code".into(), expected_sierra_code),
    ])
}
