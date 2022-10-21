use diagnostics::DiagnosticsBuilder;
use lowering::lower::lower;
use semantic::test_utils::setup_test_function;
use utils::ordered_hash_map::OrderedHashMap;

use super::generate_block_code;
use crate::expr_generator_context::ExprGeneratorContext;
use crate::test_utils::{replace_libfunc_ids, SierraGenDatabaseForTesting};
use crate::SierraGeneratorDiagnostic;

utils::test_file_test!(
    lowering_test,
    [
        "src/block_generator_test_data/function_call",
        "src/block_generator_test_data/literals",
        "src/block_generator_test_data/match"
    ],
    SierraGenDatabaseForTesting,
    block_generator_test
);

fn block_generator_test(
    db: &mut SierraGenDatabaseForTesting,
    inputs: &OrderedHashMap<String, String>,
) -> OrderedHashMap<String, String> {
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

    if lowered.root.is_none() {
        return OrderedHashMap::from([
            ("semantic_diagnostics".into(), semantic_diagnostics),
            ("lowering_diagnostics".into(), lowered.diagnostics.format(db)),
            ("sierra_gen_diagnostics".into(), "".into()),
            ("sierra_code".into(), "".into()),
        ]);
    }

    let block = &lowered.blocks[lowered.root.unwrap()];

    // Generate (pre-)Sierra statements.
    let mut diagnostics = DiagnosticsBuilder::<SierraGeneratorDiagnostic>::default();
    let mut expr_generator_context =
        ExprGeneratorContext::new(db, Some(&lowered), test_function.function_id, &mut diagnostics);
    let statements_opt = generate_block_code(&mut expr_generator_context, block);
    let expected_sierra_code = statements_opt.map_or("None".into(), |statements| {
        statements
            .iter()
            .map(|x| replace_libfunc_ids(db, x).to_string())
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
