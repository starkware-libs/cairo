use semantic::db::SemanticGroup;
use semantic::test_utils::{setup_test_function, SemanticDatabaseForTesting};
use utils::ordered_hash_map::OrderedHashMap;

use crate::lower::Lowerer;

utils::test_file_test!(
    lowering_test,
    ["src/test_data/tests"],
    SemanticDatabaseForTesting,
    test_function_lowering
);

fn test_function_lowering(
    db: &mut (dyn SemanticGroup + 'static),
    inputs: &OrderedHashMap<String, String>,
) -> OrderedHashMap<String, String> {
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
    )
    .split();
    let lowered = Lowerer::lower(db, test_function.function_id).unwrap();

    // TODO(spapini): Test some textual representation of the output.
    OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("lowering_diagnostics".into(), lowered.diagnostics.format(db)),
    ])
}
