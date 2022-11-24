use lowering::db::LoweringGroup;
use semantic::test_utils::setup_test_function;
use utils::ordered_hash_map::OrderedHashMap;

use crate::db::SierraGenGroup;
use crate::replace_ids::replace_sierra_ids;
use crate::test_utils::SierraGenDatabaseForTesting;

/// Compiles a single function to Sierra and checks the generated code.
pub fn test_function_generator(
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

    // Verify that there are no diagnostics.
    let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id);
    let sierra_gen_diagnostics = db.free_function_sierra_diagnostics(test_function.function_id);

    // Compile the function.
    let function = db.free_function_sierra(test_function.function_id);
    let sierra_code: String = function.map_or("None".into(), |func| {
        func.body
            .iter()
            .map(|x| replace_sierra_ids(db, x).to_string())
            .collect::<Vec<String>>()
            .join("\n")
    });

    OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        (
            "lowering_diagnostics".into(),
            lowering_diagnostics.map_or("".into(), |diagnostics| diagnostics.format(db)),
        ),
        ("sierra_gen_diagnostics".into(), sierra_gen_diagnostics.format(db)),
        ("sierra_code".into(), sierra_code),
    ])
}
