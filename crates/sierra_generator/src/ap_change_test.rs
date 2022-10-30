use defs::db::DefsGroup;
use itertools::Itertools;
use pretty_assertions::assert_eq;
use semantic::test_utils::setup_test_module;
use utils::ordered_hash_map::OrderedHashMap;

use crate::db::SierraGenGroup;
use crate::test_utils::SierraGenDatabaseForTesting;

utils::test_file_test!(
    contains_cycle,
    ["src/ap_change_test_data/contains_cycle",],
    SierraGenDatabaseForTesting,
    contains_cycles_test
);

fn contains_cycles_test(
    db: &mut SierraGenDatabaseForTesting,
    inputs: &OrderedHashMap<String, String>,
) -> OrderedHashMap<String, String> {
    // Parse code and create semantic model.
    let (test_module, semantic_diagnostics) =
        setup_test_module(db, inputs["module_code"].as_str()).split();

    assert_eq!(semantic_diagnostics, "", "Unexpected diagnostics.");

    let result = db
        .module_data(test_module.module_id)
        .unwrap()
        .free_functions
        .iter()
        .map(|(function_id, _)| {
            let name = db.lookup_intern_free_function(*function_id).name(db);
            format!("{}: {:?}", name, db.contains_cycle(*function_id))
        })
        .join("\n");

    // TODO: check lowering diagnostics?

    return OrderedHashMap::from([("result".into(), result)]);
}
