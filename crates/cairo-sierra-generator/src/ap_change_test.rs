use defs::db::DefsGroup;
use itertools::Itertools;
use lowering::db::LoweringGroup;
use pretty_assertions::assert_eq;
use semantic::test_utils::setup_test_module;
use utils::ordered_hash_map::OrderedHashMap;

use crate::db::SierraGenGroup;
use crate::test_utils::SierraGenDatabaseForTesting;

test_utils::test_file_test!(
    ap_change,
    ["src/ap_change_test_data/tests",],
    SierraGenDatabaseForTesting,
    contains_cycles_test
);

fn contains_cycles_test(
    db: &mut SierraGenDatabaseForTesting,
    inputs: &OrderedHashMap<String, String>,
) -> OrderedHashMap<String, String> {
    // Parse code and create semantic model.
    let test_module = setup_test_module(db, inputs["module_code"].as_str()).unwrap();

    db.module_lowering_diagnostics(test_module.module_id)
        .unwrap()
        .expect_with_db(db, "Unexpected diagnostics.");

    let result = db
        .module_data(test_module.module_id)
        .unwrap()
        .free_functions
        .iter()
        .map(|(function_id, _)| {
            let name = db.lookup_intern_free_function(*function_id).name(db);
            format!(
                "{}: ap_change={:?}, has_cycles={:?}",
                name,
                db.get_ap_change(*function_id),
                db.contains_cycle(*function_id),
            )
        })
        .join("\n");

    OrderedHashMap::from([("result".into(), result)])
}
