use cairo_lang_defs::db::DefsGroup;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_semantic::test_utils::setup_test_module;
use cairo_lang_semantic::ConcreteFunctionWithBodyId;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;

use crate::db::SierraGenGroup;
use crate::test_utils::SierraGenDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    ap_change,
    "src/ap_change_test_data",
    {tests: "tests"},
    contains_cycles_test
);

fn contains_cycles_test(inputs: &OrderedHashMap<String, String>) -> OrderedHashMap<String, String> {
    let db = &mut SierraGenDatabaseForTesting::default();
    // Parse code and create semantic model.
    let test_module = setup_test_module(db, inputs["module_code"].as_str()).unwrap();

    db.module_lowering_diagnostics(test_module.module_id)
        .unwrap()
        .expect_with_db(db, "Unexpected diagnostics.");

    let result = db
        .module_free_functions(test_module.module_id)
        .unwrap()
        .iter()
        .map(|(free_function_id, _)| {
            let function_id =
                ConcreteFunctionWithBodyId::from_no_generics_free(db, *free_function_id).unwrap();
            format!(
                "{}: ap_change={:?}, has_cycles={:?}",
                free_function_id.name(db),
                db.get_ap_change(function_id),
                db.contains_cycle(function_id),
            )
        })
        .join("\n");

    OrderedHashMap::from([("result".into(), result)])
}
