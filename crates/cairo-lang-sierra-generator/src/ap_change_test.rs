use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::NamedLanguageElementId;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_semantic::db::PluginSuiteInput;
use cairo_lang_semantic::inline_macros::get_default_plugin_suite;
use cairo_lang_semantic::test_utils::TestModule;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
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

fn contains_cycles_test(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut SierraGenDatabaseForTesting::default();
    // Parse code and create semantic model.
    let test_module_builder = TestModule::builder(db, inputs["module_code"].as_str(), None);

    let crate_id = unsafe { test_module_builder.get_crate_id() };
    db.set_crate_plugins_from_suite(crate_id, get_default_plugin_suite());

    let test_module = test_module_builder.build_and_check_for_diagnostics(db).unwrap();

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
                db.final_contains_call_cycle(function_id),
            )
        })
        .join("\n");

    TestRunnerResult::success(OrderedHashMap::from([("result".into(), result)]))
}
