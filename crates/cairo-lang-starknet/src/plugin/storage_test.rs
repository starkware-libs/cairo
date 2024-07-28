use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::attribute::SemanticQueryAttrs;
use cairo_lang_semantic::test_utils::setup_test_module;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_test_utils::{get_direct_or_file_content, verify_diagnostics_expectation};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::plugin::consts::CONTRACT_ATTR;
use crate::starknet_plugin_suite;

/// Helper function for testing ABI failures.
pub fn test_storage_path_check(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut RootDatabase::builder()
        .detect_corelib()
        .with_plugin_suite(starknet_plugin_suite())
        .build()
        .unwrap();
    let (_, cairo_code) = get_direct_or_file_content(&inputs["cairo_code"]);
    let (module, diagnostics) = setup_test_module(db, &cairo_code).split();

    let submodules = db.module_submodules_ids(module.module_id).unwrap();
    let contract_submodule = submodules
        .iter()
        .find(|submodule| submodule.has_attr(db, CONTRACT_ATTR).unwrap())
        .expect("No starknet::contract found in input code.");
    let contract_module_id = ModuleId::Submodule(*contract_submodule);
    let contract_diagnostics = db.module_semantic_diagnostics(contract_module_id).unwrap();
    let diagnostic_string = contract_diagnostics.format(db);
    let test_error = verify_diagnostics_expectation(args, &diagnostics);

    TestRunnerResult {
        outputs: OrderedHashMap::from([
            ("expected_diagnostics".into(), diagnostics),
            ("diagnostics".into(), diagnostic_string),
        ]),
        error: test_error,
    }
}

cairo_lang_test_utils::test_file_test!(
  storage_path_check,
  "src/test_data",
  {
      storage_path_check: "storage_path_check",
  },
  test_storage_path_check
);
