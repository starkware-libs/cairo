use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_semantic::items::attribute::SemanticQueryAttrs;
use cairo_lang_semantic::test_utils::setup_test_module;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_test_utils::{get_direct_or_file_content, verify_diagnostics_expectation};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::{AbiBuilder, BuilderConfig};
use crate::plugin::consts::CONTRACT_ATTR;
use crate::starknet_plugin_suite;

/// Helper function for testing ABI failures.
pub fn test_abi_failure(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut RootDatabase::builder()
        .detect_corelib()
        .with_default_plugin_suite(starknet_plugin_suite())
        .build()
        .unwrap();
    let (_, cairo_code) = get_direct_or_file_content(&inputs["cairo_code"]);
    let (module, diagnostics) = setup_test_module(db, &cairo_code).split();

    let submodules = db.module_submodules_ids(module.module_id).unwrap();
    let contract_submodule = submodules
        .iter()
        .find(|submodule| submodule.has_attr(db, CONTRACT_ATTR).unwrap())
        .expect("No starknet::contract found in input code.");
    let abi_error = AbiBuilder::from_submodule(
        db,
        *contract_submodule,
        BuilderConfig { account_contract_validations: true },
    )
    .expect("No basic errors")
    .finalize()
    .unwrap_err();

    let test_error = verify_diagnostics_expectation(args, &diagnostics);

    TestRunnerResult {
        outputs: OrderedHashMap::from([
            ("expected_error".into(), abi_error.to_string()),
            ("expected_diagnostics".into(), diagnostics),
        ]),
        error: test_error,
    }
}

cairo_lang_test_utils::test_file_test!(
  abi_failures,
  "src/test_data",
  {
      abi_failures: "abi_failures",
  },
  test_abi_failure
);

/// Helper function for testing multiple Storage path accesses to the same place.
pub fn test_storage_path_check(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut RootDatabase::builder()
        .detect_corelib()
        .with_default_plugin_suite(starknet_plugin_suite())
        .build()
        .unwrap();
    let (_, cairo_code) = get_direct_or_file_content(&inputs["cairo_code"]);
    let (_, diagnostics) = setup_test_module(db, &cairo_code).split();

    let test_error = verify_diagnostics_expectation(args, &diagnostics);

    TestRunnerResult {
        outputs: OrderedHashMap::from([("diagnostics".into(), diagnostics)]),
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
