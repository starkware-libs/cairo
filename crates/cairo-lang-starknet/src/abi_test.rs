use std::sync::Arc;

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_semantic::items::attribute::SemanticQueryAttrs;
use cairo_lang_semantic::test_utils::setup_test_module;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_test_utils::{get_direct_or_file_content, verify_diagnostics_expectation};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::abi::AbiBuilder;
use crate::inline_macros::get_starknet_inline_macro_plugins;
use crate::plugin::StarkNetPlugin;

/// Helper function for testing ABI failures.
pub fn test_abi_failure(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut RootDatabase::builder()
        .detect_corelib()
        .with_macro_plugin(Arc::new(StarkNetPlugin::default()))
        .with_inline_macro_plugins(get_starknet_inline_macro_plugins())
        .build()
        .unwrap();
    let (_, cairo_code) = get_direct_or_file_content(&inputs["cairo_code"]);
    let (module, diagnostics) = setup_test_module(db, &cairo_code).split();

    let submodules = db.module_submodules_ids(module.module_id).unwrap();
    let contract_submodule = submodules
        .iter()
        .find(|submodule| submodule.has_attr(db, "starknet::contract").unwrap())
        .expect("No starknet::contract found in input code.");
    let abi_error = AbiBuilder::submodule_as_contract_abi(db, *contract_submodule).unwrap_err();

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
