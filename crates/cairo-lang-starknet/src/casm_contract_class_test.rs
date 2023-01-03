use cairo_lang_test_utils::compare_contents_or_fix_with_path;
use test_case::test_case;

use crate::casm_contract_class::CasmContractClass;
use crate::test_utils::{get_example_file_path, get_test_contract};

#[test_case("test_contract")]
#[test_case("hello_starknet")]
fn test_casm_contract_from_contract_class(example_file_name: &str) {
    let contract_class = get_test_contract(format!("{}.cairo", example_file_name).as_str());
    let casm_contract = CasmContractClass::from_contract_class(contract_class).unwrap();

    compare_contents_or_fix_with_path(
        &get_example_file_path(format!("{}.casm", example_file_name).as_str()),
        serde_json::to_string_pretty(&casm_contract).unwrap() + "\n",
    );
}
