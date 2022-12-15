use test_utils::compare_contents_or_fix_with_path;

use crate::casm_contract_class::CasmContractClass;
use crate::test_utils::{get_example_file_path, get_test_contract};

#[test]
fn test_casm_contract_from_contract_class() {
    let contract_class = get_test_contract();
    let casm_contract = CasmContractClass::from_contract_class(contract_class).unwrap();

    compare_contents_or_fix_with_path(
        &get_example_file_path("test_contract.casm"),
        serde_json::to_string_pretty(&casm_contract).unwrap() + "\n",
    );
}
