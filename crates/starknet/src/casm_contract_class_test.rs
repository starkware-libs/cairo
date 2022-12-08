use std::fs;
use std::path::PathBuf;

use test_utils::compare_contents_or_fix_with_path;

use crate::casm_contract_class::CasmContractClass;

/// Returns a path to example contract that matches `name`.
pub fn get_example_file_path(file_name: &str) -> PathBuf {
    // Pop the "/sierra_to_casm" suffix.
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.extend(["test_data", file_name].into_iter());
    path
}

#[test]
fn test_casm_contract_from_contract_class() {
    let path = get_example_file_path("test_contract.json");

    let contract_class = serde_json::from_str(
        &fs::read_to_string(&path)
            .unwrap_or_else(|_| format!("Failed to read {}.", path.to_str().unwrap())),
    )
    .unwrap();

    let casm_contract = CasmContractClass::from_contract_class(contract_class).unwrap();

    compare_contents_or_fix_with_path(
        &get_example_file_path("test_contract.casm"),
        serde_json::to_string_pretty(&casm_contract).unwrap() + "\n",
    );
}
