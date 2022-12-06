use std::path::PathBuf;

use indoc::indoc;
use num_bigint::BigUint;
use pretty_assertions::assert_eq;
use test_utils::compare_contents_or_fix_with_path;

use crate::abi;
use crate::contract_class::{compile_path, ContractClass, ContractEntryPoint, ContractEntryPoints};

#[test]
fn test_serialization() {
    let external = vec![ContractEntryPoint { selector: BigUint::from(u128::MAX), function_id: 7 }];

    let contract = ContractClass {
        sierra_program: sierra::program::Program {
            type_declarations: vec![],
            libfunc_declarations: vec![],
            statements: vec![],
            funcs: vec![],
        },
        entry_points_by_type: ContractEntryPoints {
            external,
            l1_handler: vec![],
            constructor: vec![],
        },
        abi: abi::Contract::default(),
    };

    let serialized = serde_json::to_string_pretty(&contract).unwrap();

    assert_eq!(
        &serialized,
        indoc! {
            r#"
        {
          "sierra_program": "\n\n\n",
          "entry_points_by_type": {
            "EXTERNAL": [
              {
                "selector": "0xffffffffffffffffffffffffffffffff",
                "function_id": 7
              }
            ],
            "L1_HANDLER": [],
            "CONSTRUCTOR": []
          },
          "abi": []
        }"#}
    );

    assert_eq!(contract, serde_json::from_str(&serialized).unwrap())
}

/// Returns a path to example contract that matches `name`.
pub fn get_example_file_path(file_name: &str) -> PathBuf {
    // Pop the "/sierra_to_casm" suffix.
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.extend(["examples", file_name].into_iter());
    path
}

#[test]
fn test_compile_path() {
    let path = get_example_file_path("test_contract.cairo");

    let replace_ids = true;
    let contract = compile_path(&path, replace_ids).unwrap();

    compare_contents_or_fix_with_path(
        &get_example_file_path("test_contract.sierra"),
        contract.sierra_program.to_string(),
    );

    compare_contents_or_fix_with_path(
        &get_example_file_path("test_contract.abi"),
        serde_json::to_string_pretty(&contract.abi).unwrap() + "\n",
    );

    compare_contents_or_fix_with_path(
        &get_example_file_path("test_contract.entry_points"),
        serde_json::to_string_pretty(&contract.entry_points_by_type).unwrap() + "\n",
    );
}
