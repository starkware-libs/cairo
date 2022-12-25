use indoc::indoc;
use num_bigint::BigUint;
use pretty_assertions::assert_eq;
use test_utils::compare_contents_or_fix_with_path;

use crate::abi;
use crate::contract_class::{ContractClass, ContractEntryPoint, ContractEntryPoints};
use crate::test_utils::{get_example_file_path, get_test_contract};

#[test]
fn test_serialization() {
    let external = vec![ContractEntryPoint { selector: BigUint::from(u128::MAX), function_idx: 7 }];

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
                "function_idx": 7
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

#[test]
fn test_full_contract_deseralization() {
    let contract = get_test_contract();
    let serialized = serde_json::to_string_pretty(&contract).unwrap();
    println!("{}", contract.sierra_program);
    assert_eq!(contract, serde_json::from_str(&serialized).unwrap())
}

#[test]
fn test_compile_path() {
    let contract = get_test_contract();

    compare_contents_or_fix_with_path(
        &get_example_file_path("test_contract.json"),
        serde_json::to_string_pretty(&contract).unwrap() + "\n",
    );

    // There is a separate file for the sierra code as it is hard to review inside the json.
    compare_contents_or_fix_with_path(
        &get_example_file_path("test_contract.sierra"),
        contract.sierra_program.to_string(),
    );
}
