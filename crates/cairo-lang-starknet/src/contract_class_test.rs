use std::collections::HashMap;

use cairo_lang_test_utils::compare_contents_or_fix_with_path;
use indoc::indoc;
use num_bigint::BigUint;
use pretty_assertions::assert_eq;
use test_case::test_case;

use crate::abi;
use crate::contract_class::{ContractClass, ContractEntryPoint, ContractEntryPoints};
use crate::felt_serde::sierra_from_felts;
use crate::test_utils::{get_example_file_path, get_test_contract};

#[test]
fn test_serialization() {
    let external = vec![ContractEntryPoint { selector: BigUint::from(u128::MAX), function_idx: 7 }];

    let contract = ContractClass {
        sierra_program: vec![],
        sierra_program_debug_info: cairo_lang_sierra::debug_info::DebugInfo {
            type_names: HashMap::default(),
            libfunc_names: HashMap::default(),
            user_func_names: HashMap::default(),
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
          "sierra_program": [],
          "sierra_program_debug_info": {
            "type_names": [],
            "libfunc_names": [],
            "user_func_names": []
          },
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

#[test_case("test_contract")]
#[test_case("hello_starknet")]
fn test_full_contract_deseralization(example_file_name: &str) {
    let contract = get_test_contract(format!("{}.cairo", example_file_name).as_str());
    let serialized = serde_json::to_string_pretty(&contract).unwrap();
    assert_eq!(contract, serde_json::from_str(&serialized).unwrap())
}

#[test_case("test_contract")]
#[test_case("hello_starknet")]
fn test_compile_path(example_file_name: &str) {
    let contract = get_test_contract(format!("{}.cairo", example_file_name).as_str());

    compare_contents_or_fix_with_path(
        &get_example_file_path(format!("{}.json", example_file_name).as_str()),
        serde_json::to_string_pretty(&contract).unwrap() + "\n",
    );

    let mut sierra_program = sierra_from_felts(&contract.sierra_program).unwrap();
    contract.sierra_program_debug_info.populate(&mut sierra_program);

    // There is a separate file for the sierra code as it is hard to review inside the json.
    compare_contents_or_fix_with_path(
        &get_example_file_path(format!("{}.sierra", example_file_name).as_str()),
        sierra_program.to_string(),
    );
}
