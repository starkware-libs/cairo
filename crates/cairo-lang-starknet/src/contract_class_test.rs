use cairo_lang_test_utils::compare_contents_or_fix_with_path;
use indoc::indoc;
use num_bigint::BigUint;
use pretty_assertions::assert_eq;
use test_case::test_case;

use crate::allowed_libfuncs::{validate_compatible_sierra_version, ListSelector};
use crate::contract_class::{
    ContractClass, ContractEntryPoint, ContractEntryPoints, DEFAULT_CONTRACT_CLASS_VERSION,
};
use crate::felt252_serde::sierra_from_felt252s;
use crate::sierra_version;
use crate::test_utils::{get_example_file_path, get_test_contract};

#[test]
fn test_serialization() {
    let external = vec![ContractEntryPoint { selector: BigUint::from(u128::MAX), function_idx: 7 }];

    let contract = ContractClass {
        sierra_program: vec![],
        sierra_program_debug_info: None,
        contract_class_version: DEFAULT_CONTRACT_CLASS_VERSION.to_string(),
        entry_points_by_type: ContractEntryPoints {
            external,
            l1_handler: vec![],
            constructor: vec![],
        },
        abi: None,
    };

    let serialized = serde_json::to_string_pretty(&contract).unwrap();

    assert_eq!(
        &serialized,
        indoc! {
            r#"
        {
          "sierra_program": [],
          "sierra_program_debug_info": null,
          "contract_class_version": "0.1.0",
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
          "abi": null
        }"#}
    );

    assert_eq!(contract, serde_json::from_str(&serialized).unwrap())
}

#[test_case("test_contract")]
#[test_case("hello_starknet")]
#[test_case("erc20")]
fn test_full_contract_deseralization(example_file_name: &str) {
    let contract = get_test_contract(format!("{example_file_name}.cairo").as_str());
    let serialized = serde_json::to_string_pretty(&contract).unwrap();
    assert_eq!(contract, serde_json::from_str(&serialized).unwrap())
}

/// Tests that the sierra compiled from <test_case>.cairo is the same as in <test_case>.sierra, and
/// that the resulted json is the same as in <test_case>.json.
#[test_case("account")]
#[test_case("test_contract")]
#[test_case("minimal_contract")]
#[test_case("hello_starknet")]
#[test_case("erc20")]
#[test_case("token_bridge")]
fn test_compile_path(example_file_name: &str) {
    let contract = get_test_contract(format!("{example_file_name}.cairo").as_str());

    let list_selector = ListSelector::ListName("experimental_v0.1.0".to_string());
    validate_compatible_sierra_version(&contract, list_selector).unwrap();

    compare_contents_or_fix_with_path(
        &get_example_file_path(format!("{example_file_name}.json").as_str()),
        serde_json::to_string_pretty(&contract).unwrap() + "\n",
    );

    let (version_id, mut sierra_program) = sierra_from_felt252s(&contract.sierra_program).unwrap();
    assert_eq!(
        version_id,
        sierra_version::VersionId::current_version_id(),
        "Serialized Sierra version should be the current version."
    );
    contract.sierra_program_debug_info.unwrap().populate(&mut sierra_program);

    // There is a separate file for the sierra code as it is hard to review inside the json.
    compare_contents_or_fix_with_path(
        &get_example_file_path(format!("{example_file_name}.sierra").as_str()),
        sierra_program.to_string(),
    );
}
