#[cfg(feature = "std")]
use crate::test_utils::get_test_contract;
#[cfg(feature = "std")]
use test_case::test_case;

use super::*;
use indoc::indoc;

#[test]
fn test_serialization() {
    let external =
        Vec::from([ContractEntryPoint { selector: BigUint::from(u128::MAX), function_idx: 7 }]);

    let contract = ContractClass {
        sierra_program: Vec::new(),
        #[cfg(feature = "std")]
        sierra_program_debug_info: None,
        contract_class_version: String::from("0.1.0"),
        entry_points_by_type: ContractEntryPoints {
            external,
            l1_handler: Vec::new(),
            constructor: Vec::new(),
        },
        abi: None,
    };

    let serialized = serde_json::to_string_pretty(&contract).unwrap();

    #[cfg(feature = "std")]
    let expected_string = indoc! {r#"
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
        }"#};

    #[cfg(not(feature = "std"))]
    let expected_string = indoc! {r#"
        {
          "sierra_program": [],
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
        }"#};

    assert_eq!(&serialized, expected_string);

    assert_eq!(contract, serde_json::from_str(&serialized).unwrap())
}

// Tests the serialization and deserialization of a contract.
#[cfg(feature = "std")]
#[test_case("test_contract::test_contract")]
#[test_case("hello_starknet::hello_starknet")]
#[test_case("erc20::erc_20")]
#[test_case("with_erc20::erc20_contract")]
#[test_case("with_ownable::ownable_balance")]
#[test_case("ownable_erc20::ownable_erc20_contract")]
#[test_case("upgradable_counter::counter_contract")]
#[test_case("mintable::mintable_erc20_ownable")]
#[test_case("multi_component::contract_with_4_components")]
fn test_full_contract_deserialization_from_contracts_crate(example_file_name: &str) {
    let contract =
        get_test_contract(format!("cairo_level_tests::contracts::{example_file_name}").as_str());
    let serialized = serde_json::to_string_pretty(&contract).unwrap();
    assert_eq!(contract, serde_json::from_str(&serialized).unwrap())
}
