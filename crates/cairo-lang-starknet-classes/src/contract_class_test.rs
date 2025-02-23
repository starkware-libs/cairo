use std::io::BufReader;

use indoc::indoc;
use num_bigint::BigUint;
use pretty_assertions::assert_eq;
use test_case::test_case;

use crate::contract_class::{
    ContractClass, ContractEntryPoint, ContractEntryPoints, DEFAULT_CONTRACT_CLASS_VERSION,
};
use crate::test_utils::get_example_file_path;

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

// Tests the serialization and deserialization of a contract.
#[test_case("test_contract__test_contract")]
#[test_case("hello_starknet__hello_starknet")]
#[test_case("libfuncs_coverage__libfuncs_coverage")]
#[test_case("erc20__erc_20")]
#[test_case("with_erc20__erc20_contract")]
#[test_case("with_ownable__ownable_balance")]
#[test_case("ownable_erc20__ownable_erc20_contract")]
#[test_case("upgradable_counter__counter_contract")]
#[test_case("mintable__mintable_erc20_ownable")]
#[test_case("multi_component__contract_with_4_components")]
fn test_full_contract_deserialization_from_contracts_crate(name: &str) {
    let contract_path = get_example_file_path(&format!("{name}.contract_class.json"));
    let deserialized: serde_json::Value =
        serde_json::from_reader(BufReader::new(std::fs::File::open(contract_path).unwrap()))
            .unwrap();
    let contract: ContractClass = serde_json::from_value(deserialized.clone()).unwrap();
    let serialized = serde_json::to_value(&contract).unwrap();
    assert_eq!(serialized, deserialized);
}
