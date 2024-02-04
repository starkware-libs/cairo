use std::io::BufReader;

use cairo_felt::Felt252;
use cairo_lang_test_utils::compare_contents_or_fix_with_path;
use test_case::test_case;

use crate::casm_contract_class::{BigUintAsHex, CasmContractClass, StarknetSierraCompilationError};
use crate::contract_class::ContractClass;
use crate::test_utils::get_example_file_path;

#[test_case("test_contract__test_contract")]
#[test_case("new_syntax_test_contract__counter_contract")]
fn test_casm_contract_from_contract_class_failure(name: &str) {
    let f =
        std::fs::File::open(get_example_file_path(&format!("{name}.contract_class.json"))).unwrap();
    let mut contract_class: ContractClass = serde_json::from_reader(BufReader::new(f)).unwrap();
    contract_class.sierra_program[17] = BigUintAsHex { value: Felt252::prime() };

    let add_pythonic_hints = false;
    assert_eq!(
        CasmContractClass::from_contract_class(contract_class, add_pythonic_hints),
        Err(StarknetSierraCompilationError::ValueOutOfRange)
    );
}

/// Tests that the casm compiled from a contract in the contract_crate is the same as in
/// <test_case>.compiled_contract_class.json.
#[test_case("account__account")]
#[test_case("test_contract__test_contract")]
#[test_case("new_syntax_test_contract__counter_contract")]
#[test_case("minimal_contract__minimal_contract")]
#[test_case("hello_starknet__hello_starknet")]
#[test_case("erc20__erc_20")]
#[test_case("token_bridge__token_bridge")]
#[test_case("with_erc20__erc20_contract")]
#[test_case("with_ownable__ownable_balance")]
#[test_case("ownable_erc20__ownable_erc20_contract")]
#[test_case("upgradable_counter__counter_contract")]
#[test_case("mintable__mintable_erc20_ownable")]
#[test_case("multi_component__contract_with_4_components")]
fn test_casm_contract_from_contract_class_from_contracts_crate(name: &str) {
    let contract_path = get_example_file_path(&format!("{name}.contract_class.json"));
    let contract: ContractClass =
        serde_json::from_reader(BufReader::new(std::fs::File::open(contract_path).unwrap()))
            .unwrap();
    let add_pythonic_hints = true;
    let casm_contract =
        CasmContractClass::from_contract_class(contract, add_pythonic_hints).unwrap();
    compare_contents_or_fix_with_path(
        &get_example_file_path(&format!("{name}.compiled_contract_class.json")),
        serde_json::to_string_pretty(&casm_contract).unwrap() + "\n",
    );
}
