use std::io::BufReader;

use cairo_felt::Felt252;
use cairo_lang_test_utils::compare_contents_or_fix_with_path;
use test_case::test_case;

use crate::casm_contract_class::{BigUintAsHex, CasmContractClass, StarknetSierraCompilationError};
use crate::contract_class::ContractClass;
use crate::test_utils::{
    get_contract_file_name_from_path, get_example_file_path, get_test_contract,
};

#[test_case("test_contract::test_contract")]
#[test_case("new_syntax_test_contract::counter_contract")]
fn test_casm_contract_from_contract_class_failure(example_contract_path: &str) {
    let example_file_name = get_contract_file_name_from_path(example_contract_path);
    let f = std::fs::File::open(get_example_file_path(&format!(
        "{example_file_name}.contract_class.json"
    )))
    .unwrap();
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
#[test_case("account::account")]
#[test_case("test_contract::test_contract")]
#[test_case("new_syntax_test_contract::counter_contract")]
#[test_case("minimal_contract::minimal_contract")]
#[test_case("hello_starknet::hello_starknet")]
#[test_case("erc20::erc_20")]
#[test_case("token_bridge::token_bridge")]
#[test_case("with_erc20::erc20_contract")]
#[test_case("with_ownable::ownable_balance")]
#[test_case("ownable_erc20::ownable_erc20_contract")]
#[test_case("upgradable_counter::counter_contract")]
#[test_case("mintable::mintable_erc20_ownable")]
#[test_case("multi_component::contract_with_4_components")]
fn test_casm_contract_from_contract_class_from_contracts_crate(example_contract_path: &str) {
    let contract_class = get_test_contract(
        format!("cairo_level_tests::contracts::{example_contract_path}").as_str(),
    );
    let example_file_name = get_contract_file_name_from_path(example_contract_path);
    let add_pythonic_hints = true;
    let casm_contract =
        CasmContractClass::from_contract_class(contract_class, add_pythonic_hints).unwrap();

    compare_contents_or_fix_with_path(
        &get_example_file_path(
            format!("{example_file_name}.compiled_contract_class.json").as_str(),
        ),
        serde_json::to_string_pretty(&casm_contract).unwrap() + "\n",
    );
}
