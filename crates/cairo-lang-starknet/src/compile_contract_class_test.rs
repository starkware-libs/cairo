use cairo_lang_test_utils::compare_contents_or_fix_with_path;
use pretty_assertions::assert_eq;
use test_case::test_case;

use crate::allowed_libfuncs::{validate_compatible_sierra_version, ListSelector};
use crate::compiler_version;
use crate::contract_class::ContractClass;
use crate::felt252_serde::sierra_from_felt252s;
use crate::test_utils::{
    get_contract_file_name_from_path, get_example_file_path, get_test_contract,
};

/// Tests that the sierra compiled from a contract in the contracts crate is the same as in
/// <test_case>.sierra, and that the resulted json is the same as in
/// <test_case>.contract_class.json.
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
fn test_compile_path_from_contracts_crate(example_contract_path: &str) {
    let contract = get_test_contract(
        format!("cairo_level_tests::contracts::{example_contract_path}").as_str(),
    );
    let example_file_name = get_contract_file_name_from_path(example_contract_path);
    test_compile_path_aux(&example_file_name, &contract);
}

// Helper function for the common parts `test_compile_path_*`.
fn test_compile_path_aux(example_file_name: &str, contract: &ContractClass) {
    let list_selector = ListSelector::ListName("all".to_string());
    validate_compatible_sierra_version(contract, list_selector).unwrap();

    compare_contents_or_fix_with_path(
        &get_example_file_path(format!("{example_file_name}.contract_class.json").as_str()),
        serde_json::to_string_pretty(contract).unwrap() + "\n",
    );

    let (sierra_version_id, compiler_version_id, mut sierra_program) =
        sierra_from_felt252s(&contract.sierra_program).unwrap();
    assert_eq!(
        sierra_version_id,
        compiler_version::current_sierra_version_id(),
        "Serialized Sierra version should be the current version."
    );
    assert_eq!(
        compiler_version_id,
        compiler_version::current_compiler_version_id(),
        "Serialized compiler version should be the current version."
    );
    contract.sierra_program_debug_info.clone().unwrap().populate(&mut sierra_program);

    // There is a separate file for the sierra code as it is hard to review inside the json.
    compare_contents_or_fix_with_path(
        &get_example_file_path(format!("{example_file_name}.sierra").as_str()),
        sierra_program.to_string(),
    );
}
