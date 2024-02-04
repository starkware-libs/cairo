use cairo_lang_starknet_classes::allowed_libfuncs::ListSelector;
use cairo_lang_test_utils::compare_contents_or_fix_with_path;
use test_case::test_case;

use crate::test_utils::{get_example_file_path, get_test_contract};

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
    let example_file_name = example_contract_path.replace("::", "__");
    let list_selector = ListSelector::ListName("all".to_string());
    contract.validate_version_compatible(list_selector).unwrap();

    compare_contents_or_fix_with_path(
        &get_example_file_path(format!("{example_file_name}.contract_class.json").as_str()),
        serde_json::to_string_pretty(&contract).unwrap() + "\n",
    );

    let sierra_program = contract.extract_sierra_program().unwrap();

    // There is a separate file for the sierra code as it is hard to review inside the json.
    compare_contents_or_fix_with_path(
        &get_example_file_path(format!("{example_file_name}.sierra").as_str()),
        sierra_program.to_string(),
    );
}
