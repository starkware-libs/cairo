use cairo_lang_compiler::CompilerConfig;
use cairo_lang_lowering::utils::InliningStrategy;
use cairo_lang_starknet_classes::allowed_libfuncs::ListSelector;
use cairo_lang_test_utils::compare_contents_or_fix_with_path;
use test_case::test_case;

use crate::compile::compile_path;
use crate::test_utils::{get_example_file_path, get_test_contract};

/// Tests that the Sierra compiled from a contract in the contracts crate is the same as in
/// <test_case>.sierra, and that the resulting JSON is the same as in
/// <test_case>.contract_class.json.
#[test_case("account::account")]
#[test_case("circuit_contract::circuit_contract")]
#[test_case("test_contract::test_contract")]
#[test_case("new_syntax_test_contract::counter_contract")]
#[test_case("max_entrypoint::max_entrypoint_contract")]
#[test_case("minimal_contract::minimal_contract")]
#[test_case("hello_starknet::hello_starknet")]
#[test_case("libfuncs_coverage::libfuncs_coverage")]
#[test_case("erc20::erc_20")]
#[test_case("storage_accesses::storage_accesses")]
#[test_case("token_bridge::token_bridge")]
#[test_case("with_erc20::erc20_contract")]
#[test_case("with_ownable::ownable_balance")]
#[test_case("with_ownable_mini::ownable_mini_contract")]
#[test_case("with_erc20_mini::erc20_mini_contract")]
#[test_case("ownable_erc20::ownable_erc20_contract")]
#[test_case("proxy::proxy")]
#[test_case("upgradable_counter::counter_contract")]
#[test_case("mintable::mintable_erc20_ownable")]
#[test_case("multi_component::contract_with_4_components")]
fn test_compile_path_from_contracts_crate(example_contract_path: &str) {
    let contract = get_test_contract(
        format!("cairo_level_tests::contracts::{example_contract_path}").as_str(),
    );
    let example_file_name = example_contract_path.replace("::", "__");
    let list_selector = ListSelector::ListName("all".to_string());
    let extracted = contract.extract_sierra_program(true).unwrap();
    extracted.validate_version_compatible(list_selector).unwrap();

    compare_contents_or_fix_with_path(
        &get_example_file_path(format!("{example_file_name}.contract_class.json").as_str()),
        serde_json::to_string_pretty(&contract).unwrap() + "\n",
    );

    // There is a separate file for the sierra code as it is hard to review inside the json.
    compare_contents_or_fix_with_path(
        &get_example_file_path(format!("{example_file_name}.sierra").as_str()),
        extracted.program.to_string(),
    );
}

/// Tests the two-pass class-hash injection: the contract's external `get_class_hash` calls the
/// generated `__class_hash__::class_hash()`, making the reserved `__externally_provided_const__`
/// extern reachable. Compilation succeeds only because `compile_path` installs a provider for it
/// (computing the contract's own class hash); otherwise Sierra generation would fail. The result
/// is also deterministic across runs.
#[test]
fn class_hash_injection_compiles() {
    let path = get_example_file_path("class_hash_test_contract.cairo");
    let compile = || {
        compile_path(
            &path,
            None,
            CompilerConfig { replace_ids: true, ..Default::default() },
            InliningStrategy::default(),
        )
        .unwrap()
    };

    let contract = compile();
    assert_eq!(contract.entry_points_by_type.external.len(), 1);
    contract.sanity_check();

    // The injected class hash is deterministic, so recompilation yields an identical program.
    assert_eq!(contract.sierra_program, compile().sierra_program);
}

/// Tests STATIC interface forwarding via the compile-time class-hash feature: `static_proxy`
/// forwards `ICounterContract` to `counter_contract`'s class using the generated
/// `counter_contract::__class_hash__::ForwardingClassHashImpl` (no stored class hash, no
/// constructor). Verifies the proxy compiles to a class exposing the forwarded entry points; the
/// forwarded class hash is injected by the two-pass in `compile_path`.
#[test]
fn static_forwarding_compiles() {
    let path = get_example_file_path("static_forwarding_contract.cairo");
    let contract = compile_path(
        &path,
        Some("static_forwarding_contract::static_forwarding_contract::static_proxy"),
        CompilerConfig { replace_ids: true, ..Default::default() },
        InliningStrategy::default(),
    )
    .unwrap();

    // The two `ICounterContract` methods are forwarded as external entry points on the proxy.
    assert_eq!(contract.entry_points_by_type.external.len(), 2);
    contract.sanity_check();
}
