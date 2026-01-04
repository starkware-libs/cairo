use perpetuals::tests::constants::*;
use perpetuals::tests::test_utils::{PerpetualsInitConfig, init_by_dispatcher};
use snforge_std::cheatcodes::contract_class::DeclareResultTrait;
use snforge_std::start_cheat_block_timestamp_global;
use starknet::ContractAddress;
use starkware_utils::time::time::{Time, Timestamp};
use starkware_utils_testing::test_utils::{
    Deployable, assert_panic_with_error, cheat_caller_address_once,
};
use crate::core::components::external_components::interface::{
    IExternalComponentsDispatcher, IExternalComponentsDispatcherTrait,
    IExternalComponentsSafeDispatcher, IExternalComponentsSafeDispatcherTrait,
};
use crate::core::interface::{ICoreDispatcher, ICoreDispatcherTrait};


fn setup() -> (PerpetualsInitConfig, ContractAddress) {
    let cfg: PerpetualsInitConfig = PerpetualsInitConfig {
        upgrade_delay: 100, ..Default::default(),
    };
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(@cfg, @token_state);
    (cfg, contract_address)
}

#[test]
#[feature("safe_dispatcher")]
fn test_rejects_mismatch_in_component_names() {
    let (cfg, contract_address) = setup();
    let invalid_named_component = snforge_std::declare("MockInvalidExternalComponent")
        .unwrap()
        .contract_class();
    let external_components_dispatcher = IExternalComponentsSafeDispatcher {
        contract_address: contract_address,
    };
    cheat_caller_address_once(
        contract_address: contract_address, caller_address: cfg.governance_admin,
    );
    let result = external_components_dispatcher
        .register_external_component(
            component_type: 'TRANSFERS', component_address: *invalid_named_component.class_hash,
        );

    assert_panic_with_error(
        :result,
        expected_error: format!(
            "Component type mismatch: declared {}, expected {}", 'BAD_NAME', 'TRANSFERS',
        ),
    );
}

#[test]
#[feature("safe_dispatcher")]
fn test_should_reject_activation_of_different_hash() {
    let (cfg, contract_address) = setup();
    let invalid_named_component = snforge_std::declare("MockInvalidExternalComponent")
        .unwrap()
        .contract_class();
    let valid_named_component = snforge_std::declare("MockValidExternalComponent")
        .unwrap()
        .contract_class();
    let external_components_dispatcher = IExternalComponentsSafeDispatcher {
        contract_address: contract_address,
    };
    cheat_caller_address_once(
        contract_address: contract_address, caller_address: cfg.governance_admin,
    );
    external_components_dispatcher
        .register_external_component(
            component_type: 'TRANSFERS', component_address: *valid_named_component.class_hash,
        )
        .unwrap();

    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::weeks(2)).into(),
    );

    cheat_caller_address_once(
        contract_address: contract_address, caller_address: cfg.governance_admin,
    );
    let result = external_components_dispatcher
        .activate_external_component(
            component_type: 'TRANSFERS', component_address: *invalid_named_component.class_hash,
        );

    assert_panic_with_error(
        :result,
        expected_error: format!(
            "{} not registered with hash {:?}", 'TRANSFERS', *invalid_named_component.class_hash,
        ),
    );
}


#[test]
#[feature("safe_dispatcher")]
fn test_should_reject_activation_of_component_without_valid_registration() {
    let (cfg, contract_address) = setup();
    let valid_named_component = snforge_std::declare("MockValidExternalComponent")
        .unwrap()
        .contract_class();
    let external_components_dispatcher = IExternalComponentsSafeDispatcher {
        contract_address: contract_address,
    };
    cheat_caller_address_once(
        contract_address: contract_address, caller_address: cfg.governance_admin,
    );
    let result = external_components_dispatcher
        .activate_external_component(
            component_type: 'TRANSFERS', component_address: *valid_named_component.class_hash,
        );

    assert_panic_with_error(
        :result,
        expected_error: format!(
            "{} not registered with hash {:?}", 'TRANSFERS', *valid_named_component.class_hash,
        ),
    );
}

#[test]
#[should_panic(expected: "Activation time not reached")]
fn test_should_reject_activation_if_not_enough_time_passed() {
    let (cfg, contract_address) = setup();
    let valid_named_component = snforge_std::declare("MockValidExternalComponent")
        .unwrap()
        .contract_class();
    let external_components_dispatcher = IExternalComponentsDispatcher {
        contract_address: contract_address,
    };
    cheat_caller_address_once(
        contract_address: contract_address, caller_address: cfg.governance_admin,
    );

    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::seconds(0)).into(),
    );

    external_components_dispatcher
        .register_external_component(
            component_type: 'TRANSFERS', component_address: *valid_named_component.class_hash,
        );

    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::seconds(cfg.upgrade_delay - 1)).into(),
    );

    cheat_caller_address_once(
        contract_address: contract_address, caller_address: cfg.governance_admin,
    );
    external_components_dispatcher
        .activate_external_component(
            component_type: 'TRANSFERS', component_address: *valid_named_component.class_hash,
        );
}


#[test]
#[should_panic(expected: 'MOCK_TRANSFER')]
fn test_should_activate_registered_component() {
    let (cfg, contract_address) = setup();
    let valid_named_component = snforge_std::declare("MockValidExternalComponent")
        .unwrap()
        .contract_class();
    let external_components_dispatcher = IExternalComponentsDispatcher {
        contract_address: contract_address,
    };
    cheat_caller_address_once(
        contract_address: contract_address, caller_address: cfg.governance_admin,
    );

    external_components_dispatcher
        .register_external_component(
            component_type: 'TRANSFERS', component_address: *valid_named_component.class_hash,
        );

    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::seconds(cfg.upgrade_delay + 1)).into(),
    );

    cheat_caller_address_once(
        contract_address: contract_address, caller_address: cfg.governance_admin,
    );
    external_components_dispatcher
        .activate_external_component(
            component_type: 'TRANSFERS', component_address: *valid_named_component.class_hash,
        );

    cheat_caller_address_once(contract_address: contract_address, caller_address: cfg.operator);
    ICoreDispatcher { contract_address: contract_address }
        .transfer(
            operator_nonce: 0,
            asset_id: 1.into(),
            recipient: 1_u32.into(),
            position_id: 1_u32.into(),
            amount: 100,
            expiration: Timestamp { seconds: 0 },
            salt: 0,
        )
}
