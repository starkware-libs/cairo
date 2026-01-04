use core::num::traits::Zero;
use perpetuals::core::components::assets::assets_manager::{
    IAssetsExternalDispatcher, IAssetsExternalDispatcherTrait,
};
use perpetuals::core::components::assets::interface::{IAssetsDispatcher, IAssetsDispatcherTrait};
use perpetuals::core::components::deposit::interface::{IDepositDispatcher, IDepositDispatcherTrait};
use perpetuals::core::components::positions::interface::{
    IPositionsDispatcher, IPositionsDispatcherTrait,
};
use perpetuals::core::interface::{ICoreDispatcher, ICoreDispatcherTrait};
use perpetuals::core::types::order::Order;
use perpetuals::tests::constants::*;
use perpetuals::tests::test_utils::{PerpetualsInitConfig, init_by_dispatcher};
use snforge_std::test_address;
use starknet::ContractAddress;
use starkware_utils::time::time::Time;
use starkware_utils_testing::test_utils::Deployable;
use crate::core::components::external_components::interface::{
    IExternalComponentsDispatcher, IExternalComponentsDispatcherTrait,
};

fn setup() -> (PerpetualsInitConfig, ContractAddress) {
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(@cfg, @token_state);
    (cfg, contract_address)
}

#[test]
#[should_panic(expected: "ONLY_OPERATOR")]
fn test_process_deposit_only_operator() {
    let (cfg, contract_address) = setup();
    let deposit_dispatcher = IDepositDispatcher { contract_address };
    deposit_dispatcher
        .process_deposit(
            operator_nonce: Zero::zero(),
            depositor: test_address(),
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: POSITION_ID_100,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: 0,
        );
}

#[test]
#[should_panic(expected: 'POSITION_DOESNT_EXIST')]
fn test_withdraw_request_position_doesnt_exist() {
    let (cfg, contract_address) = setup();
    let dispatcher = ICoreDispatcher { contract_address };
    dispatcher
        .withdraw_request(
            signature: array![].span(),
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: test_address(),
            position_id: POSITION_ID_100,
            amount: WITHDRAW_AMOUNT.into(),
            expiration: Time::now(),
            salt: 0,
        );
}

#[test]
#[should_panic(expected: "ONLY_OPERATOR")]
fn test_withdraw_only_operator() {
    let (cfg, contract_address) = setup();
    let dispatcher = ICoreDispatcher { contract_address };
    dispatcher
        .withdraw(
            operator_nonce: Zero::zero(),
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: test_address(),
            position_id: POSITION_ID_100,
            amount: WITHDRAW_AMOUNT.into(),
            expiration: Time::now(),
            salt: 0,
        );
}

#[test]
#[should_panic(expected: 'POSITION_DOESNT_EXIST')]
fn test_transfer_request_position_doesnt_exist() {
    let (cfg, contract_address) = setup();
    let dispatcher = ICoreDispatcher { contract_address };
    dispatcher
        .transfer_request(
            signature: array![].span(),
            asset_id: cfg.collateral_cfg.collateral_id,
            recipient: POSITION_ID_100,
            position_id: POSITION_ID_200,
            amount: TRANSFER_AMOUNT.into(),
            expiration: Time::now(),
            salt: 0,
        );
}

#[test]
#[should_panic(expected: "ONLY_OPERATOR")]
fn test_transfer_only_operator() {
    let (cfg, contract_address) = setup();
    let dispatcher = ICoreDispatcher { contract_address };
    dispatcher
        .transfer(
            operator_nonce: Zero::zero(),
            asset_id: cfg.collateral_cfg.collateral_id,
            recipient: POSITION_ID_100,
            position_id: POSITION_ID_200,
            amount: TRANSFER_AMOUNT.into(),
            expiration: Time::now(),
            salt: 0,
        );
}

#[test]
#[should_panic(expected: "ONLY_OPERATOR")]
fn test_trade_only_operator() {
    let (cfg, contract_address) = setup();
    let dispatcher = ICoreDispatcher { contract_address };
    let default_order = Order {
        position_id: POSITION_ID_100,
        base_asset_id: cfg.collateral_cfg.collateral_id,
        base_amount: 0,
        quote_asset_id: cfg.collateral_cfg.collateral_id,
        quote_amount: 0,
        fee_asset_id: cfg.collateral_cfg.collateral_id,
        fee_amount: 0,
        expiration: Time::now(),
        salt: 0,
    };
    dispatcher
        .trade(
            operator_nonce: Zero::zero(),
            signature_a: array![].span(),
            signature_b: array![].span(),
            order_a: default_order,
            order_b: default_order,
            actual_amount_base_a: 0,
            actual_amount_quote_a: 0,
            actual_fee_a: 0,
            actual_fee_b: 0,
        );
}

#[test]
#[should_panic(expected: "ONLY_OPERATOR")]
fn test_liquidate_only_operator() {
    let (cfg, contract_address) = setup();
    let dispatcher = ICoreDispatcher { contract_address };
    let default_order = Order {
        position_id: POSITION_ID_100,
        base_asset_id: cfg.collateral_cfg.collateral_id,
        base_amount: 0,
        quote_asset_id: cfg.collateral_cfg.collateral_id,
        quote_amount: 0,
        fee_asset_id: cfg.collateral_cfg.collateral_id,
        fee_amount: 0,
        expiration: Time::now(),
        salt: 0,
    };
    dispatcher
        .liquidate(
            operator_nonce: Zero::zero(),
            liquidator_signature: array![].span(),
            liquidated_position_id: POSITION_ID_100,
            liquidator_order: default_order,
            actual_amount_base_liquidated: 0,
            actual_amount_quote_liquidated: 0,
            actual_liquidator_fee: 0,
            liquidated_fee_amount: 0,
        );
}

#[test]
#[should_panic(expected: "ONLY_OPERATOR")]
fn test_deleverage_only_operator() {
    let (cfg, contract_address) = setup();
    let dispatcher = ICoreDispatcher { contract_address };
    dispatcher
        .deleverage(
            operator_nonce: Zero::zero(),
            deleveraged_position_id: POSITION_ID_100,
            deleverager_position_id: POSITION_ID_100,
            base_asset_id: cfg.collateral_cfg.collateral_id,
            deleveraged_base_amount: 0,
            deleveraged_quote_amount: 0,
        );
}

#[test]
#[should_panic(expected: "ONLY_APP_GOVERNOR")]
fn test_add_oracle_to_asset_only_app_governor() {
    let (cfg, contract_address) = setup();
    let dispatcher = IAssetsExternalDispatcher { contract_address };
    dispatcher
        .add_oracle_to_asset(
            asset_id: cfg.synthetic_cfg.synthetic_id,
            oracle_public_key: Zero::zero(),
            oracle_name: Zero::zero(),
            asset_name: Zero::zero(),
        );
}

#[test]
#[should_panic(expected: "ONLY_APP_GOVERNOR")]
fn test_add_synthetic_asset_only_app_governor() {
    let (cfg, contract_address) = setup();
    let dispatcher = IAssetsExternalDispatcher { contract_address };
    dispatcher
        .add_synthetic_asset(
            asset_id: cfg.synthetic_cfg.synthetic_id,
            risk_factor_tiers: array![].span(),
            risk_factor_first_tier_boundary: Zero::zero(),
            risk_factor_tier_size: Zero::zero(),
            quorum: 0,
            resolution_factor: 0,
        );
}

#[test]
#[should_panic(expected: "ONLY_APP_GOVERNOR")]
fn test_deactivate_synthetic_only_app_governor() {
    let (cfg, contract_address) = setup();
    let dispatcher = IAssetsExternalDispatcher { contract_address };
    dispatcher.deactivate_synthetic(synthetic_id: cfg.synthetic_cfg.synthetic_id);
}

#[test]
#[should_panic(expected: "ONLY_OPERATOR")]
fn test_funding_tick_only_operator() {
    let (_, contract_address) = setup();
    let dispatcher = IAssetsDispatcher { contract_address };
    dispatcher
        .funding_tick(
            operator_nonce: Zero::zero(), funding_ticks: array![].span(), timestamp: Time::now(),
        );
}

#[test]
#[should_panic(expected: "ONLY_OPERATOR")]
fn test_price_tick_only_operator() {
    let (cfg, contract_address) = setup();
    let dispatcher = IAssetsDispatcher { contract_address };
    dispatcher
        .price_tick(
            operator_nonce: Zero::zero(),
            asset_id: cfg.synthetic_cfg.synthetic_id,
            oracle_price: Zero::zero(),
            signed_prices: array![].span(),
        );
}

#[test]
#[should_panic(expected: "ONLY_OPERATOR")]
fn test_new_position_only_operator() {
    let (_, contract_address) = setup();
    let dispatcher = IPositionsDispatcher { contract_address };
    dispatcher
        .new_position(
            operator_nonce: Zero::zero(),
            position_id: POSITION_ID_100,
            owner_public_key: Zero::zero(),
            owner_account: Zero::zero(),
            owner_protection_enabled: true,
        );
}

#[test]
#[should_panic(expected: "ONLY_OPERATOR")]
fn test_set_owner_account_only_operator() {
    let (_, contract_address) = setup();
    let dispatcher = IPositionsDispatcher { contract_address };
    dispatcher
        .set_owner_account(
            operator_nonce: Zero::zero(),
            position_id: POSITION_ID_100,
            new_owner_account: Zero::zero(),
            expiration: Time::now(),
        );
}

#[test]
#[should_panic(expected: ('POSITION_DOESNT_EXIST',))]
fn test_set_public_key_request_position_doesnt_exist() {
    let (_, contract_address) = setup();
    let dispatcher = IPositionsDispatcher { contract_address };
    dispatcher
        .set_public_key_request(
            signature: array![].span(),
            position_id: POSITION_ID_100,
            new_public_key: Zero::zero(),
            expiration: Time::now(),
        );
}

#[test]
#[should_panic(expected: "ONLY_OPERATOR")]
fn test_set_public_key_only_operator() {
    let (_, contract_address) = setup();
    let dispatcher = IPositionsDispatcher { contract_address };
    dispatcher
        .set_public_key(
            operator_nonce: Zero::zero(),
            position_id: POSITION_ID_100,
            new_public_key: Zero::zero(),
            expiration: Time::now(),
        );
}

#[test]
#[should_panic(expected: "ONLY_UPGRADE_GOVERNOR")]
fn test_register_new_component_only_upgrade_governor() {
    let (_, contract_address) = setup();
    let external_components_dispatcher = IExternalComponentsDispatcher {
        contract_address: contract_address,
    };
    external_components_dispatcher
        .register_external_component(
            component_type: 'TRANSFERS', component_address: 'SOME_HASH'.try_into().unwrap(),
        );
}

#[test]
#[should_panic(expected: "ONLY_UPGRADE_GOVERNOR")]
fn test_activate_new_component_only_upgrade_governor() {
    let (_, contract_address) = setup();
    let external_components_dispatcher = IExternalComponentsDispatcher {
        contract_address: contract_address,
    };
    external_components_dispatcher
        .activate_external_component(
            component_type: 'TRANSFERS', component_address: 'SOME_HASH'.try_into().unwrap(),
        );
}
