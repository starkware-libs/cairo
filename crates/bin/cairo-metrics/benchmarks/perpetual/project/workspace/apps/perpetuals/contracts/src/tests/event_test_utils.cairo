use perpetuals::core::components::assets::events as assets_events;
use perpetuals::core::components::deposit::events as deposit_events;
use perpetuals::core::components::positions::events as positions_events;
use perpetuals::core::components::snip::SNIP12MetadataImpl;
use perpetuals::core::components::withdrawal::withdrawal_manager::{
    ForcedWithdraw, ForcedWithdrawRequest,
};
use perpetuals::core::events;
use perpetuals::core::types::asset::AssetId;
use perpetuals::core::types::funding::FundingIndex;
use perpetuals::core::types::position::PositionId;
use perpetuals::core::types::price::Price;
use perpetuals::tests::constants::*;
use snforge_std::cheatcodes::events::Event;
use snforge_std::signature::stark_curve::StarkCurveSignerImpl;
use starknet::ContractAddress;
use starkware_utils::signature::stark::PublicKey;
use starkware_utils::time::time::Timestamp;
use starkware_utils_testing::test_utils::assert_expected_event_emitted;
use crate::core::components::transfer::transfer_manager::{Transfer, TransferRequest};


pub fn assert_new_position_event_with_expected(
    spied_event: @(ContractAddress, Event),
    position_id: PositionId,
    owner_public_key: PublicKey,
    owner_account: ContractAddress,
) {
    let expected_event = positions_events::NewPosition {
        position_id, owner_public_key, owner_account,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("NewPosition"),
        expected_event_name: "NewPosition",
    );
}

pub fn assert_deposit_event_with_expected(
    spied_event: @(ContractAddress, Event),
    position_id: PositionId,
    depositing_address: ContractAddress,
    collateral_id: AssetId,
    quantized_amount: u64,
    unquantized_amount: u64,
    deposit_request_hash: felt252,
    salt: felt252,
) {
    let expected_event = deposit_events::Deposit {
        position_id,
        depositing_address,
        collateral_id,
        quantized_amount,
        unquantized_amount,
        deposit_request_hash,
        salt,
    };

    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("Deposit"),
        expected_event_name: "Deposit",
    );
}

pub fn assert_deposit_canceled_event_with_expected(
    spied_event: @(ContractAddress, Event),
    position_id: PositionId,
    depositing_address: ContractAddress,
    collateral_id: AssetId,
    quantized_amount: u64,
    unquantized_amount: u64,
    deposit_request_hash: felt252,
    salt: felt252,
) {
    let expected_event = deposit_events::DepositCanceled {
        position_id,
        depositing_address,
        collateral_id,
        quantized_amount,
        unquantized_amount: unquantized_amount.into(),
        deposit_request_hash,
        salt,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("DepositCanceled"),
        expected_event_name: "DepositCanceled",
    );
}

pub fn assert_deposit_processed_event_with_expected(
    spied_event: @(ContractAddress, Event),
    position_id: PositionId,
    depositing_address: ContractAddress,
    collateral_id: AssetId,
    quantized_amount: u64,
    unquantized_amount: u64,
    deposit_request_hash: felt252,
    salt: felt252,
) {
    let expected_event = deposit_events::DepositProcessed {
        position_id,
        depositing_address,
        collateral_id,
        quantized_amount,
        unquantized_amount: unquantized_amount.into(),
        deposit_request_hash,
        salt,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("DepositProcessed"),
        expected_event_name: "DepositProcessed",
    );
}

pub fn assert_withdraw_request_event_with_expected(
    spied_event: @(ContractAddress, Event),
    position_id: PositionId,
    recipient: ContractAddress,
    collateral_id: AssetId,
    amount: u64,
    expiration: Timestamp,
    withdraw_request_hash: felt252,
    salt: felt252,
) {
    let expected_event = events::WithdrawRequest {
        position_id, recipient, collateral_id, amount, expiration, withdraw_request_hash, salt,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("WithdrawRequest"),
        expected_event_name: "WithdrawRequest",
    );
}

pub fn assert_withdraw_event_with_expected(
    spied_event: @(ContractAddress, Event),
    position_id: PositionId,
    recipient: ContractAddress,
    collateral_id: AssetId,
    amount: u64,
    expiration: Timestamp,
    withdraw_request_hash: felt252,
    salt: felt252,
) {
    let expected_event = events::Withdraw {
        position_id, recipient, collateral_id, amount, expiration, withdraw_request_hash, salt,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("Withdraw"),
        expected_event_name: "Withdraw",
    );
}

pub fn assert_forced_withdraw_request_event_with_expected(
    spied_event: @(ContractAddress, Event),
    position_id: PositionId,
    recipient: ContractAddress,
    collateral_id: AssetId,
    amount: u64,
    expiration: Timestamp,
    forced_withdraw_request_hash: felt252,
    salt: felt252,
) {
    let expected_event = ForcedWithdrawRequest {
        position_id,
        recipient,
        collateral_id,
        amount,
        expiration,
        forced_withdraw_request_hash,
        salt,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("ForcedWithdrawRequest"),
        expected_event_name: "ForcedWithdrawRequest",
    );
}

pub fn assert_forced_withdraw_event_with_expected(
    spied_event: @(ContractAddress, Event),
    position_id: PositionId,
    recipient: ContractAddress,
    collateral_id: AssetId,
    amount: u64,
    expiration: Timestamp,
    forced_withdraw_request_hash: felt252,
    salt: felt252,
) {
    let expected_event = ForcedWithdraw {
        position_id,
        recipient,
        collateral_id,
        amount,
        expiration,
        forced_withdraw_request_hash,
        salt,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("ForcedWithdraw"),
        expected_event_name: "ForcedWithdraw",
    );
}

pub fn assert_trade_event_with_expected(
    spied_event: @(ContractAddress, Event),
    order_base_asset_id: AssetId,
    order_a_position_id: PositionId,
    order_a_base_amount: i64,
    order_a_quote_amount: i64,
    fee_a_amount: u64,
    collateral_id: AssetId,
    order_b_position_id: PositionId,
    order_b_base_amount: i64,
    order_b_quote_amount: i64,
    fee_b_amount: u64,
    actual_amount_base_a: i64,
    actual_amount_quote_a: i64,
    actual_fee_a: u64,
    actual_fee_b: u64,
    order_a_hash: felt252,
    order_b_hash: felt252,
) {
    let expected_event = events::Trade {
        order_a_position_id,
        order_a_base_asset_id: order_base_asset_id,
        order_a_base_amount,
        order_a_quote_asset_id: collateral_id,
        order_a_quote_amount,
        fee_a_asset_id: collateral_id,
        fee_a_amount,
        order_b_position_id,
        order_b_base_asset_id: order_base_asset_id,
        order_b_base_amount,
        order_b_quote_asset_id: collateral_id,
        order_b_quote_amount,
        fee_b_asset_id: collateral_id,
        fee_b_amount,
        actual_amount_base_a,
        actual_amount_quote_a,
        actual_fee_a,
        actual_fee_b,
        order_a_hash,
        order_b_hash,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("Trade"),
        expected_event_name: "Trade",
    );
}

pub fn assert_liquidate_event_with_expected(
    spied_event: @(ContractAddress, Event),
    liquidated_position_id: PositionId,
    liquidator_order_position_id: PositionId,
    liquidator_order_base_asset_id: AssetId,
    liquidator_order_base_amount: i64,
    collateral_id: AssetId,
    liquidator_order_quote_amount: i64,
    liquidator_order_fee_amount: u64,
    actual_amount_base_liquidated: i64,
    actual_amount_quote_liquidated: i64,
    actual_liquidator_fee: u64,
    insurance_fund_fee_amount: u64,
    liquidator_order_hash: felt252,
) {
    let expected_event = events::Liquidate {
        liquidated_position_id,
        liquidator_order_position_id,
        liquidator_order_base_asset_id,
        liquidator_order_base_amount,
        liquidator_order_quote_asset_id: collateral_id,
        liquidator_order_quote_amount,
        liquidator_order_fee_asset_id: collateral_id,
        liquidator_order_fee_amount,
        actual_amount_base_liquidated,
        actual_amount_quote_liquidated,
        actual_liquidator_fee,
        insurance_fund_fee_asset_id: collateral_id,
        insurance_fund_fee_amount,
        liquidator_order_hash,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("Liquidate"),
        expected_event_name: "Liquidate",
    );
}

pub fn assert_deleverage_event_with_expected(
    spied_event: @(ContractAddress, Event),
    deleveraged_position_id: PositionId,
    deleverager_position_id: PositionId,
    base_asset_id: AssetId,
    collateral_id: AssetId,
    deleveraged_base_amount: i64,
    deleveraged_quote_amount: i64,
) {
    let expected_event = events::Deleverage {
        deleveraged_position_id,
        deleverager_position_id,
        base_asset_id,
        deleveraged_base_amount,
        quote_asset_id: collateral_id,
        deleveraged_quote_amount,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("Deleverage"),
        expected_event_name: "Deleverage",
    );
}

pub fn assert_transfer_request_event_with_expected(
    spied_event: @(ContractAddress, Event),
    position_id: PositionId,
    recipient: PositionId,
    collateral_id: AssetId,
    amount: u64,
    expiration: Timestamp,
    transfer_request_hash: felt252,
    salt: felt252,
) {
    let expected_event = TransferRequest {
        position_id, recipient, collateral_id, amount, expiration, transfer_request_hash, salt,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("TransferRequest"),
        expected_event_name: "TransferRequest",
    );
}

pub fn assert_transfer_event_with_expected(
    spied_event: @(ContractAddress, Event),
    position_id: PositionId,
    recipient: PositionId,
    collateral_id: AssetId,
    amount: u64,
    expiration: Timestamp,
    transfer_request_hash: felt252,
    salt: felt252,
) {
    let expected_event = Transfer {
        position_id, recipient, collateral_id, amount, expiration, transfer_request_hash, salt,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("Transfer"),
        expected_event_name: "Transfer",
    );
}

pub fn assert_set_owner_account_event_with_expected(
    spied_event: @(ContractAddress, Event),
    position_id: PositionId,
    public_key: PublicKey,
    new_owner_account: ContractAddress,
    set_owner_account_hash: felt252,
) {
    let expected_event = positions_events::SetOwnerAccount {
        position_id, public_key, new_owner_account, set_owner_account_hash,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("SetOwnerAccount"),
        expected_event_name: "SetOwnerAccount",
    );
}


pub fn assert_set_public_key_request_event_with_expected(
    spied_event: @(ContractAddress, Event),
    position_id: PositionId,
    old_public_key: PublicKey,
    new_public_key: PublicKey,
    expiration: Timestamp,
    set_public_key_request_hash: felt252,
) {
    let expected_event = positions_events::SetPublicKeyRequest {
        position_id, old_public_key, new_public_key, expiration, set_public_key_request_hash,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("SetPublicKeyRequest"),
        expected_event_name: "SetPublicKeyRequest",
    );
}

pub fn assert_set_public_key_event_with_expected(
    spied_event: @(ContractAddress, Event),
    position_id: PositionId,
    old_public_key: PublicKey,
    new_public_key: PublicKey,
    set_public_key_request_hash: felt252,
) {
    let expected_event = positions_events::SetPublicKey {
        position_id, old_public_key, new_public_key, set_public_key_request_hash,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("SetPublicKey"),
        expected_event_name: "SetPublicKey",
    );
}

pub fn assert_funding_tick_event_with_expected(
    spied_event: @(ContractAddress, Event), asset_id: AssetId, funding_index: FundingIndex,
) {
    let expected_event = assets_events::FundingTick { asset_id, funding_index };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("FundingTick"),
        expected_event_name: "FundingTick",
    );
}

pub fn assert_price_tick_event_with_expected(
    spied_event: @(ContractAddress, Event), asset_id: AssetId, price: Price,
) {
    let expected_event = assets_events::PriceTick { asset_id, price };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("PriceTick"),
        expected_event_name: "PriceTick",
    );
}

pub fn assert_asset_activated_event_with_expected(
    spied_event: @(ContractAddress, Event), asset_id: AssetId,
) {
    let expected_event = assets_events::AssetActivated { asset_id };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("AssetActivated"),
        expected_event_name: "AssetActivated",
    );
}

pub fn assert_add_synthetic_event_with_expected(
    spied_event: @(ContractAddress, Event),
    asset_id: AssetId,
    risk_factor_tiers: Span<u16>,
    risk_factor_first_tier_boundary: u128,
    risk_factor_tier_size: u128,
    resolution_factor: u64,
    quorum: u8,
) {
    let expected_event = assets_events::SyntheticAdded {
        asset_id,
        risk_factor_tiers,
        risk_factor_first_tier_boundary,
        risk_factor_tier_size,
        resolution_factor,
        quorum,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("SyntheticAdded"),
        expected_event_name: "SyntheticAdded",
    );
}

pub fn assert_change_synthetic_event_with_expected(
    spied_event: @(ContractAddress, Event),
    asset_id: AssetId,
    risk_factor_tiers: Span<u16>,
    risk_factor_first_tier_boundary: u128,
    risk_factor_tier_size: u128,
    resolution_factor: u64,
    quorum: u8,
) {
    let expected_event = assets_events::SyntheticChanged {
        asset_id,
        risk_factor_tiers,
        risk_factor_first_tier_boundary,
        risk_factor_tier_size,
        resolution_factor,
        quorum,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("SyntheticChanged"),
        expected_event_name: "SyntheticChanged",
    );
}

pub fn assert_add_spot_event_with_expected(
    spied_event: @(ContractAddress, Event),
    asset_id: AssetId,
    risk_factor_tiers: Span<u16>,
    risk_factor_first_tier_boundary: u128,
    risk_factor_tier_size: u128,
    resolution_factor: u64,
    quorum: u8,
    contract_address: ContractAddress,
    quantum: u64,
) {
    let expected_event = assets_events::SpotAssetAdded {
        asset_id,
        risk_factor_tiers,
        risk_factor_first_tier_boundary,
        risk_factor_tier_size,
        resolution_factor,
        quorum,
        contract_address,
        quantum,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("SpotAssetAdded"),
        expected_event_name: "SpotAssetAdded",
    );
}

pub fn assert_deactivate_synthetic_asset_event_with_expected(
    spied_event: @(ContractAddress, Event), asset_id: AssetId,
) {
    let expected_event = assets_events::SyntheticAssetDeactivated { asset_id };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("SyntheticAssetDeactivated"),
        expected_event_name: "SyntheticAssetDeactivated",
    );
}


pub fn assert_remove_oracle_event_with_expected(
    spied_event: @(ContractAddress, Event), asset_id: AssetId, oracle_public_key: PublicKey,
) {
    let expected_event = assets_events::OracleRemoved { asset_id, oracle_public_key };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("OracleRemoved"),
        expected_event_name: "OracleRemoved",
    );
}

pub fn assert_add_oracle_event_with_expected(
    spied_event: @(ContractAddress, Event),
    asset_id: AssetId,
    asset_name: felt252,
    oracle_public_key: PublicKey,
    oracle_name: felt252,
) {
    let expected_event = assets_events::OracleAdded {
        asset_id, asset_name, oracle_public_key, oracle_name,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("OracleAdded"),
        expected_event_name: "OracleAdded",
    );
}

pub fn assert_update_asset_quorum_event_with_expected(
    spied_event: @(ContractAddress, Event), asset_id: AssetId, new_quorum: u8, old_quorum: u8,
) {
    let expected_event = assets_events::AssetQuorumUpdated { asset_id, new_quorum, old_quorum };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("AssetQuorumUpdated"),
        expected_event_name: "AssetQuorumUpdated",
    );
}

pub fn assert_forced_trade_request_event_with_expected(
    spied_event: @(ContractAddress, Event),
    order_a_position_id: PositionId,
    order_a_base_asset_id: AssetId,
    order_a_base_amount: i64,
    order_a_quote_asset_id: AssetId,
    order_a_quote_amount: i64,
    fee_a_asset_id: AssetId,
    fee_a_amount: u64,
    order_b_position_id: PositionId,
    order_b_base_asset_id: AssetId,
    order_b_base_amount: i64,
    order_b_quote_asset_id: AssetId,
    order_b_quote_amount: i64,
    fee_b_asset_id: AssetId,
    fee_b_amount: u64,
    order_a_hash: felt252,
    order_b_hash: felt252,
) {
    let expected_event = events::ForcedTradeRequest {
        order_a_position_id,
        order_a_base_asset_id,
        order_a_base_amount,
        order_a_quote_asset_id,
        order_a_quote_amount,
        fee_a_asset_id,
        fee_a_amount,
        order_b_position_id,
        order_b_base_asset_id,
        order_b_base_amount,
        order_b_quote_asset_id,
        order_b_quote_amount,
        fee_b_asset_id,
        fee_b_amount,
        order_a_hash,
        order_b_hash,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("ForcedTradeRequest"),
        expected_event_name: "ForcedTradeRequest",
    );
}

pub fn assert_forced_trade_event_with_expected(
    spied_event: @(ContractAddress, Event),
    order_a_position_id: PositionId,
    order_a_base_asset_id: AssetId,
    order_a_base_amount: i64,
    order_a_quote_asset_id: AssetId,
    order_a_quote_amount: i64,
    fee_a_asset_id: AssetId,
    fee_a_amount: u64,
    order_b_position_id: PositionId,
    order_b_base_asset_id: AssetId,
    order_b_base_amount: i64,
    order_b_quote_asset_id: AssetId,
    order_b_quote_amount: i64,
    fee_b_asset_id: AssetId,
    fee_b_amount: u64,
    actual_amount_base_a: i64,
    actual_amount_quote_a: i64,
    order_a_hash: felt252,
    order_b_hash: felt252,
) {
    let expected_event = events::ForcedTrade {
        order_a_position_id,
        order_a_base_asset_id,
        order_a_base_amount,
        order_a_quote_asset_id,
        order_a_quote_amount,
        fee_a_asset_id,
        fee_a_amount,
        order_b_position_id,
        order_b_base_asset_id,
        order_b_base_amount,
        order_b_quote_asset_id,
        order_b_quote_amount,
        fee_b_asset_id,
        fee_b_amount,
        actual_amount_base_a,
        actual_amount_quote_a,
        order_a_hash,
        order_b_hash,
    };
    assert_expected_event_emitted(
        :spied_event,
        :expected_event,
        expected_event_selector: @selector!("ForcedTrade"),
        expected_event_name: "ForcedTrade",
    );
}
