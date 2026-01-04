use perpetuals::core::types::asset::AssetId;
use perpetuals::core::types::position::PositionId;
use starknet::ContractAddress;
use starkware_utils::time::time::Timestamp;

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct WithdrawRequest {
    #[key]
    pub position_id: PositionId,
    #[key]
    pub recipient: ContractAddress,
    pub collateral_id: AssetId,
    pub amount: u64,
    pub expiration: Timestamp,
    #[key]
    pub withdraw_request_hash: felt252,
    pub salt: felt252,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct Withdraw {
    #[key]
    pub position_id: PositionId,
    #[key]
    pub recipient: ContractAddress,
    pub collateral_id: AssetId,
    pub amount: u64,
    pub expiration: Timestamp,
    #[key]
    pub withdraw_request_hash: felt252,
    pub salt: felt252,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct Trade {
    #[key]
    pub order_a_position_id: PositionId,
    pub order_a_base_asset_id: AssetId,
    pub order_a_base_amount: i64,
    pub order_a_quote_asset_id: AssetId,
    pub order_a_quote_amount: i64,
    pub fee_a_asset_id: AssetId,
    pub fee_a_amount: u64,
    #[key]
    pub order_b_position_id: PositionId,
    pub order_b_base_asset_id: AssetId,
    pub order_b_base_amount: i64,
    pub order_b_quote_asset_id: AssetId,
    pub order_b_quote_amount: i64,
    pub fee_b_asset_id: AssetId,
    pub fee_b_amount: u64,
    pub actual_amount_base_a: i64,
    pub actual_amount_quote_a: i64,
    pub actual_fee_a: u64,
    pub actual_fee_b: u64,
    #[key]
    pub order_a_hash: felt252,
    #[key]
    pub order_b_hash: felt252,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct Liquidate {
    #[key]
    pub liquidated_position_id: PositionId,
    #[key]
    pub liquidator_order_position_id: PositionId,
    pub liquidator_order_base_asset_id: AssetId,
    pub liquidator_order_base_amount: i64,
    pub liquidator_order_quote_asset_id: AssetId,
    pub liquidator_order_quote_amount: i64,
    pub liquidator_order_fee_asset_id: AssetId,
    pub liquidator_order_fee_amount: u64,
    pub actual_amount_base_liquidated: i64,
    pub actual_amount_quote_liquidated: i64,
    pub actual_liquidator_fee: u64,
    pub insurance_fund_fee_asset_id: AssetId,
    pub insurance_fund_fee_amount: u64,
    #[key]
    pub liquidator_order_hash: felt252,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct AssetPositionReduced {
    #[key]
    pub position_id_a: PositionId,
    #[key]
    pub position_id_b: PositionId,
    pub base_asset_id: AssetId,
    pub base_amount_a: i64,
    pub quote_asset_id: AssetId,
    pub quote_amount_a: i64,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct Deleverage {
    #[key]
    pub deleveraged_position_id: PositionId,
    #[key]
    pub deleverager_position_id: PositionId,
    pub base_asset_id: AssetId,
    pub deleveraged_base_amount: i64,
    pub quote_asset_id: AssetId,
    pub deleveraged_quote_amount: i64,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct ForcedTradeRequest {
    #[key]
    pub order_a_position_id: PositionId,
    pub order_a_base_asset_id: AssetId,
    pub order_a_base_amount: i64,
    pub order_a_quote_asset_id: AssetId,
    pub order_a_quote_amount: i64,
    pub fee_a_asset_id: AssetId,
    pub fee_a_amount: u64,
    #[key]
    pub order_b_position_id: PositionId,
    pub order_b_base_asset_id: AssetId,
    pub order_b_base_amount: i64,
    pub order_b_quote_asset_id: AssetId,
    pub order_b_quote_amount: i64,
    pub fee_b_asset_id: AssetId,
    pub fee_b_amount: u64,
    #[key]
    pub order_a_hash: felt252,
    #[key]
    pub order_b_hash: felt252,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct ForcedTrade {
    #[key]
    pub order_a_position_id: PositionId,
    pub order_a_base_asset_id: AssetId,
    pub order_a_base_amount: i64,
    pub order_a_quote_asset_id: AssetId,
    pub order_a_quote_amount: i64,
    pub fee_a_asset_id: AssetId,
    pub fee_a_amount: u64,
    #[key]
    pub order_b_position_id: PositionId,
    pub order_b_base_asset_id: AssetId,
    pub order_b_base_amount: i64,
    pub order_b_quote_asset_id: AssetId,
    pub order_b_quote_amount: i64,
    pub fee_b_asset_id: AssetId,
    pub fee_b_amount: u64,
    pub actual_amount_base_a: i64,
    pub actual_amount_quote_a: i64,
    #[key]
    pub order_a_hash: felt252,
    #[key]
    pub order_b_hash: felt252,
}
