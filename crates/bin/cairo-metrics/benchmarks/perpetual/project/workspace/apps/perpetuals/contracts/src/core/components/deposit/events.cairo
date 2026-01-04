use perpetuals::core::types::asset::AssetId;
use perpetuals::core::types::position::PositionId;
use starknet::ContractAddress;

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct Deposit {
    #[key]
    pub position_id: PositionId,
    #[key]
    pub depositing_address: ContractAddress,
    pub collateral_id: AssetId,
    pub quantized_amount: u64,
    pub unquantized_amount: u64,
    #[key]
    pub deposit_request_hash: felt252,
    pub salt: felt252,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct DepositProcessed {
    #[key]
    pub position_id: PositionId,
    #[key]
    pub depositing_address: ContractAddress,
    pub collateral_id: AssetId,
    pub quantized_amount: u64,
    pub unquantized_amount: u256,
    #[key]
    pub deposit_request_hash: felt252,
    pub salt: felt252,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct DepositCanceled {
    #[key]
    pub position_id: PositionId,
    #[key]
    pub depositing_address: ContractAddress,
    pub collateral_id: AssetId,
    pub quantized_amount: u64,
    pub unquantized_amount: u256,
    #[key]
    pub deposit_request_hash: felt252,
    pub salt: felt252,
}
