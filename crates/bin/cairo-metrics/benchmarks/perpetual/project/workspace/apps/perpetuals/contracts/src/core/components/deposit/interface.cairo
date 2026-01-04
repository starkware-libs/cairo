use perpetuals::core::types::asset::AssetId;
use perpetuals::core::types::position::PositionId;
use starknet::ContractAddress;
use starkware_utils::signature::stark::HashType;
use starkware_utils::time::time::{TimeDelta, Timestamp};

#[starknet::interface]
pub trait IDeposit<TContractState> {
    fn deposit(
        ref self: TContractState, position_id: PositionId, quantized_amount: u64, salt: felt252,
    );

    fn deposit_asset(
        ref self: TContractState,
        asset_id: AssetId,
        position_id: PositionId,
        quantized_amount: u64,
        salt: felt252,
    );
    fn cancel_deposit(
        ref self: TContractState,
        asset_id: AssetId,
        position_id: PositionId,
        quantized_amount: u64,
        salt: felt252,
    );
    fn reject_deposit(
        ref self: TContractState,
        operator_nonce: u64,
        depositor: ContractAddress,
        asset_id: AssetId,
        position_id: PositionId,
        quantized_amount: u64,
        salt: felt252,
    );
    fn process_deposit(
        ref self: TContractState,
        operator_nonce: u64,
        depositor: ContractAddress,
        asset_id: AssetId,
        position_id: PositionId,
        quantized_amount: u64,
        salt: felt252,
    );
    fn get_deposit_status(self: @TContractState, deposit_hash: HashType) -> DepositStatus;
    fn get_cancel_delay(self: @TContractState) -> TimeDelta;
}

#[derive(Debug, Drop, PartialEq, Serde, starknet::Store)]
pub enum DepositStatus {
    #[default]
    NOT_REGISTERED,
    PROCESSED,
    CANCELED,
    PENDING: Timestamp,
}
