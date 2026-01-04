use perpetuals::core::types::position::PositionId;
use starkware_utils::signature::stark::HashType;

#[starknet::interface]
pub trait IFulfillment<TContractState> {
    fn update_fulfillment(
        ref self: TContractState,
        position_id: PositionId,
        hash: HashType,
        order_base_amount: i64,
        actual_base_amount: i64,
    );
    fn clean_fulfillment(ref self: TContractState, hashes: Span<felt252>);
}

