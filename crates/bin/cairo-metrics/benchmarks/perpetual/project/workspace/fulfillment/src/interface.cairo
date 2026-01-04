use starknet::ContractAddress;
use starkware_utils::signature::stark::HashType;
use starkware_utils::time::time::Timestamp;


#[derive(Copy, Drop, Serde)]
pub struct UpdateFulfillment {
    pub key: HashType,
    pub id: felt252,
    pub diff: u64,
    pub limit: u64,
}


#[starknet::interface]
pub trait IFulfillment<TState> {
    fn update_fulfillment(ref self: TState, updates: Span<UpdateFulfillment>);
    fn get_owner(self: @TState) -> ContractAddress;
    fn get_expiration_time(self: @TState) -> Timestamp;
    fn get_fulfillment(self: @TState, key: HashType) -> u64;
}
