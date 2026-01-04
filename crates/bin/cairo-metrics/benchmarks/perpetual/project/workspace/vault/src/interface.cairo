use starknet::ContractAddress;

#[starknet::interface]
pub trait IProtocolVault<TState> {
    fn redeem_with_price(ref self: TState, shares: u256, value_of_shares: u256) -> u256;
    fn get_owning_position_id(self: @TState) -> u32;
    fn get_perps_contract(self: @TState) -> ContractAddress;
}
