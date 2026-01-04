use starknet::ContractAddress;

#[starknet::interface]
pub trait VestingWalletABI<TState> {
    // IVesting
    fn start(self: @TState) -> u64;
    fn cliff(self: @TState) -> u64;
    fn duration(self: @TState) -> u64;
    fn end(self: @TState) -> u64;
    fn released(self: @TState, token: ContractAddress) -> u256;
    fn releasable(self: @TState, token: ContractAddress) -> u256;
    fn vested_amount(self: @TState, token: ContractAddress, timestamp: u64) -> u256;
    fn release(ref self: TState, token: ContractAddress) -> u256;

    // IOwnable
    fn owner(self: @TState) -> ContractAddress;
    fn transfer_ownership(ref self: TState, new_owner: ContractAddress);
    fn renounce_ownership(ref self: TState);

    // IOwnableCamelOnly
    fn transferOwnership(ref self: TState, newOwner: ContractAddress);
    fn renounceOwnership(ref self: TState);
}
