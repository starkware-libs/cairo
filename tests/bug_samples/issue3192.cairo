#[starknet::interface]
pub trait IContract<TContractState> {
    fn foo(self: @TContractState, calldata: felt252);
}
