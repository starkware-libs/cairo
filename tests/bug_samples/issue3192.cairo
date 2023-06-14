#[starknet::interface]
trait IContract<TContractState> {
    fn foo(self: @TContractState, calldata: felt252);
}
