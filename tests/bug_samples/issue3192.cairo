#[starknet::interface]
trait IContract<TStorage> {
    fn foo(self: @TStorage, calldata: felt252);
}
