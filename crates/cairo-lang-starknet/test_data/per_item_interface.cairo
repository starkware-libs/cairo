#[starknet::interface]
trait InterfaceTrait<TContractState> {
    fn foo(self: @TContractState);
}

#[starknet::contract]
mod contract {
    #[storage]
    struct Storage {}

    #[abi(per_item)]
    impl Impl of super::InterfaceTrait<ContractState> {
        fn foo(self: @ContractState) {}
    }
}
