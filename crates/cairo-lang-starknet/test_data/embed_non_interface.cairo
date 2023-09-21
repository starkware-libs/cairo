trait NonInterfaceTrait<TContractState> {
    fn foo(self: @TContractState);
}

#[starknet::contract]
mod contract {
    #[storage]
    struct Storage {
    }

    #[abi(embed_v0)]
    impl Impl of super::NonInterfaceTrait<ContractState> {
        fn foo(self: @ContractState) {}
    }
}
