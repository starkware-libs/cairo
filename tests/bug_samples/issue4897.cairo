#[cfg(missing_cfg)]
#[starknet::interface]
trait ITests<TContractState> {
    fn set_value(ref self: TContractState, value: felt252);
}


#[starknet::contract]
mod MyContract {
    #[storage]
    struct Storage {
        value: felt252
    }

    #[cfg(missing_cfg)]
    #[abi(embed_v0)]
    impl TestsImpl of super::ITests<ContractState> {
        fn set_value(ref self: ContractState, value: felt252) {
            self.value.write(value);
        }
    }
}
