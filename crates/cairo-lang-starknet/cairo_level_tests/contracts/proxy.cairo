#[starknet::interface]
trait ICounterContract<TContractState> {
    fn increase_counter(ref self: TContractState, amount: u128);
    fn decrease_counter(ref self: TContractState, amount: u128);
    fn get_counter(self: @TContractState) -> u128;
}

#[starknet::contract]
mod proxy {
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};

    #[storage]
    struct Storage {
        implementation: starknet::ClassHash,
    }

    #[constructor]
    fn constructor(ref self: ContractState, implementation: starknet::ClassHash) {
        self.implementation.write(implementation);
    }

    impl ForwardingClassHashImpl of starknet::ForwardingClassHash<ContractState> {
        fn class_hash(self: @ContractState) -> starknet::ClassHash {
            self.implementation.read()
        }
    }

    #[abi(embed_v0)]
    impl ForwardedImpl = super::ICounterContractForwardImpl<ContractState>;
}
