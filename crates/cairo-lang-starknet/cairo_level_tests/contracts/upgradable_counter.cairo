#[starknet::interface]
trait ICounterContract<TContractState> {
    fn increase_counter(ref self: TContractState, amount: u128);
    fn decrease_counter(ref self: TContractState, amount: u128);
    fn get_counter(self: @TContractState) -> u128;
}

#[starknet::contract]
mod counter_contract {
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use crate::components::ownable::ownable as ownable_comp;
    use crate::components::upgradable::upgradable as upgradable_comp;
    component!(path: upgradable_comp, storage: upgradable, event: UpgradableEvent);
    component!(path: ownable_comp, storage: ownable, event: OwnableEvent);

    #[storage]
    struct Storage {
        counter: u128,
        #[substorage(v0)]
        upgradable: upgradable_comp::Storage,
        #[substorage(v0)]
        ownable: ownable_comp::Storage,
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        CounterIncreased: CounterIncreased,
        CounterDecreased: CounterDecreased,
        UpgradableEvent: upgradable_comp::Event,
        OwnableEvent: ownable_comp::Event,
    }

    #[derive(Drop, starknet::Event)]
    struct CounterIncreased {
        amount: u128,
    }

    #[derive(Drop, starknet::Event)]
    struct CounterDecreased {
        amount: u128,
    }

    #[constructor]
    fn constructor(ref self: ContractState, initial_counter: u128) {
        self.counter.write(initial_counter);
    }

    #[abi(embed_v0)]
    impl CounterContract of super::ICounterContract<ContractState> {
        fn get_counter(self: @ContractState) -> u128 {
            self.counter.read()
        }

        fn increase_counter(ref self: ContractState, amount: u128) {
            let current = self.counter.read();
            self.counter.write(current + amount);
            self.emit(CounterIncreased { amount });
        }

        fn decrease_counter(ref self: ContractState, amount: u128) {
            let current = self.counter.read();
            self.counter.write(current - amount);
            self.emit(CounterDecreased { amount });
        }
    }

    #[abi(embed_v0)]
    impl Upgradable = upgradable_comp::UpgradableImpl<ContractState>;
}
