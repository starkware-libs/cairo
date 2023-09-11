#[starknet::interface]
trait IOtherContract<TContractState> {
    fn decrease_allowed(self: @TContractState) -> bool;
}

#[starknet::interface]
trait ICounterContract<TContractState> {
    fn increase_counter(ref self: TContractState, amount: u128);
    fn decrease_counter(ref self: TContractState, amount: u128);
    fn get_counter(self: @TContractState) -> u128;
}


#[derive(Drop, starknet::Event)]
struct OutOfScopeEvent {}

#[starknet::contract]
mod counter_contract {
    use starknet::ContractAddress;
    use super::{
        IOtherContractDispatcher, IOtherContractDispatcherTrait, IOtherContractLibraryDispatcher,
        OutOfScopeEvent,
    };
    use super::test_component::dataComponentMemberStateTrait;
    component !(path: super::test_component, storage: test_component_storage, event: Event);

    #[storage]
    struct Storage {
        #[nested(v0)]
        test_component_storage: super::test_component::Storage,
        counter: u128,
        other_contract: IOtherContractDispatcher
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        // TODO: Rename event once the name does not have to match the type.
        Event: super::test_component::Event,
        CounterIncreased: CounterIncreased,
        CounterDecreased: CounterDecreased,
        OutOfScopeEvent: OutOfScopeEvent
    }

    #[derive(Drop, starknet::Event)]
    struct CounterIncreased {
        amount: u128
    }

    #[derive(Drop, starknet::Event)]
    struct CounterDecreased {
        amount: u128
    }

    #[constructor]
    fn constructor(
        ref self: ContractState, initial_counter: u128, other_contract_addr: ContractAddress
    ) {
        self.counter.write(initial_counter);
        self
            .other_contract
            .write(IOtherContractDispatcher { contract_address: other_contract_addr });
    }

    #[external(v0)]
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
            let allowed = self.other_contract.read().decrease_allowed();
            if allowed {
                let current = self.counter.read();
                self.counter.write(current - amount);
                self.emit(CounterDecreased { amount });
            }
        }
    }
}


#[starknet::component]
mod test_component {
    #[storage]
    struct Storage {
        data: u32
    }
}
