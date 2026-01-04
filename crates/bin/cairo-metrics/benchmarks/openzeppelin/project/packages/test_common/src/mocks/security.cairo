use starknet::ContractAddress;

#[starknet::contract]
#[with_components(Initializable)]
pub mod InitializableMock {
    #[abi(embed_v0)]
    impl InitializableImpl =
        InitializableComponent::InitializableImpl<ContractState>;

    #[storage]
    pub struct Storage {}
}

#[starknet::contract]
#[with_components(Pausable)]
pub mod PausableMock {
    #[abi(embed_v0)]
    impl PausableImpl = PausableComponent::PausableImpl<ContractState>;

    #[storage]
    pub struct Storage {}
}

#[starknet::interface]
trait IReentrancyGuarded<TState> {
    fn count_external_recursive(ref self: TState, n: felt252);
}

#[starknet::interface]
pub trait IReentrancyMock<TState> {
    fn count(ref self: TState);
    fn current_count(self: @TState) -> felt252;
    fn callback(ref self: TState);
    fn count_local_recursive(ref self: TState, n: felt252);
    fn count_external_recursive(ref self: TState, n: felt252);
    fn count_and_call(ref self: TState, attacker: ContractAddress);
}

#[starknet::contract]
#[with_components(ReentrancyGuard)]
pub mod ReentrancyMock {
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use starknet::{ContractAddress, get_contract_address};
    use super::{
        IAttackerDispatcher, IAttackerDispatcherTrait, IReentrancyGuardedDispatcher,
        IReentrancyGuardedDispatcherTrait,
    };

    #[storage]
    pub struct Storage {
        pub counter: felt252,
    }

    #[abi(embed_v0)]
    impl IReentrancyMockImpl of super::IReentrancyMock<ContractState> {
        fn count(ref self: ContractState) {
            self.counter.write(self.counter.read() + 1);
        }

        fn current_count(self: @ContractState) -> felt252 {
            self.counter.read()
        }

        fn callback(ref self: ContractState) {
            self.reentrancy_guard.start();
            self.count();
            self.reentrancy_guard.end();
        }

        fn count_local_recursive(ref self: ContractState, n: felt252) {
            self.reentrancy_guard.start();

            if n != 0 {
                self.count();
                self.count_local_recursive(n - 1);
            }

            self.reentrancy_guard.end();
        }

        fn count_external_recursive(ref self: ContractState, n: felt252) {
            self.reentrancy_guard.start();

            if n != 0 {
                self.count();
                let this: ContractAddress = get_contract_address();
                IReentrancyGuardedDispatcher { contract_address: this }
                    .count_external_recursive(n - 1)
            }

            self.reentrancy_guard.end();
        }

        fn count_and_call(ref self: ContractState, attacker: ContractAddress) {
            self.reentrancy_guard.start();

            self.count();
            IAttackerDispatcher { contract_address: attacker }.call_sender();

            self.reentrancy_guard.end();
        }
    }
}

//
// Attacker
//

#[starknet::interface]
trait IAttacker<TState> {
    fn call_sender(self: @TState);
}

#[starknet::contract]
pub mod Attacker {
    use starknet::{ContractAddress, get_caller_address};
    use super::{IReentrancyMockDispatcher, IReentrancyMockDispatcherTrait};

    #[storage]
    pub struct Storage {}

    #[abi(embed_v0)]
    impl IAttackerImpl of super::IAttacker<ContractState> {
        fn call_sender(self: @ContractState) {
            let caller: ContractAddress = get_caller_address();
            IReentrancyMockDispatcher { contract_address: caller }.callback();
        }
    }
}
