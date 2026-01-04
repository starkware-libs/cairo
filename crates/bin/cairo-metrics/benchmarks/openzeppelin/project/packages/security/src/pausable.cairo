// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (security/src/pausable.cairo)

/// # Pausable Component
///
/// The Pausable component allows the using contract to implement an
/// emergency stop mechanism. Only functions that call `assert_paused`
/// or `assert_not_paused` will be affected by this mechanism.
#[starknet::component]
pub mod PausableComponent {
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use starknet::{ContractAddress, get_caller_address};
    use crate::interface::IPausable;

    #[storage]
    pub struct Storage {
        pub Pausable_paused: bool,
    }

    #[event]
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub enum Event {
        Paused: Paused,
        Unpaused: Unpaused,
    }

    /// Emitted when the pause is triggered by `account`.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct Paused {
        pub account: ContractAddress,
    }

    /// Emitted when the pause is lifted by `account`.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct Unpaused {
        pub account: ContractAddress,
    }

    pub mod Errors {
        pub const PAUSED: felt252 = 'Pausable: paused';
        pub const NOT_PAUSED: felt252 = 'Pausable: not paused';
    }

    #[embeddable_as(PausableImpl)]
    impl Pausable<
        TContractState, +HasComponent<TContractState>,
    > of IPausable<ComponentState<TContractState>> {
        /// Returns true if the contract is paused, and false otherwise.
        fn is_paused(self: @ComponentState<TContractState>) -> bool {
            self.Pausable_paused.read()
        }
    }

    #[generate_trait]
    pub impl InternalImpl<
        TContractState, +HasComponent<TContractState>,
    > of InternalTrait<TContractState> {
        /// Makes a function only callable when the contract is not paused.
        fn assert_not_paused(self: @ComponentState<TContractState>) {
            assert(!self.Pausable_paused.read(), Errors::PAUSED);
        }

        /// Makes a function only callable when the contract is paused.
        fn assert_paused(self: @ComponentState<TContractState>) {
            assert(self.Pausable_paused.read(), Errors::NOT_PAUSED);
        }

        /// Triggers a stopped state.
        ///
        /// Requirements:
        ///
        /// - The contract is not paused.
        ///
        /// Emits a `Paused` event.
        fn pause(ref self: ComponentState<TContractState>) {
            self.assert_not_paused();
            self.Pausable_paused.write(true);
            self.emit(Paused { account: get_caller_address() });
        }

        /// Lifts the pause on the contract.
        ///
        /// Requirements:
        ///
        /// - The contract is paused.
        ///
        /// Emits an `Unpaused` event.
        fn unpause(ref self: ComponentState<TContractState>) {
            self.assert_paused();
            self.Pausable_paused.write(false);
            self.emit(Unpaused { account: get_caller_address() });
        }
    }
}
