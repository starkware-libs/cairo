// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (security/src/initializable.cairo)

/// # Initializable Component
///
/// The Initializable component provides a simple mechanism that executes
/// logic once and only once. This can be useful for setting a contract's
/// initial state in scenarios where a constructor cannot be used.
#[starknet::component]
pub mod InitializableComponent {
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use crate::interface::IInitializable;

    #[storage]
    pub struct Storage {
        pub Initializable_initialized: bool,
    }

    pub mod Errors {
        pub const INITIALIZED: felt252 = 'Initializable: is initialized';
    }

    #[embeddable_as(InitializableImpl)]
    impl Initializable<
        TContractState, +HasComponent<TContractState>,
    > of IInitializable<ComponentState<TContractState>> {
        /// Returns whether the contract has been initialized.
        fn is_initialized(self: @ComponentState<TContractState>) -> bool {
            self.Initializable_initialized.read()
        }
    }

    #[generate_trait]
    pub impl InternalImpl<
        TContractState, +HasComponent<TContractState>,
    > of InternalTrait<TContractState> {
        /// Ensures that the calling function can only be called once.
        ///
        /// Requirements:
        ///
        /// - `initialize` was not previously called.
        fn initialize(ref self: ComponentState<TContractState>) {
            assert(!self.is_initialized(), Errors::INITIALIZED);
            self.Initializable_initialized.write(true);
        }
    }
}
