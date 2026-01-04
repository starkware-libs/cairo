// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (security/src/reentrancyguard.cairo)

/// # ReentrancyGuard Component
///
/// The ReentrancyGuard component helps prevent nested (reentrant) calls
/// to a function.
#[starknet::component]
pub mod ReentrancyGuardComponent {
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    #[storage]
    pub struct Storage {
        pub ReentrancyGuard_entered: bool,
    }

    pub mod Errors {
        pub const REENTRANT_CALL: felt252 = 'ReentrancyGuard: reentrant call';
    }

    #[generate_trait]
    pub impl InternalImpl<
        TContractState, +HasComponent<TContractState>,
    > of InternalTrait<TContractState> {
        /// Prevents a contract's function from calling itself or another protected function,
        /// directly or indirectly.
        fn start(ref self: ComponentState<TContractState>) {
            assert(!self.ReentrancyGuard_entered.read(), Errors::REENTRANT_CALL);
            self.ReentrancyGuard_entered.write(true);
        }

        /// Removes the reentrant guard.
        fn end(ref self: ComponentState<TContractState>) {
            self.ReentrancyGuard_entered.write(false);
        }
    }
}
