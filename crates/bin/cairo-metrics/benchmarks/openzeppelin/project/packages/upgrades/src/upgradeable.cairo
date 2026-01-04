// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (upgrades/src/upgradeable.cairo)

/// # Upgradeable Component
///
/// The Upgradeable component provides a mechanism to make a contract upgradeable.
#[starknet::component]
pub mod UpgradeableComponent {
    use core::num::traits::Zero;
    use starknet::{ClassHash, SyscallResultTrait};

    #[storage]
    pub struct Storage {}

    #[event]
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub enum Event {
        Upgraded: Upgraded,
    }

    /// Emitted when the contract is upgraded.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct Upgraded {
        pub class_hash: ClassHash,
    }

    pub mod Errors {
        pub const INVALID_CLASS: felt252 = 'Class hash cannot be zero';
    }

    //
    // Internal
    //

    #[generate_trait]
    pub impl InternalImpl<
        TContractState, +HasComponent<TContractState>,
    > of InternalTrait<TContractState> {
        /// Replaces the contract's class hash with `new_class_hash`.
        ///
        /// Requirements:
        ///
        /// - `new_class_hash` is not zero.
        ///
        /// Emits an `Upgraded` event.
        fn upgrade(ref self: ComponentState<TContractState>, new_class_hash: ClassHash) {
            assert(new_class_hash.is_non_zero(), Errors::INVALID_CLASS);
            starknet::syscalls::replace_class_syscall(new_class_hash).unwrap_syscall();
            self.emit(Upgraded { class_hash: new_class_hash });
        }

        /// Replaces the contract's class hash with `new_class_hash` and then calls `selector`
        /// from the upgraded context.
        /// This function returns the unwrapped response data from a call made to the contract
        /// itself, using the specified `selector` as the entrypoint.
        ///
        /// Requirements:
        ///
        /// - `new_class_hash` is not zero.
        ///
        /// Emits an `Upgraded` event.
        fn upgrade_and_call(
            ref self: ComponentState<TContractState>,
            new_class_hash: ClassHash,
            selector: felt252,
            calldata: Span<felt252>,
        ) -> Span<felt252> {
            self.upgrade(new_class_hash);
            let this = starknet::get_contract_address();
            // `call_contract_syscall` is used in order to call `selector` from the new class.
            // See:
            // https://docs.starknet.io/architecture-and-concepts/smart-contracts/system-calls-cairo1/#replace_class
            starknet::syscalls::call_contract_syscall(this, selector, calldata).unwrap_syscall()
        }
    }
}
