// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (utils/src/cryptography/nonces.cairo)

/// # Nonces Component
///
/// The Nonces component provides a simple mechanism for handling incremental
/// nonces for a set of addresses. It is commonly used to prevent replay attacks
/// when contracts accept signatures as input.
#[starknet::component]
pub mod NoncesComponent {
    use starknet::ContractAddress;
    use starknet::storage::{Map, StorageMapReadAccess, StorageMapWriteAccess};
    use crate::interfaces::INonces;

    #[storage]
    pub struct Storage {
        pub Nonces_nonces: Map<ContractAddress, felt252>,
    }

    pub mod Errors {
        pub const INVALID_NONCE: felt252 = 'Nonces: invalid nonce';
    }

    #[embeddable_as(NoncesImpl)]
    impl Nonces<
        TContractState, +HasComponent<TContractState>,
    > of INonces<ComponentState<TContractState>> {
        /// Returns the next unused nonce for an address.
        fn nonces(self: @ComponentState<TContractState>, owner: ContractAddress) -> felt252 {
            self.Nonces_nonces.read(owner)
        }
    }

    #[generate_trait]
    pub impl InternalImpl<
        TContractState, +HasComponent<TContractState>,
    > of InternalTrait<TContractState> {
        /// Consumes a nonce, returns the current value, and increments nonce.
        fn use_nonce(ref self: ComponentState<TContractState>, owner: ContractAddress) -> felt252 {
            // For each account, the nonce has an initial value of 0, can only be incremented by
            // one, and cannot be decremented or reset. This guarantees that the nonce never
            // overflows.
            let nonce = self.Nonces_nonces.read(owner);
            self.Nonces_nonces.write(owner, nonce + 1);
            nonce
        }

        /// Same as `use_nonce` but checking that `nonce` is the next valid one for `owner`.
        fn use_checked_nonce(
            ref self: ComponentState<TContractState>, owner: ContractAddress, nonce: felt252,
        ) -> felt252 {
            let current = self.use_nonce(owner);
            assert(nonce == current, Errors::INVALID_NONCE);
            current
        }
    }
}
