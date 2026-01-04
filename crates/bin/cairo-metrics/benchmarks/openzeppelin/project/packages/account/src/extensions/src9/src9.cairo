// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (account/src/extensions/src9/src9.cairo)

/// # SRC9 Component (Outside Execution)
///
/// The SRC9 component allows a protocol to submit transactions on behalf of a user account,
/// as long as they provide the relevant signatures.
///
/// This component is designed to be account agnostic, as long as the contract implements the ISRC6
/// interface.
#[starknet::component]
pub mod SRC9Component {
    use openzeppelin_introspection::src5::SRC5Component;
    use openzeppelin_introspection::src5::SRC5Component::InternalTrait as SRC5InternalTrait;
    use openzeppelin_utils::cryptography::snip12::{OffchainMessageHash, SNIP12Metadata};
    use starknet::storage::{Map, StorageMapReadAccess, StorageMapWriteAccess};
    use crate::extensions::src9::snip12_utils::OutsideExecutionStructHash;
    use crate::extensions::src9::{OutsideExecution, interface};
    use crate::interface::{ISRC6Dispatcher, ISRC6DispatcherTrait};
    use crate::utils::execute_calls;

    #[storage]
    pub struct Storage {
        pub SRC9_nonces: Map<felt252, bool>,
    }

    pub mod Errors {
        pub const INVALID_CALLER: felt252 = 'SRC9: invalid caller';
        pub const INVALID_AFTER: felt252 = 'SRC9: now <= execute_after';
        pub const INVALID_BEFORE: felt252 = 'SRC9: now >= execute_before';
        pub const DUPLICATED_NONCE: felt252 = 'SRC9: duplicated nonce';
        pub const INVALID_SIGNATURE: felt252 = 'SRC9: invalid signature';
    }

    // Name and version as defined in the SNIP-9
    pub impl SNIP12MetadataImpl of SNIP12Metadata {
        fn name() -> felt252 {
            'Account.execute_from_outside'
        }
        fn version() -> felt252 {
            2
        }
    }

    //
    // External
    //

    #[embeddable_as(OutsideExecutionV2Impl)]
    impl OutsideExecutionV2<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of interface::ISRC9_V2<ComponentState<TContractState>> {
        /// Allows anyone to submit a transaction on behalf of the account as long as they
        /// provide the relevant signatures.
        ///
        /// This method allows reentrancy. A call to `__execute__` or `execute_from_outside_v2` can
        /// trigger another nested transaction to `execute_from_outside_v2`. This implementation
        /// verifies that the provided `signature` matches the hash of `outside_execution` and that
        /// `nonce` was not already used.
        ///
        /// Arguments:
        ///
        /// - `outside_execution` - The parameters of the transaction to execute.
        /// - `signature` - A valid signature on the SNIP-12 message encoding of
        /// `outside_execution`.
        ///
        /// Requirements:
        ///
        /// - The caller must be the `outside_execution.caller` unless 'ANY_CALLER' is used.
        /// - The current time must be within the `outside_execution.execute_after` and
        /// `outside_execution.execute_before` span, excluding boundaries.
        /// - The `outside_execution.nonce` must not be used before.
        /// - The `signature` must be valid.
        fn execute_from_outside_v2(
            ref self: ComponentState<TContractState>,
            outside_execution: OutsideExecution,
            signature: Span<felt252>,
        ) -> Array<Span<felt252>> {
            // 'ANY_CALLER' can be used to bypass the caller validation
            if outside_execution.caller.into() != 'ANY_CALLER' {
                assert(
                    starknet::get_caller_address() == outside_execution.caller,
                    Errors::INVALID_CALLER,
                );
            }

            // 1. Validate the execution time span
            let now = starknet::get_block_timestamp();
            assert(outside_execution.execute_after < now, Errors::INVALID_AFTER);
            assert(now < outside_execution.execute_before, Errors::INVALID_BEFORE);

            // 2. Validate the nonce
            assert(!self.SRC9_nonces.read(outside_execution.nonce), Errors::DUPLICATED_NONCE);

            // 3. Mark the nonce as used
            self.SRC9_nonces.write(outside_execution.nonce, true);

            // 4. Validate the signature
            let this = starknet::get_contract_address();
            let outside_tx_hash = outside_execution.get_message_hash(this);

            // Make this component agnostic to the account implementation, as long
            // as the contract implements the SRC6 interface.
            let is_valid_signature_felt = ISRC6Dispatcher { contract_address: this }
                .is_valid_signature(outside_tx_hash, signature.into());

            // Check either 'VALID' or true for backwards compatibility.
            let is_valid_signature = is_valid_signature_felt == starknet::VALIDATED
                || is_valid_signature_felt == 1;

            assert(is_valid_signature, Errors::INVALID_SIGNATURE);

            // 5. Execute the calls
            execute_calls(outside_execution.calls)
        }

        /// Returns the status of a given nonce. `true` if the nonce is available to use.
        fn is_valid_outside_execution_nonce(
            self: @ComponentState<TContractState>, nonce: felt252,
        ) -> bool {
            !self.SRC9_nonces.read(nonce)
        }
    }

    //
    // Internal
    //

    #[generate_trait]
    pub impl InternalImpl<
        TContractState,
        +HasComponent<TContractState>,
        impl SRC5: SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of InternalTrait<TContractState> {
        /// Initializes the account by registering the ISRC9_V2 interface ID.
        fn initializer(ref self: ComponentState<TContractState>) {
            let mut src5_component = get_dep_component_mut!(ref self, SRC5);
            src5_component.register_interface(interface::ISRC9_V2_ID);
        }
    }
}
