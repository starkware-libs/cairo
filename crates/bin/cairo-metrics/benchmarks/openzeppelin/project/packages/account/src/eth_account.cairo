// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (account/src/eth_account.cairo)

/// # EthAccount Component
///
/// The EthAccount component enables contracts to behave as accounts signing with Ethereum keys.
#[starknet::component]
pub mod EthAccountComponent {
    use core::hash::{HashStateExTrait, HashStateTrait};
    use core::num::traits::Zero;
    use core::poseidon::{PoseidonTrait, poseidon_hash_span};
    use openzeppelin_introspection::src5::SRC5Component;
    use openzeppelin_introspection::src5::SRC5Component::{
        InternalTrait as SRC5InternalTrait, SRC5Impl,
    };
    use starknet::SyscallResultTrait;
    use starknet::account::Call;
    use starknet::secp256_trait::Secp256PointTrait;
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use crate::interface;
    use crate::interface::EthPublicKey;
    use crate::utils::secp256_point::Secp256PointStorePacking;
    use crate::utils::{execute_single_call, is_tx_version_valid, is_valid_eth_signature};

    #[storage]
    pub struct Storage {
        pub EthAccount_public_key: EthPublicKey,
    }

    #[event]
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub enum Event {
        OwnerAdded: OwnerAdded,
        OwnerRemoved: OwnerRemoved,
    }

    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct OwnerAdded {
        #[key]
        pub new_owner_guid: felt252,
    }

    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct OwnerRemoved {
        #[key]
        pub removed_owner_guid: felt252,
    }

    pub mod Errors {
        pub const INVALID_CALLER: felt252 = 'EthAccount: invalid caller';
        pub const INVALID_SIGNATURE: felt252 = 'EthAccount: invalid signature';
        pub const INVALID_TX_VERSION: felt252 = 'EthAccount: invalid tx version';
        pub const UNAUTHORIZED: felt252 = 'EthAccount: unauthorized';
    }

    //
    // External
    //

    #[embeddable_as(SRC6Impl)]
    impl SRC6<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of interface::ISRC6<ComponentState<TContractState>> {
        /// Executes a list of calls from the account.
        ///
        /// Requirements:
        ///
        /// - The transaction version must be greater than or equal to `MIN_TRANSACTION_VERSION`.
        /// - If the transaction is a simulation (version >= `QUERY_OFFSET`), it must be
        /// greater than or equal to `QUERY_OFFSET` + `MIN_TRANSACTION_VERSION`.
        fn __execute__(self: @ComponentState<TContractState>, calls: Array<Call>) {
            // Avoid calls from other contracts
            // https://github.com/OpenZeppelin/cairo-contracts/issues/344
            let sender = starknet::get_caller_address();
            assert(sender.is_zero(), Errors::INVALID_CALLER);
            assert(is_tx_version_valid(), Errors::INVALID_TX_VERSION);

            for call in calls.span() {
                execute_single_call(call);
            }
        }

        /// Verifies the validity of the signature for the current transaction.
        /// This function is used by the protocol to verify `invoke` transactions.
        fn __validate__(self: @ComponentState<TContractState>, calls: Array<Call>) -> felt252 {
            self.validate_transaction()
        }

        /// Verifies that the given signature is valid for the given hash.
        fn is_valid_signature(
            self: @ComponentState<TContractState>, hash: felt252, signature: Array<felt252>,
        ) -> felt252 {
            if self._is_valid_signature(hash, signature.span()) {
                starknet::VALIDATED
            } else {
                0
            }
        }
    }

    #[embeddable_as(DeclarerImpl)]
    impl Declarer<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of interface::IDeclarer<ComponentState<TContractState>> {
        /// Verifies the validity of the signature for the current transaction.
        /// This function is used by the protocol to verify `declare` transactions.
        fn __validate_declare__(
            self: @ComponentState<TContractState>, class_hash: felt252,
        ) -> felt252 {
            self.validate_transaction()
        }
    }

    #[embeddable_as(DeployableImpl)]
    impl Deployable<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of interface::IEthDeployable<ComponentState<TContractState>> {
        /// Verifies the validity of the signature for the current transaction.
        /// This function is used by the protocol to verify `deploy_account` transactions.
        fn __validate_deploy__(
            self: @ComponentState<TContractState>,
            class_hash: felt252,
            contract_address_salt: felt252,
            public_key: EthPublicKey,
        ) -> felt252 {
            self.validate_transaction()
        }
    }

    #[embeddable_as(PublicKeyImpl)]
    impl PublicKey<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of interface::IEthPublicKey<ComponentState<TContractState>> {
        /// Returns the current public key of the account.
        fn get_public_key(self: @ComponentState<TContractState>) -> EthPublicKey {
            self.EthAccount_public_key.read()
        }

        /// Sets the public key of the account to `new_public_key`.
        ///
        /// Requirements:
        ///
        /// - The caller must be the contract itself.
        /// - The signature must be valid for the new owner.
        ///
        /// Emits an `OwnerRemoved` event.
        fn set_public_key(
            ref self: ComponentState<TContractState>,
            new_public_key: EthPublicKey,
            signature: Span<felt252>,
        ) {
            self.assert_only_self();

            let current_public_key: EthPublicKey = self.EthAccount_public_key.read();
            let removed_owner_guid = _get_guid_from_public_key(current_public_key);

            self.assert_valid_new_owner(current_public_key, new_public_key, signature);

            self.emit(OwnerRemoved { removed_owner_guid });
            self._set_public_key(new_public_key);
        }
    }

    /// Adds camelCase support for `ISRC6`.
    #[embeddable_as(SRC6CamelOnlyImpl)]
    impl SRC6CamelOnly<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of interface::ISRC6CamelOnly<ComponentState<TContractState>> {
        fn isValidSignature(
            self: @ComponentState<TContractState>, hash: felt252, signature: Array<felt252>,
        ) -> felt252 {
            SRC6::is_valid_signature(self, hash, signature)
        }
    }

    /// Adds camelCase support for `PublicKeyTrait`.
    #[embeddable_as(PublicKeyCamelImpl)]
    impl PublicKeyCamel<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of interface::IEthPublicKeyCamel<ComponentState<TContractState>> {
        fn getPublicKey(self: @ComponentState<TContractState>) -> EthPublicKey {
            self.EthAccount_public_key.read()
        }

        fn setPublicKey(
            ref self: ComponentState<TContractState>,
            newPublicKey: EthPublicKey,
            signature: Span<felt252>,
        ) {
            PublicKey::set_public_key(ref self, newPublicKey, signature);
        }
    }

    #[embeddable_as(EthAccountMixinImpl)]
    impl EthAccountMixin<
        TContractState,
        +HasComponent<TContractState>,
        impl SRC5: SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of interface::EthAccountABI<ComponentState<TContractState>> {
        // ISRC6
        fn __execute__(self: @ComponentState<TContractState>, calls: Array<Call>) {
            SRC6::__execute__(self, calls)
        }

        fn __validate__(self: @ComponentState<TContractState>, calls: Array<Call>) -> felt252 {
            SRC6::__validate__(self, calls)
        }

        fn is_valid_signature(
            self: @ComponentState<TContractState>, hash: felt252, signature: Array<felt252>,
        ) -> felt252 {
            SRC6::is_valid_signature(self, hash, signature)
        }

        // ISRC6CamelOnly
        fn isValidSignature(
            self: @ComponentState<TContractState>, hash: felt252, signature: Array<felt252>,
        ) -> felt252 {
            SRC6CamelOnly::isValidSignature(self, hash, signature)
        }

        // IDeclarer
        fn __validate_declare__(
            self: @ComponentState<TContractState>, class_hash: felt252,
        ) -> felt252 {
            Declarer::__validate_declare__(self, class_hash)
        }

        // IDeployable
        fn __validate_deploy__(
            self: @ComponentState<TContractState>,
            class_hash: felt252,
            contract_address_salt: felt252,
            public_key: EthPublicKey,
        ) -> felt252 {
            Deployable::__validate_deploy__(self, class_hash, contract_address_salt, public_key)
        }

        // IPublicKey
        fn get_public_key(self: @ComponentState<TContractState>) -> EthPublicKey {
            PublicKey::get_public_key(self)
        }

        fn set_public_key(
            ref self: ComponentState<TContractState>,
            new_public_key: EthPublicKey,
            signature: Span<felt252>,
        ) {
            PublicKey::set_public_key(ref self, new_public_key, signature);
        }

        // IPublicKeyCamel
        fn getPublicKey(self: @ComponentState<TContractState>) -> EthPublicKey {
            PublicKeyCamel::getPublicKey(self)
        }

        fn setPublicKey(
            ref self: ComponentState<TContractState>,
            newPublicKey: EthPublicKey,
            signature: Span<felt252>,
        ) {
            PublicKeyCamel::setPublicKey(ref self, newPublicKey, signature);
        }

        // ISRC5
        fn supports_interface(
            self: @ComponentState<TContractState>, interface_id: felt252,
        ) -> bool {
            let src5 = get_dep_component!(self, SRC5);
            src5.supports_interface(interface_id)
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
        /// Initializes the account with the given public key, and registers the ISRC6 interface ID.
        ///
        /// Emits an `OwnerAdded` event.
        fn initializer(ref self: ComponentState<TContractState>, public_key: EthPublicKey) {
            let mut src5_component = get_dep_component_mut!(ref self, SRC5);
            src5_component.register_interface(interface::ISRC6_ID);
            self._set_public_key(public_key);
        }

        /// Validates that the caller is the account itself. Otherwise it reverts.
        fn assert_only_self(self: @ComponentState<TContractState>) {
            let caller = starknet::get_caller_address();
            let self = starknet::get_contract_address();
            assert(self == caller, Errors::UNAUTHORIZED);
        }

        /// Validates that `new_owner` accepted the ownership of the contract.
        ///
        /// WARNING: This function assumes that `current_owner` is the current owner of the
        /// contract, and does not validate this assumption.
        ///
        /// Requirements:
        ///
        /// - The signature must be valid for the `new_owner`.
        fn assert_valid_new_owner(
            self: @ComponentState<TContractState>,
            current_owner: EthPublicKey,
            new_owner: EthPublicKey,
            signature: Span<felt252>,
        ) {
            let message_hash = PoseidonTrait::new()
                .update_with('StarkNet Message')
                .update_with('accept_ownership')
                .update_with(starknet::get_contract_address())
                .update_with(current_owner.get_coordinates().unwrap_syscall())
                .finalize();

            let is_valid = is_valid_eth_signature(message_hash, new_owner, signature);
            assert(is_valid, Errors::INVALID_SIGNATURE);
        }

        /// Validates the signature for the current transaction.
        /// Returns the short string `VALID` if valid, otherwise it reverts.
        fn validate_transaction(self: @ComponentState<TContractState>) -> felt252 {
            let tx_info = starknet::get_tx_info().unbox();
            let tx_hash = tx_info.transaction_hash;
            let signature = tx_info.signature;
            assert(self._is_valid_signature(tx_hash, signature), Errors::INVALID_SIGNATURE);
            starknet::VALIDATED
        }

        /// Sets the public key without validating the caller.
        /// The usage of this method outside the `set_public_key` function is discouraged.
        ///
        /// Emits an `OwnerAdded` event.
        fn _set_public_key(ref self: ComponentState<TContractState>, new_public_key: EthPublicKey) {
            self.EthAccount_public_key.write(new_public_key);
            let new_owner_guid = _get_guid_from_public_key(new_public_key);
            self.emit(OwnerAdded { new_owner_guid });
        }

        /// Returns whether the given signature is valid for the given hash
        /// using the account's current public key.
        fn _is_valid_signature(
            self: @ComponentState<TContractState>, hash: felt252, signature: Span<felt252>,
        ) -> bool {
            let public_key: EthPublicKey = self.EthAccount_public_key.read();
            is_valid_eth_signature(hash, public_key, signature)
        }
    }

    fn _get_guid_from_public_key(public_key: EthPublicKey) -> felt252 {
        let (x, y) = public_key.get_coordinates().unwrap_syscall();
        poseidon_hash_span(array![x.low.into(), x.high.into(), y.low.into(), y.high.into()].span())
    }
}
