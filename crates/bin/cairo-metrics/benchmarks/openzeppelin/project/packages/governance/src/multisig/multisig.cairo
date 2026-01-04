// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (governance/src/multisig/multisig.cairo)

/// # Multisig Component
///
/// Component that implements a multi-signature mechanism to enhance the security and governance
/// of smart contract transactions. It requires multiple registered signers to collectively approve
/// and execute transactions, ensuring that no single signer can perform critical actions
/// unilaterally.
///
/// By default, this component is self-administered, meaning modifications to signers or quorum must
/// be performed by the contract itself through the multisig approval process. Only registered
/// signers can submit, confirm, revoke, or execute transactions. A common use case is to secure
/// important operations by requiring multiple approvals, such as in fund management or protocol
/// governance.
#[starknet::component]
pub mod MultisigComponent {
    use core::hash::{HashStateExTrait, HashStateTrait};
    use core::num::traits::Zero;
    use core::panic_with_const_felt252;
    use core::pedersen::PedersenTrait;
    use starknet::account::Call;
    use starknet::storage::{
        Map, StorageMapReadAccess, StorageMapWriteAccess, StoragePointerReadAccess,
        StoragePointerWriteAccess,
    };
    use starknet::syscalls::call_contract_syscall;
    use starknet::{ContractAddress, SyscallResultTrait};
    use crate::multisig::interface::{IMultisig, TransactionID, TransactionState};
    use crate::multisig::storage_utils::{
        SignersInfo, SignersInfoStorePackingV2, TxInfo, TxInfoStorePacking,
    };
    use crate::utils::call_impls::{CallPartialEq, HashCallImpl, HashCallsImpl};

    #[storage]
    pub struct Storage {
        pub Multisig_signers_info: SignersInfo,
        pub Multisig_is_signer: Map<ContractAddress, bool>,
        pub Multisig_signers_by_index: Map<u32, ContractAddress>,
        pub Multisig_signers_indices: Map<ContractAddress, u32>,
        pub Multisig_tx_info: Map<TransactionID, TxInfo>,
        pub Multisig_tx_confirmed_by: Map<(TransactionID, ContractAddress), bool>,
    }

    #[event]
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub enum Event {
        SignerAdded: SignerAdded,
        SignerRemoved: SignerRemoved,
        QuorumUpdated: QuorumUpdated,
        TransactionSubmitted: TransactionSubmitted,
        TransactionConfirmed: TransactionConfirmed,
        TransactionExecuted: TransactionExecuted,
        ConfirmationRevoked: ConfirmationRevoked,
        CallSalt: CallSalt,
    }

    /// Emitted when a new `signer` is added.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct SignerAdded {
        #[key]
        pub signer: ContractAddress,
    }

    /// Emitted when a `signer` is removed.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct SignerRemoved {
        #[key]
        pub signer: ContractAddress,
    }

    /// Emitted when the `quorum` value is updated.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct QuorumUpdated {
        pub old_quorum: u32,
        pub new_quorum: u32,
    }

    /// Emitted when a new transaction is submitted by a `signer`.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct TransactionSubmitted {
        #[key]
        pub id: TransactionID,
        #[key]
        pub signer: ContractAddress,
    }

    /// Emitted when a transaction is confirmed by a `signer`.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct TransactionConfirmed {
        #[key]
        pub id: TransactionID,
        #[key]
        pub signer: ContractAddress,
    }

    /// Emitted when a `signer` revokes his confirmation.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct ConfirmationRevoked {
        #[key]
        pub id: TransactionID,
        #[key]
        pub signer: ContractAddress,
    }

    /// Emitted when a transaction is executed.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct TransactionExecuted {
        #[key]
        pub id: TransactionID,
    }

    /// Emitted when a new transaction is submitted with non-zero salt.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct CallSalt {
        #[key]
        pub id: felt252,
        pub salt: felt252,
    }

    pub mod Errors {
        pub const UNAUTHORIZED: felt252 = 'Multisig: unauthorized';
        pub const NOT_A_SIGNER: felt252 = 'Multisig: not a signer';
        pub const ALREADY_A_SIGNER: felt252 = 'Multisig: already a signer';
        pub const ALREADY_CONFIRMED: felt252 = 'Multisig: already confirmed';
        pub const HAS_NOT_CONFIRMED: felt252 = 'Multisig: has not confirmed';
        pub const TX_ALREADY_EXISTS: felt252 = 'Multisig: tx already exists';
        pub const TX_NOT_FOUND: felt252 = 'Multisig: tx not found';
        pub const TX_NOT_CONFIRMED: felt252 = 'Multisig: tx not confirmed';
        pub const TX_ALREADY_EXECUTED: felt252 = 'Multisig: tx already executed';
        pub const ZERO_ADDRESS_SIGNER: felt252 = 'Multisig: zero address signer';
        pub const ZERO_QUORUM: felt252 = 'Multisig: quorum cannot be 0';
        pub const QUORUM_TOO_HIGH: felt252 = 'Multisig: quorum > signers';
    }

    //
    // External
    //

    #[embeddable_as(MultisigImpl)]
    impl Multisig<
        TContractState, +HasComponent<TContractState>, +Drop<TContractState>,
    > of IMultisig<ComponentState<TContractState>> {
        /// Returns the current quorum value. The quorum is the minimum number
        /// of confirmations required to approve a transaction.
        fn get_quorum(self: @ComponentState<TContractState>) -> u32 {
            self.Multisig_signers_info.read().quorum
        }

        /// Returns whether the given `signer` is registered.
        /// Only registered signers can submit, confirm, or execute transactions.
        fn is_signer(self: @ComponentState<TContractState>, signer: ContractAddress) -> bool {
            self.Multisig_is_signer.read(signer)
        }

        /// Returns the list of all current signers.
        fn get_signers(self: @ComponentState<TContractState>) -> Span<ContractAddress> {
            let mut result = array![];
            let signers_count = self.Multisig_signers_info.read().signers_count;
            for i in 0..signers_count {
                result.append(self.Multisig_signers_by_index.read(i));
            }
            result.span()
        }

        /// Returns whether the transaction with the given `id` has been confirmed.
        /// A confirmed transaction has received the required number of confirmations (quorum).
        fn is_confirmed(self: @ComponentState<TContractState>, id: TransactionID) -> bool {
            match self.resolve_tx_state(id) {
                TransactionState::NotFound => false,
                TransactionState::Pending => false,
                TransactionState::Confirmed => true,
                TransactionState::Executed => true,
            }
        }

        /// Returns whether the transaction with the given `id` has been confirmed by the specified
        /// `signer`.
        ///
        /// NOTE: Even if a `signer` is removed after confirming a transaction, this function will
        /// still return true. However, their confirmation does not count toward the quorum when it
        /// is checked. Therefore, this function should not be relied upon to verify legitimate
        /// confirmations toward meeting the quorum. For that, use `get_transaction_confirmations`
        /// instead.
        fn is_confirmed_by(
            self: @ComponentState<TContractState>, id: TransactionID, signer: ContractAddress,
        ) -> bool {
            self.Multisig_tx_confirmed_by.read((id, signer))
        }

        /// Returns whether the transaction with the given `id` has been executed.
        fn is_executed(self: @ComponentState<TContractState>, id: TransactionID) -> bool {
            self.Multisig_tx_info.read(id).is_executed
        }

        /// Returns the block number when the transaction with the given `id` was submitted.
        fn get_submitted_block(self: @ComponentState<TContractState>, id: TransactionID) -> u64 {
            self.Multisig_tx_info.read(id).submitted_block
        }

        /// Returns the current state of the transaction with the given `id`.
        ///
        /// The possible states are:
        ///
        /// - `NotFound`: the transaction does not exist.
        /// - `Pending`: the transaction exists but hasn't reached the required confirmations.
        /// - `Confirmed`: the transaction has reached the required confirmations but hasn't been
        /// executed.
        /// - `Executed`: the transaction has been executed.
        fn get_transaction_state(
            self: @ComponentState<TContractState>, id: TransactionID,
        ) -> TransactionState {
            self.resolve_tx_state(id)
        }

        /// Returns the number of confirmations from registered signers for the transaction with the
        /// specified `id`.
        fn get_transaction_confirmations(
            self: @ComponentState<TContractState>, id: TransactionID,
        ) -> u32 {
            let mut result = 0;
            let all_signers = self.get_signers();
            for signer in all_signers {
                if self.is_confirmed_by(id, *signer) {
                    result += 1;
                }
            }
            result
        }

        /// Returns the computed identifier of a transaction containing a single call.
        fn hash_transaction(
            self: @ComponentState<TContractState>,
            to: ContractAddress,
            selector: felt252,
            calldata: Span<felt252>,
            salt: felt252,
        ) -> TransactionID {
            let call = Call { to, selector, calldata };
            self.hash_transaction_batch(array![call].span(), salt)
        }

        /// Returns the computed identifier of a transaction containing a batch of calls.
        fn hash_transaction_batch(
            self: @ComponentState<TContractState>, calls: Span<Call>, salt: felt252,
        ) -> TransactionID {
            PedersenTrait::new(0).update_with(calls).update_with(salt).finalize()
        }

        /// Adds new signers and updates the quorum.
        ///
        /// Requirements:
        ///
        /// - The caller must be the contract itself.
        /// - `new_quorum` must be less than or equal to the total number of signers after addition.
        ///
        /// Emits a `SignerAdded` event for each signer added.
        ///
        /// Emits a `QuorumUpdated` event if the quorum changes.
        fn add_signers(
            ref self: ComponentState<TContractState>,
            new_quorum: u32,
            signers_to_add: Span<ContractAddress>,
        ) {
            self.assert_only_self();
            self._add_signers(new_quorum, signers_to_add);
        }

        /// Removes signers and updates the quorum.
        ///
        /// Requirements:
        ///
        /// - The caller must be the contract itself.
        /// - `new_quorum` must be less than or equal to the total number of signers after removal.
        ///
        /// Emits a `SignerRemoved` event for each signer removed.
        ///
        /// Emits a `QuorumUpdated` event if the quorum changes.
        fn remove_signers(
            ref self: ComponentState<TContractState>,
            new_quorum: u32,
            signers_to_remove: Span<ContractAddress>,
        ) {
            self.assert_only_self();
            self._remove_signers(new_quorum, signers_to_remove);
        }

        /// Replaces an existing signer with a new signer.
        ///
        /// Requirements:
        ///
        /// - The caller must be the contract itself.
        /// - `signer_to_remove` must be an existing signer.
        /// - `signer_to_add` must not be an existing signer.
        /// - `signer_to_add` must be a non-zero address.
        ///
        /// Emits a `SignerRemoved` event for the removed signer.
        ///
        /// Emits a `SignerAdded` event for the new signer.
        fn replace_signer(
            ref self: ComponentState<TContractState>,
            signer_to_remove: ContractAddress,
            signer_to_add: ContractAddress,
        ) {
            self.assert_only_self();
            self._replace_signer(signer_to_remove, signer_to_add);
        }

        /// Updates the quorum value to `new_quorum` if it differs from the current quorum.
        ///
        /// Requirements:
        ///
        /// - The caller must be the contract itself.
        /// - `new_quorum` must be non-zero.
        /// - `new_quorum` must be less than or equal to the total number of signers.
        ///
        /// Emits a `QuorumUpdated` event if the quorum changes.
        fn change_quorum(ref self: ComponentState<TContractState>, new_quorum: u32) {
            self.assert_only_self();
            self._change_quorum(new_quorum);
        }

        /// Submits a new transaction for confirmation.
        ///
        /// Requirements:
        ///
        /// - The caller must be a registered signer.
        /// - The transaction must not have been submitted before.
        ///
        /// Emits a `TransactionSubmitted` event.
        ///
        /// Emits a `CallSalt` event if `salt` is not zero.
        fn submit_transaction(
            ref self: ComponentState<TContractState>,
            to: ContractAddress,
            selector: felt252,
            calldata: Span<felt252>,
            salt: felt252,
        ) -> TransactionID {
            let call = Call { to, selector, calldata };
            self.submit_transaction_batch(array![call].span(), salt)
        }

        /// Submits a new batch transaction for confirmation.
        ///
        /// Requirements:
        ///
        /// - The caller must be a registered signer.
        /// - The transaction must not have been submitted before.
        ///
        /// Emits a `TransactionSubmitted` event.
        ///
        /// Emits a `CallSalt` event if `salt` is not zero.
        fn submit_transaction_batch(
            ref self: ComponentState<TContractState>, calls: Span<Call>, salt: felt252,
        ) -> TransactionID {
            let caller = starknet::get_caller_address();
            self.assert_one_of_signers(caller);
            let id = self.hash_transaction_batch(calls, salt);
            assert(self.get_submitted_block(id).is_zero(), Errors::TX_ALREADY_EXISTS);

            let tx_info = TxInfo {
                is_executed: false, submitted_block: starknet::get_block_number(),
            };
            self.Multisig_tx_info.write(id, tx_info);
            if salt.is_non_zero() {
                self.emit(CallSalt { id, salt });
            }
            self.emit(TransactionSubmitted { id, signer: caller });

            id
        }

        /// Confirms a transaction with the given `id`.
        ///
        /// Requirements:
        ///
        /// - The caller must be a registered signer.
        /// - The transaction must exist and not be executed.
        /// - The caller must not have already confirmed the transaction.
        ///
        /// Emits a `TransactionConfirmed` event.
        fn confirm_transaction(ref self: ComponentState<TContractState>, id: TransactionID) {
            let caller = starknet::get_caller_address();
            self.assert_one_of_signers(caller);
            self.assert_tx_exists(id);
            assert(!self.is_executed(id), Errors::TX_ALREADY_EXECUTED);
            assert(!self.is_confirmed_by(id, caller), Errors::ALREADY_CONFIRMED);

            self.Multisig_tx_confirmed_by.write((id, caller), true);
            self.emit(TransactionConfirmed { id, signer: caller });
        }

        /// Revokes a previous confirmation for a transaction with the given `id`.
        ///
        /// Requirements:
        ///
        /// - The transaction must exist and not be executed.
        /// - The caller must have previously confirmed the transaction.
        ///
        /// Emits a `ConfirmationRevoked` event.
        fn revoke_confirmation(ref self: ComponentState<TContractState>, id: TransactionID) {
            let caller = starknet::get_caller_address();
            self.assert_tx_exists(id);
            assert(!self.is_executed(id), Errors::TX_ALREADY_EXECUTED);
            assert(self.is_confirmed_by(id, caller), Errors::HAS_NOT_CONFIRMED);

            self.Multisig_tx_confirmed_by.write((id, caller), false);
            self.emit(ConfirmationRevoked { id, signer: caller });
        }

        /// Executes a confirmed transaction.
        ///
        /// Requirements:
        ///
        /// - The caller must be a registered signer.
        /// - The transaction must be confirmed and not yet executed.
        ///
        /// Emits a `TransactionExecuted` event.
        fn execute_transaction(
            ref self: ComponentState<TContractState>,
            to: ContractAddress,
            selector: felt252,
            calldata: Span<felt252>,
            salt: felt252,
        ) {
            let call = Call { to, selector, calldata };
            self.execute_transaction_batch(array![call].span(), salt)
        }

        /// Executes a confirmed batch transaction.
        ///
        /// Requirements:
        ///
        /// - The caller must be a registered signer.
        /// - The transaction must be confirmed and not yet executed.
        ///
        /// Emits a `TransactionExecuted` event.
        fn execute_transaction_batch(
            ref self: ComponentState<TContractState>, calls: Span<Call>, salt: felt252,
        ) {
            let id = self.hash_transaction_batch(calls, salt);
            match self.resolve_tx_state(id) {
                TransactionState::NotFound => panic_with_const_felt252::<Errors::TX_NOT_FOUND>(),
                TransactionState::Pending => panic_with_const_felt252::<Errors::TX_NOT_CONFIRMED>(),
                TransactionState::Executed => panic_with_const_felt252::<
                    Errors::TX_ALREADY_EXECUTED,
                >(),
                TransactionState::Confirmed => {
                    let caller = starknet::get_caller_address();
                    self.assert_one_of_signers(caller);
                    let mut tx_info = self.Multisig_tx_info.read(id);
                    tx_info.is_executed = true;
                    self.Multisig_tx_info.write(id, tx_info);
                    for call in calls {
                        let Call { to, selector, calldata } = *call;
                        call_contract_syscall(to, selector, calldata).unwrap_syscall();
                    }
                    self.emit(TransactionExecuted { id });
                },
            };
        }
    }

    //
    // Internal
    //

    #[generate_trait]
    pub impl InternalImpl<
        TContractState, +HasComponent<TContractState>, +Drop<TContractState>,
    > of InternalTrait<TContractState> {
        /// Initializes the Multisig component with the initial `quorum` and `signers`.
        /// This function must be called during contract initialization to set up the initial state.
        ///
        /// Requirements:
        ///
        /// - `quorum` must be non-zero and less than or equal to the number of `signers`.
        ///
        /// Emits a `SignerAdded` event for each signer added.
        ///
        /// Emits a `QuorumUpdated` event.
        fn initializer(
            ref self: ComponentState<TContractState>, quorum: u32, signers: Span<ContractAddress>,
        ) {
            self._add_signers(quorum, signers);
        }

        /// Resolves and returns the current state of the transaction with the given `id`.
        ///
        /// The possible states are:
        ///
        /// - `NotFound`: the transaction does not exist.
        /// - `Pending`: the transaction exists but hasn't reached the required confirmations.
        /// - `Confirmed`: the transaction has reached the required confirmations but hasn't been
        /// executed.
        /// - `Executed`: the transaction has been executed.
        fn resolve_tx_state(
            self: @ComponentState<TContractState>, id: TransactionID,
        ) -> TransactionState {
            let TxInfo { is_executed, submitted_block } = self.Multisig_tx_info.read(id);
            if submitted_block.is_zero() {
                TransactionState::NotFound
            } else if is_executed {
                TransactionState::Executed
            } else {
                let is_confirmed = self.get_transaction_confirmations(id) >= self.get_quorum();
                if is_confirmed {
                    TransactionState::Confirmed
                } else {
                    TransactionState::Pending
                }
            }
        }

        /// Asserts that the `caller` is one of the registered signers.
        ///
        /// Requirements:
        ///
        /// - The `caller` must be a registered signer.
        fn assert_one_of_signers(self: @ComponentState<TContractState>, caller: ContractAddress) {
            assert(self.is_signer(caller), Errors::NOT_A_SIGNER);
        }

        /// Asserts that a transaction with the given `id` exists.
        ///
        /// Requirements:
        ///
        /// - The transaction with the given `id` must have been submitted.
        fn assert_tx_exists(self: @ComponentState<TContractState>, id: TransactionID) {
            assert(self.get_submitted_block(id).is_non_zero(), Errors::TX_NOT_FOUND);
        }

        /// Asserts that the caller is the contract itself.
        ///
        /// Requirements:
        ///
        /// - The caller must be the contract's own address.
        fn assert_only_self(self: @ComponentState<TContractState>) {
            let caller = starknet::get_caller_address();
            let self = starknet::get_contract_address();
            assert(caller == self, Errors::UNAUTHORIZED);
        }

        /// Adds new signers and updates the quorum.
        ///
        /// Requirements:
        ///
        /// - Each signer address must be non-zero.
        /// - `new_quorum` must be non-zero and less than or equal to the total number of signers
        /// after addition.
        ///
        /// Emits a `SignerAdded` event for each new signer added.
        ///
        /// Emits a `QuorumUpdated` event if the quorum changes.
        fn _add_signers(
            ref self: ComponentState<TContractState>,
            new_quorum: u32,
            signers_to_add: Span<ContractAddress>,
        ) {
            if !signers_to_add.is_empty() {
                let SignersInfo { quorum, mut signers_count } = self.Multisig_signers_info.read();
                for signer in signers_to_add {
                    let signer_to_add = *signer;
                    assert(signer_to_add.is_non_zero(), Errors::ZERO_ADDRESS_SIGNER);
                    if self.is_signer(signer_to_add) {
                        continue;
                    }
                    let index = signers_count;
                    self.Multisig_is_signer.write(signer_to_add, true);
                    self.Multisig_signers_by_index.write(index, signer_to_add);
                    self.Multisig_signers_indices.write(signer_to_add, index);
                    self.emit(SignerAdded { signer: signer_to_add });

                    signers_count += 1;
                }
                self.Multisig_signers_info.write(SignersInfo { quorum, signers_count });
            }
            self._change_quorum(new_quorum);
        }

        /// Removes existing signers and updates the quorum.
        ///
        /// Requirements:
        ///
        /// - `new_quorum` must be non-zero and less than or equal to the total number of signers
        /// after removal.
        ///
        /// Emits a `SignerRemoved` event for each signer removed.
        ///
        /// Emits a `QuorumUpdated` event if the quorum changes.
        fn _remove_signers(
            ref self: ComponentState<TContractState>,
            new_quorum: u32,
            signers_to_remove: Span<ContractAddress>,
        ) {
            if !signers_to_remove.is_empty() {
                let SignersInfo { quorum, mut signers_count } = self.Multisig_signers_info.read();
                for signer in signers_to_remove {
                    let signer_to_remove = *signer;
                    if !self.is_signer(signer_to_remove) {
                        continue;
                    }
                    let last_index = signers_count - 1;
                    let index = self.Multisig_signers_indices.read(signer_to_remove);
                    if index != last_index {
                        // Swap signer to remove with the last signer
                        let last_signer = self.Multisig_signers_by_index.read(last_index);
                        self.Multisig_signers_indices.write(last_signer, index);
                        self.Multisig_signers_by_index.write(index, last_signer);
                    }
                    // Remove the last signer
                    self.Multisig_is_signer.write(signer_to_remove, false);
                    self.Multisig_signers_by_index.write(last_index, Zero::zero());
                    self.Multisig_signers_indices.write(signer_to_remove, 0);
                    self.emit(SignerRemoved { signer: signer_to_remove });

                    signers_count -= 1;
                }
                self.Multisig_signers_info.write(SignersInfo { quorum, signers_count });
            }
            self._change_quorum(new_quorum);
        }

        /// Replaces an existing signer with a new signer.
        ///
        /// Requirements:
        ///
        /// - `signer_to_remove` must be an existing signer.
        /// - `signer_to_add` must not be an existing signer.
        /// - `signer_to_add` must be a non-zero address.
        ///
        /// Emits a `SignerRemoved` event for the removed signer.
        ///
        /// Emits a `SignerAdded` event for the new signer.
        fn _replace_signer(
            ref self: ComponentState<TContractState>,
            signer_to_remove: ContractAddress,
            signer_to_add: ContractAddress,
        ) {
            assert(signer_to_add.is_non_zero(), Errors::ZERO_ADDRESS_SIGNER);
            assert(!self.is_signer(signer_to_add), Errors::ALREADY_A_SIGNER);
            assert(self.is_signer(signer_to_remove), Errors::NOT_A_SIGNER);

            self.Multisig_is_signer.write(signer_to_remove, false);
            self.Multisig_is_signer.write(signer_to_add, true);
            let index = self.Multisig_signers_indices.read(signer_to_remove);
            self.Multisig_signers_by_index.write(index, signer_to_add);
            self.Multisig_signers_indices.write(signer_to_add, index);
            self.Multisig_signers_indices.write(signer_to_remove, 0);
            self.emit(SignerRemoved { signer: signer_to_remove });
            self.emit(SignerAdded { signer: signer_to_add });
        }

        /// Updates the quorum value to `new_quorum` if it differs from the current quorum.
        ///
        /// Requirements:
        ///
        /// - `new_quorum` must be non-zero.
        /// - `new_quorum` must be less than or equal to the total number of signers.
        ///
        /// Emits a `QuorumUpdated` event if the quorum changes.
        fn _change_quorum(ref self: ComponentState<TContractState>, new_quorum: u32) {
            let SignersInfo {
                quorum: old_quorum, signers_count,
            } = self.Multisig_signers_info.read();
            assert(new_quorum.is_non_zero(), Errors::ZERO_QUORUM);
            assert(new_quorum <= signers_count, Errors::QUORUM_TOO_HIGH);
            if new_quorum != old_quorum {
                self.Multisig_signers_info.write(SignersInfo { quorum: new_quorum, signers_count });
                self.emit(QuorumUpdated { old_quorum, new_quorum });
            }
        }
    }
}
