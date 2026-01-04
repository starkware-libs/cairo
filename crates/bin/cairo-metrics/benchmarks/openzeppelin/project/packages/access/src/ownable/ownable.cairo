// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (access/src/ownable/ownable.cairo)

/// # Ownable Component
///
/// The Ownable component provides a basic access control mechanism, where
/// there is an account (an owner) that can be granted exclusive access to
/// specific functions.
///
/// The initial owner can be set by using the `initializer` function in
/// construction time. This can later be changed with `transfer_ownership`.
///
/// The component also offers functionality for a two-step ownership
/// transfer where the new owner first has to accept their ownership to
/// finalize the transfer.
#[starknet::component]
pub mod OwnableComponent {
    use core::num::traits::Zero;
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use starknet::{ContractAddress, get_caller_address};
    use crate::ownable::interface;
    use crate::ownable::interface::IOwnableTwoStep;

    #[storage]
    pub struct Storage {
        pub Ownable_owner: ContractAddress,
        pub Ownable_pending_owner: ContractAddress,
    }

    #[event]
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub enum Event {
        OwnershipTransferred: OwnershipTransferred,
        OwnershipTransferStarted: OwnershipTransferStarted,
    }

    /// Emitted when `new_owner` is set as owner of the contract.
    /// `new_owner` can be set to zero only if the ownership is renounced.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct OwnershipTransferred {
        #[key]
        pub previous_owner: ContractAddress,
        #[key]
        pub new_owner: ContractAddress,
    }

    /// Emitted when `transfer_ownership` is called on a contract that implements `IOwnableTwoStep`.
    /// `previous_owner` is the address of the current owner.
    /// `new_owner` is the address of the pending owner.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct OwnershipTransferStarted {
        #[key]
        pub previous_owner: ContractAddress,
        #[key]
        pub new_owner: ContractAddress,
    }

    pub mod Errors {
        pub const NOT_OWNER: felt252 = 'Caller is not the owner';
        pub const NOT_PENDING_OWNER: felt252 = 'Caller is not the pending owner';
        pub const ZERO_ADDRESS_OWNER: felt252 = 'New owner is the zero address';
    }

    #[embeddable_as(OwnableImpl)]
    impl Ownable<
        TContractState, +HasComponent<TContractState>,
    > of interface::IOwnable<ComponentState<TContractState>> {
        /// Returns the address of the current owner.
        fn owner(self: @ComponentState<TContractState>) -> ContractAddress {
            self.Ownable_owner.read()
        }

        /// Transfers ownership of the contract to a new address.
        ///
        /// Requirements:
        ///
        /// - `new_owner` is not the zero address.
        /// - The caller is the contract owner.
        ///
        /// Emits an `OwnershipTransferred` event.
        fn transfer_ownership(
            ref self: ComponentState<TContractState>, new_owner: ContractAddress,
        ) {
            assert(!new_owner.is_zero(), Errors::ZERO_ADDRESS_OWNER);
            self.assert_only_owner();
            self._transfer_ownership(new_owner);
        }

        /// Leaves the contract without an owner. It will not be possible to call
        /// `assert_only_owner` functions anymore. Can only be called by the current owner.
        ///
        /// Requirements:
        ///
        /// - The caller is the contract owner.
        ///
        /// Emits an `OwnershipTransferred` event.
        fn renounce_ownership(ref self: ComponentState<TContractState>) {
            self.assert_only_owner();
            self._transfer_ownership(Zero::zero());
        }
    }

    /// Adds support for two step ownership transfer.
    #[embeddable_as(OwnableTwoStepImpl)]
    impl OwnableTwoStep<
        TContractState, +HasComponent<TContractState>,
    > of interface::IOwnableTwoStep<ComponentState<TContractState>> {
        /// Returns the address of the current owner.
        fn owner(self: @ComponentState<TContractState>) -> ContractAddress {
            self.Ownable_owner.read()
        }

        /// Returns the address of the pending owner.
        fn pending_owner(self: @ComponentState<TContractState>) -> ContractAddress {
            self.Ownable_pending_owner.read()
        }

        /// Finishes the two-step ownership transfer process by accepting the ownership.
        /// Can only be called by the pending owner.
        ///
        /// Requirements:
        ///
        /// - The caller is the pending owner.
        ///
        /// Emits an `OwnershipTransferred` event.
        fn accept_ownership(ref self: ComponentState<TContractState>) {
            let caller = get_caller_address();
            let pending_owner = self.Ownable_pending_owner.read();
            assert(caller == pending_owner, Errors::NOT_PENDING_OWNER);
            self._transfer_ownership(pending_owner);
        }

        /// Starts the two-step ownership transfer process by setting the pending owner.
        ///
        /// Setting `new_owner` to the zero address is allowed; this can be used to cancel an
        /// initiated ownership transfer.
        /// Requirements:
        ///
        /// - The caller is the contract owner.
        ///
        /// Emits an `OwnershipTransferStarted` event.
        fn transfer_ownership(
            ref self: ComponentState<TContractState>, new_owner: ContractAddress,
        ) {
            self.assert_only_owner();
            self._propose_owner(new_owner);
        }

        /// Leaves the contract without owner. It will not be possible to call `assert_only_owner`
        /// functions anymore. Can only be called by the current owner.
        ///
        /// Requirements:
        ///
        /// - The caller is the contract owner.
        ///
        /// Emits an `OwnershipTransferred` event.
        fn renounce_ownership(ref self: ComponentState<TContractState>) {
            self.assert_only_owner();
            self._transfer_ownership(Zero::zero());
        }
    }

    /// Adds camelCase support for `IOwnable`.
    #[embeddable_as(OwnableCamelOnlyImpl)]
    impl OwnableCamelOnly<
        TContractState, +HasComponent<TContractState>,
    > of interface::IOwnableCamelOnly<ComponentState<TContractState>> {
        fn transferOwnership(ref self: ComponentState<TContractState>, newOwner: ContractAddress) {
            Ownable::transfer_ownership(ref self, newOwner);
        }

        fn renounceOwnership(ref self: ComponentState<TContractState>) {
            Ownable::renounce_ownership(ref self);
        }
    }

    /// Adds camelCase support for `IOwnableTwoStep`.
    #[embeddable_as(OwnableTwoStepCamelOnlyImpl)]
    impl OwnableTwoStepCamelOnly<
        TContractState, +HasComponent<TContractState>,
    > of interface::IOwnableTwoStepCamelOnly<ComponentState<TContractState>> {
        fn pendingOwner(self: @ComponentState<TContractState>) -> ContractAddress {
            OwnableTwoStep::pending_owner(self)
        }

        fn acceptOwnership(ref self: ComponentState<TContractState>) {
            self.accept_ownership();
        }

        fn transferOwnership(ref self: ComponentState<TContractState>, newOwner: ContractAddress) {
            OwnableTwoStep::transfer_ownership(ref self, newOwner);
        }

        fn renounceOwnership(ref self: ComponentState<TContractState>) {
            OwnableTwoStep::renounce_ownership(ref self);
        }
    }

    #[embeddable_as(OwnableMixinImpl)]
    impl OwnableMixin<
        TContractState, +HasComponent<TContractState>, +Drop<TContractState>,
    > of interface::OwnableABI<ComponentState<TContractState>> {
        // IOwnable
        fn owner(self: @ComponentState<TContractState>) -> ContractAddress {
            Ownable::owner(self)
        }

        fn transfer_ownership(
            ref self: ComponentState<TContractState>, new_owner: ContractAddress,
        ) {
            Ownable::transfer_ownership(ref self, new_owner);
        }

        fn renounce_ownership(ref self: ComponentState<TContractState>) {
            Ownable::renounce_ownership(ref self);
        }

        // IOwnableCamelOnly
        fn transferOwnership(ref self: ComponentState<TContractState>, newOwner: ContractAddress) {
            OwnableCamelOnly::transferOwnership(ref self, newOwner);
        }

        fn renounceOwnership(ref self: ComponentState<TContractState>) {
            OwnableCamelOnly::renounceOwnership(ref self);
        }
    }

    #[embeddable_as(OwnableTwoStepMixinImpl)]
    impl OwnableTwoStepMixin<
        TContractState, +HasComponent<TContractState>, +Drop<TContractState>,
    > of interface::OwnableTwoStepABI<ComponentState<TContractState>> {
        // IOwnableTwoStep
        fn owner(self: @ComponentState<TContractState>) -> ContractAddress {
            OwnableTwoStep::owner(self)
        }

        fn pending_owner(self: @ComponentState<TContractState>) -> ContractAddress {
            OwnableTwoStep::pending_owner(self)
        }

        fn accept_ownership(ref self: ComponentState<TContractState>) {
            OwnableTwoStep::accept_ownership(ref self);
        }

        fn transfer_ownership(
            ref self: ComponentState<TContractState>, new_owner: ContractAddress,
        ) {
            OwnableTwoStep::transfer_ownership(ref self, new_owner);
        }

        fn renounce_ownership(ref self: ComponentState<TContractState>) {
            OwnableTwoStep::renounce_ownership(ref self);
        }

        // IOwnableTwoStepCamelOnly
        fn pendingOwner(self: @ComponentState<TContractState>) -> ContractAddress {
            OwnableTwoStepCamelOnly::pendingOwner(self)
        }

        fn acceptOwnership(ref self: ComponentState<TContractState>) {
            OwnableTwoStepCamelOnly::acceptOwnership(ref self);
        }

        fn transferOwnership(ref self: ComponentState<TContractState>, newOwner: ContractAddress) {
            OwnableTwoStepCamelOnly::transferOwnership(ref self, newOwner);
        }

        fn renounceOwnership(ref self: ComponentState<TContractState>) {
            OwnableTwoStepCamelOnly::renounceOwnership(ref self);
        }
    }

    #[generate_trait]
    pub impl InternalImpl<
        TContractState, +HasComponent<TContractState>,
    > of InternalTrait<TContractState> {
        /// Sets the contract's initial owner.
        ///
        /// Requirements:
        ///
        /// - `owner` is not the zero address.
        ///
        /// This function should be called at construction time.
        fn initializer(ref self: ComponentState<TContractState>, owner: ContractAddress) {
            assert(!owner.is_zero(), Errors::ZERO_ADDRESS_OWNER);
            self._transfer_ownership(owner);
        }

        /// Panics if called by any account other than the owner. Use this
        /// to restrict access to certain functions to the owner.
        fn assert_only_owner(self: @ComponentState<TContractState>) {
            let owner = self.Ownable_owner.read();
            let caller = get_caller_address();
            assert(caller == owner, Errors::NOT_OWNER);
        }

        /// Transfers ownership of the contract to a new address and resets
        /// the pending owner to the zero address.
        ///
        /// Internal function without access restriction.
        ///
        /// Emits an `OwnershipTransferred` event.
        fn _transfer_ownership(
            ref self: ComponentState<TContractState>, new_owner: ContractAddress,
        ) {
            self.Ownable_pending_owner.write(Zero::zero());

            let previous_owner: ContractAddress = self.Ownable_owner.read();
            self.Ownable_owner.write(new_owner);
            self.emit(OwnershipTransferred { previous_owner, new_owner });
        }

        /// Sets a new pending owner.
        ///
        /// Internal function without access restriction.
        ///
        /// Emits an `OwnershipTransferStarted` event.
        fn _propose_owner(ref self: ComponentState<TContractState>, new_owner: ContractAddress) {
            let previous_owner = self.Ownable_owner.read();
            self.Ownable_pending_owner.write(new_owner);
            self.emit(OwnershipTransferStarted { previous_owner, new_owner });
        }
    }
}
