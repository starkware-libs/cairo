// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (governance/src/votes/votes.cairo)

/// # Votes Component
///
/// The Votes component provides a flexible system for tracking and delegating voting power.
/// It is currently implemented for ERC20 and ERC721 tokens. An account can delegate
/// their voting power to a representative, that will pool delegated voting units from different
/// delegators and can then use it to vote in decisions. Voting power must be delegated to be
/// counted, and an account must delegate to itself if it wishes to vote directly without a trusted
/// representative.
///
/// When integrating the Votes component, the ´VotingUnitsTrait´ must be implemented to get the
/// voting units for a given account as a function of the implementing contract. For simplicity,
/// this module already provides two implementations for ERC20 and ERC721 tokens, which will work
/// out of the box if the respective components are integrated.
///
/// NOTE: ERC20 and ERC721 tokens implementing this component must call ´transfer_voting_units´
/// whenever a transfer, mint, or burn operation is performed. Hooks can be leveraged for this
/// purpose, as shown in the following ERC20 example:
///
/// See [the documentation]
/// (https://docs.openzeppelin.com/contracts-cairo/2.0.0/governance.html#usage_2)
/// for examples and more details.
#[starknet::component]
pub mod VotesComponent {
    use core::num::traits::Zero;
    use openzeppelin_account::interface::{ISRC6Dispatcher, ISRC6DispatcherTrait};
    use openzeppelin_introspection::src5::SRC5Component;
    use openzeppelin_token::erc20::ERC20Component;
    use openzeppelin_token::erc20::interface::IERC20;
    use openzeppelin_token::erc721::ERC721Component;
    use openzeppelin_token::erc721::interface::IERC721;
    use openzeppelin_utils::cryptography::snip12::{OffchainMessageHash, SNIP12Metadata};
    use openzeppelin_utils::nonces::NoncesComponent;
    use openzeppelin_utils::nonces::NoncesComponent::InternalTrait as NoncesInternalTrait;
    use openzeppelin_utils::structs::checkpoint::{Checkpoint, Trace, TraceTrait};
    use starknet::ContractAddress;
    use starknet::storage::{Map, StorageMapReadAccess, StorageMapWriteAccess, StoragePathEntry};
    use crate::votes::delegation::Delegation;
    use crate::votes::interface::IVotes;

    #[storage]
    pub struct Storage {
        pub Votes_delegatee: Map<ContractAddress, ContractAddress>,
        pub Votes_delegate_checkpoints: Map<ContractAddress, Trace>,
        pub Votes_total_checkpoints: Trace,
    }

    #[event]
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub enum Event {
        DelegateChanged: DelegateChanged,
        DelegateVotesChanged: DelegateVotesChanged,
    }

    /// Emitted when `delegator` delegates their votes from `from_delegate` to `to_delegate`.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct DelegateChanged {
        #[key]
        pub delegator: ContractAddress,
        #[key]
        pub from_delegate: ContractAddress,
        #[key]
        pub to_delegate: ContractAddress,
    }

    /// Emitted when `delegate` votes are updated from `previous_votes` to `new_votes`.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct DelegateVotesChanged {
        #[key]
        pub delegate: ContractAddress,
        pub previous_votes: u256,
        pub new_votes: u256,
    }

    pub mod Errors {
        pub const FUTURE_LOOKUP: felt252 = 'Votes: future Lookup';
        pub const EXPIRED_SIGNATURE: felt252 = 'Votes: expired signature';
        pub const INVALID_SIGNATURE: felt252 = 'Votes: invalid signature';
    }

    /// A trait that must be implemented when integrating {VotesComponent} into a contract. It
    /// offers a mechanism to retrieve the number of voting units for a given account at the current
    /// time.
    pub trait VotingUnitsTrait<TState> {
        /// Returns the number of voting units for a given account. For ERC20, this is typically the
        /// token balance. For ERC721, this is typically the number of tokens owned.
        ///
        /// WARNING: While any formula can be used as a measure of voting units, the internal vote
        /// accounting of the contract may be compromised if voting units are transferred in any
        /// external flow by following a different formula. For example, when implementing the hook
        /// for ERC20, the number of voting units transferred should match the formula given by the
        /// `get_voting_units` implementation.
        fn get_voting_units(self: @TState, account: ContractAddress) -> u256;
    }

    //
    // External
    //

    #[embeddable_as(VotesImpl)]
    impl Votes<
        TContractState,
        +HasComponent<TContractState>,
        impl Nonces: NoncesComponent::HasComponent<TContractState>,
        +VotingUnitsTrait<ComponentState<TContractState>>,
        +SNIP12Metadata,
        +Drop<TContractState>,
    > of IVotes<ComponentState<TContractState>> {
        /// Returns the current amount of votes that `account` has.
        fn get_votes(self: @ComponentState<TContractState>, account: ContractAddress) -> u256 {
            self.Votes_delegate_checkpoints.entry(account).latest()
        }

        /// Returns the amount of votes that `account` had at a specific moment in the past.
        ///
        /// Requirements:
        ///
        /// - `timepoint` must be in the past.
        fn get_past_votes(
            self: @ComponentState<TContractState>, account: ContractAddress, timepoint: u64,
        ) -> u256 {
            let current_timepoint = starknet::get_block_timestamp();
            assert(timepoint < current_timepoint, Errors::FUTURE_LOOKUP);
            self.Votes_delegate_checkpoints.entry(account).upper_lookup_recent(timepoint)
        }

        /// Returns the total supply of votes available at a specific moment in the past.
        ///
        /// Requirements:
        ///
        /// - `timepoint` must be in the past.
        fn get_past_total_supply(self: @ComponentState<TContractState>, timepoint: u64) -> u256 {
            let current_timepoint = starknet::get_block_timestamp();
            assert(timepoint < current_timepoint, Errors::FUTURE_LOOKUP);
            self.Votes_total_checkpoints.deref().upper_lookup_recent(timepoint)
        }

        /// Returns the delegate that `account` has chosen.
        fn delegates(
            self: @ComponentState<TContractState>, account: ContractAddress,
        ) -> ContractAddress {
            self.Votes_delegatee.read(account)
        }

        /// Delegates votes from the sender to `delegatee`.
        ///
        /// Emits a `DelegateChanged` event.
        /// May emit one or two `DelegateVotesChanged` events.
        fn delegate(ref self: ComponentState<TContractState>, delegatee: ContractAddress) {
            let sender = starknet::get_caller_address();
            self._delegate(sender, delegatee);
        }

        /// Delegates votes from the sender to `delegatee` through a SNIP12 message signature
        /// validation.
        ///
        /// Requirements:
        ///
        /// - `expiry` must not be in the past.
        /// - `nonce` must match the account's current nonce.
        /// - `delegator` must implement `SRC6::is_valid_signature`.
        /// - `signature` should be valid for the message hash.
        ///
        /// Emits a `DelegateChanged` event.
        /// May emit one or two `DelegateVotesChanged` events.
        fn delegate_by_sig(
            ref self: ComponentState<TContractState>,
            delegator: ContractAddress,
            delegatee: ContractAddress,
            nonce: felt252,
            expiry: u64,
            signature: Span<felt252>,
        ) {
            assert(starknet::get_block_timestamp() <= expiry, Errors::EXPIRED_SIGNATURE);

            // Check and increase nonce.
            let mut nonces_component = get_dep_component_mut!(ref self, Nonces);
            nonces_component.use_checked_nonce(delegator, nonce);

            // Build hash for calling `is_valid_signature`.
            let verifying_contract = starknet::get_contract_address();
            let delegation = Delegation { verifying_contract, delegatee, nonce, expiry };
            let hash = delegation.get_message_hash(delegator);

            let is_valid_signature_felt = ISRC6Dispatcher { contract_address: delegator }
                .is_valid_signature(hash, signature.into());

            // Check either 'VALID' or true for backwards compatibility.
            let is_valid_signature = is_valid_signature_felt == starknet::VALIDATED
                || is_valid_signature_felt == 1;

            assert(is_valid_signature, Errors::INVALID_SIGNATURE);

            // Delegate votes.
            self._delegate(delegator, delegatee);
        }
    }

    //
    // Internal
    //

    impl ERC20VotesImpl<
        TContractState,
        +HasComponent<TContractState>,
        impl ERC20: ERC20Component::HasComponent<TContractState>,
        +ERC20Component::ERC20HooksTrait<TContractState>,
    > of VotingUnitsTrait<ComponentState<TContractState>> {
        /// Returns the number of voting units for a given account.
        ///
        /// This implementation is specific to ERC20 tokens, where the balance
        /// of tokens directly represents the number of voting units.
        ///
        /// NOTE: This implementation will work out of the box if the ERC20 component
        /// is implemented in the final contract.
        ///
        /// WARNING: This implementation assumes tokens map to voting units 1:1.
        /// Any deviation from this formula when transferring voting units (e.g. by using hooks)
        /// may compromise the internal vote accounting.
        fn get_voting_units(
            self: @ComponentState<TContractState>, account: ContractAddress,
        ) -> u256 {
            let erc20_component = get_dep_component!(self, ERC20);
            erc20_component.balance_of(account)
        }
    }

    impl ERC721VotesImpl<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        impl ERC721: ERC721Component::HasComponent<TContractState>,
        +ERC721Component::ERC721HooksTrait<TContractState>,
        +Drop<TContractState>,
    > of VotingUnitsTrait<ComponentState<TContractState>> {
        /// Returns the number of voting units for a given account.
        ///
        /// This implementation is specific to ERC721 tokens, where each token
        /// represents one voting unit. The function returns the balance of
        /// ERC721 tokens for the specified account.
        ///
        /// NOTE: This implementation will work out of the box if the ERC721 component
        /// is implemented in the final contract.
        ///
        /// WARNING: This implementation assumes tokens map to voting units 1:1.
        /// Any deviation from this formula when transferring voting units (e.g. by using hooks)
        /// may compromise the internal vote accounting.
        fn get_voting_units(
            self: @ComponentState<TContractState>, account: ContractAddress,
        ) -> u256 {
            let erc721_component = get_dep_component!(self, ERC721);
            erc721_component.balance_of(account).into()
        }
    }

    #[generate_trait]
    pub impl InternalImpl<
        TContractState,
        +HasComponent<TContractState>,
        +VotingUnitsTrait<ComponentState<TContractState>>,
        +NoncesComponent::HasComponent<TContractState>,
        +SNIP12Metadata,
        +Drop<TContractState>,
    > of InternalTrait<TContractState> {
        /// Returns the current total supply of votes.
        fn get_total_supply(self: @ComponentState<TContractState>) -> u256 {
            self.Votes_total_checkpoints.deref().latest()
        }

        /// Moves delegated votes from one delegate to another.
        ///
        /// May emit one or two `DelegateVotesChanged` events.
        fn move_delegate_votes(
            ref self: ComponentState<TContractState>,
            from: ContractAddress,
            to: ContractAddress,
            amount: u256,
        ) {
            let block_timestamp = starknet::get_block_timestamp();
            if from != to && amount > 0 {
                if from.is_non_zero() {
                    let mut trace = self.Votes_delegate_checkpoints.entry(from);
                    let (previous_votes, new_votes) = trace
                        .push(block_timestamp, trace.into().latest() - amount);
                    self.emit(DelegateVotesChanged { delegate: from, previous_votes, new_votes });
                }
                if to.is_non_zero() {
                    let mut trace = self.Votes_delegate_checkpoints.entry(to);
                    let (previous_votes, new_votes) = trace
                        .push(block_timestamp, trace.into().latest() + amount);
                    self.emit(DelegateVotesChanged { delegate: to, previous_votes, new_votes });
                }
            }
        }

        /// Transfers, mints, or burns voting units.
        ///
        /// To register a mint, `from` should be zero. To register a burn, `to`
        /// should be zero. Total supply of voting units will be adjusted with mints and burns.
        ///
        /// WARNING: If voting units are based on an underlying transferable asset (like a token),
        /// you must call this function every time the asset is transferred to keep the internal
        /// voting power accounting in sync. For ERC20 and ERC721 tokens, this is typically handled
        /// using hooks.
        ///
        /// May emit one or two `DelegateVotesChanged` events.
        fn transfer_voting_units(
            ref self: ComponentState<TContractState>,
            from: ContractAddress,
            to: ContractAddress,
            amount: u256,
        ) {
            let block_timestamp = starknet::get_block_timestamp();
            if from.is_zero() {
                let mut trace = self.Votes_total_checkpoints.deref();
                trace.push(block_timestamp, trace.into().latest() + amount);
            }
            if to.is_zero() {
                let mut trace = self.Votes_total_checkpoints.deref();
                trace.push(block_timestamp, trace.into().latest() - amount);
            }
            self.move_delegate_votes(self.delegates(from), self.delegates(to), amount);
        }

        /// Returns the number of checkpoints for `account`.
        fn num_checkpoints(self: @ComponentState<TContractState>, account: ContractAddress) -> u64 {
            self.Votes_delegate_checkpoints.entry(account).length()
        }

        /// Returns the `pos`-th checkpoint for `account`.
        fn checkpoints(
            self: @ComponentState<TContractState>, account: ContractAddress, pos: u64,
        ) -> Checkpoint {
            self.Votes_delegate_checkpoints.entry(account).at(pos)
        }

        /// Delegates all of `account`'s voting units to `delegatee`.
        ///
        /// Emits a `DelegateChanged` event.
        /// May emit one or two `DelegateVotesChanged` events.
        fn _delegate(
            ref self: ComponentState<TContractState>,
            account: ContractAddress,
            delegatee: ContractAddress,
        ) {
            let from_delegate = self.delegates(account);
            self.Votes_delegatee.write(account, delegatee);
            self
                .emit(
                    DelegateChanged { delegator: account, from_delegate, to_delegate: delegatee },
                );
            self.move_delegate_votes(from_delegate, delegatee, self.get_voting_units(account));
        }
    }
}
