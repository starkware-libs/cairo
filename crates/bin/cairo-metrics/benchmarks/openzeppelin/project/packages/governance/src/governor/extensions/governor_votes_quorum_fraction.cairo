// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0
// (governance/src/governor/extensions/governor_votes_quorum_fraction.cairo)

/// # GovernorVotesQuorumFraction Component
///
/// Extension of GovernorComponent for voting weight extraction from a token with the Votes
/// extension and a quorum expressed as a fraction of the total supply.
#[starknet::component]
pub mod GovernorVotesQuorumFractionComponent {
    use core::num::traits::Zero;
    use openzeppelin_introspection::src5::SRC5Component;
    use openzeppelin_utils::structs::checkpoint::{Trace, TraceTrait};
    use starknet::ContractAddress;
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use crate::governor::GovernorComponent;
    use crate::governor::GovernorComponent::ComponentState as GovernorComponentState;
    use crate::governor::extensions::interface::IQuorumFraction;
    use crate::votes::interface::{IVotesDispatcher, IVotesDispatcherTrait};

    #[storage]
    pub struct Storage {
        pub Governor_token: ContractAddress,
        pub Governor_quorum_numerator_history: Trace,
    }

    #[event]
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub enum Event {
        QuorumNumeratorUpdated: QuorumNumeratorUpdated,
    }

    /// Emitted when the quorum numerator is updated.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct QuorumNumeratorUpdated {
        pub old_quorum_numerator: u256,
        pub new_quorum_numerator: u256,
    }

    pub mod Errors {
        pub const INVALID_QUORUM_FRACTION: felt252 = 'Invalid quorum fraction';
        pub const INVALID_TOKEN: felt252 = 'Invalid votes token';
    }

    //
    // Extensions
    //

    pub impl GovernorQuorum<
        TContractState,
        +GovernorComponent::HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        impl GovernorVotesQuorumFraction: HasComponent<TContractState>,
        +Drop<TContractState>,
    > of GovernorComponent::GovernorQuorumTrait<TContractState> {
        /// See `GovernorComponent::GovernorQuorumTrait::quorum`.
        ///
        /// It is computed as a percentage of the votes token total supply at a given timepoint in
        /// the past.
        fn quorum(self: @GovernorComponentState<TContractState>, timepoint: u64) -> u256 {
            let contract = self.get_contract();
            let this_component = GovernorVotesQuorumFraction::get_component(contract);

            let token = this_component.Governor_token.read();
            let votes_dispatcher = IVotesDispatcher { contract_address: token };

            let past_total_supply = votes_dispatcher.get_past_total_supply(timepoint);
            let quorum_numerator = this_component.quorum_numerator(timepoint);
            let quorum_denominator = this_component.quorum_denominator();

            past_total_supply * quorum_numerator / quorum_denominator
        }
    }

    pub impl GovernorVotes<
        TContractState,
        +GovernorComponent::HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        impl GovernorVotesQuorumFraction: HasComponent<TContractState>,
        +Drop<TContractState>,
    > of GovernorComponent::GovernorVotesTrait<TContractState> {
        /// See `GovernorComponent::GovernorVotesTrait::clock`.
        fn clock(self: @GovernorComponentState<TContractState>) -> u64 {
            // VotesComponent uses the block timestamp for tracking checkpoints.
            // That must be updated in order to allow for more flexible clock modes.
            starknet::get_block_timestamp()
        }

        /// See `GovernorComponent::GovernorVotesTrait::CLOCK_MODE`.
        fn clock_mode(self: @GovernorComponentState<TContractState>) -> ByteArray {
            "mode=timestamp&from=starknet::SN_MAIN"
        }

        /// See `GovernorComponent::GovernorVotesTrait::get_votes`.
        fn get_votes(
            self: @GovernorComponentState<TContractState>,
            account: ContractAddress,
            timepoint: u64,
            params: Span<felt252>,
        ) -> u256 {
            let contract = self.get_contract();
            let this_component = GovernorVotesQuorumFraction::get_component(contract);

            let token = this_component.Governor_token.read();
            let votes_dispatcher = IVotesDispatcher { contract_address: token };

            votes_dispatcher.get_past_votes(account, timepoint)
        }
    }

    //
    // External
    //

    #[embeddable_as(QuorumFractionImpl)]
    impl QuorumFraction<
        TContractState, +HasComponent<TContractState>, +Drop<TContractState>,
    > of IQuorumFraction<ComponentState<TContractState>> {
        /// Returns the token that voting power is sourced from.
        fn token(self: @ComponentState<TContractState>) -> ContractAddress {
            self.Governor_token.read()
        }

        /// Returns the current quorum numerator.
        fn current_quorum_numerator(self: @ComponentState<TContractState>) -> u256 {
            self.Governor_quorum_numerator_history.deref().latest()
        }

        /// Returns the quorum numerator at a specific timepoint.
        fn quorum_numerator(self: @ComponentState<TContractState>, timepoint: u64) -> u256 {
            // Optimistic search: check the latest checkpoint.
            // The initializer call ensures that there is at least one checkpoint in the history.
            //
            // NOTE: This optimization is especially helpful when the supply is not updated often.
            let history = self.Governor_quorum_numerator_history.deref();
            let (_, key, value) = history.latest_checkpoint();

            if key <= timepoint {
                return value;
            }

            // Fallback to the binary search
            history.upper_lookup(timepoint)
        }

        /// Returns the quorum denominator.
        fn quorum_denominator(self: @ComponentState<TContractState>) -> u256 {
            1000
        }
    }

    //
    // Internal
    //

    #[generate_trait]
    pub impl InternalImpl<
        TContractState,
        +HasComponent<TContractState>,
        +GovernorComponent::GovernorVotesTrait<TContractState>,
        impl Governor: GovernorComponent::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of InternalTrait<TContractState> {
        /// Initializes the component by setting the votes token and the
        /// initial quorum numerator value.
        ///
        /// Requirements:
        ///
        /// - `votes_token` must not be zero.
        /// - `quorum_numerator` must be less than `quorum_denominator`.
        ///
        /// Emits a `QuorumNumeratorUpdated` event.
        fn initializer(
            ref self: ComponentState<TContractState>,
            votes_token: ContractAddress,
            quorum_numerator: u256,
        ) {
            assert(votes_token.is_non_zero(), Errors::INVALID_TOKEN);

            self.Governor_token.write(votes_token);
            self.update_quorum_numerator(quorum_numerator);
        }

        /// Updates the quorum numerator.
        ///
        /// NOTE: This function does not emit an event if the new quorum numerator is the same as
        /// the old one.
        ///
        /// Requirements:
        ///
        /// - `new_quorum_numerator` must be less than `quorum_denominator`.
        ///
        /// May emit a `QuorumNumeratorUpdated` event.
        fn update_quorum_numerator(
            ref self: ComponentState<TContractState>, new_quorum_numerator: u256,
        ) {
            let denominator = self.quorum_denominator();

            assert(new_quorum_numerator <= denominator, Errors::INVALID_QUORUM_FRACTION);

            let old_quorum_numerator = self.current_quorum_numerator();

            if old_quorum_numerator != new_quorum_numerator {
                let governor_component = get_dep_component!(@self, Governor);

                let clock = governor_component.clock();

                self.Governor_quorum_numerator_history.deref().push(clock, new_quorum_numerator);

                self.emit(QuorumNumeratorUpdated { old_quorum_numerator, new_quorum_numerator });
            }
        }
    }
}
