// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0
// (governance/src/governor/extensions/governor_counting_simple.cairo)

/// # GovernorCountingSimple Component
///
/// Extension of GovernorComponent for simple vote counting with three options.
#[starknet::component]
pub mod GovernorCountingSimpleComponent {
    use openzeppelin_introspection::src5::SRC5Component;
    use starknet::ContractAddress;
    use starknet::storage::{
        Map, StorageMapReadAccess, StorageMapWriteAccess, StoragePathEntry,
        StoragePointerReadAccess, StoragePointerWriteAccess,
    };
    use crate::governor::GovernorComponent;
    use crate::governor::GovernorComponent::{
        ComponentState as GovernorComponentState, InternalTrait,
    };

    type ProposalId = felt252;

    #[storage]
    pub struct Storage {
        pub Governor_proposals_votes: Map<ProposalId, ProposalVote>,
    }

    /// Supported vote types.
    #[derive(Drop, PartialEq, Debug)]
    pub enum VoteType {
        Against,
        For,
        Abstain,
    }

    impl U8TryIntoVoteType of TryInto<u8, VoteType> {
        fn try_into(self: u8) -> Option<VoteType> {
            match self {
                0 => Option::Some(VoteType::Against),
                1 => Option::Some(VoteType::For),
                2 => Option::Some(VoteType::Abstain),
                _ => Option::None,
            }
        }
    }

    impl VoteTypeIntoU8 of Into<VoteType, u8> {
        fn into(self: VoteType) -> u8 {
            match self {
                VoteType::Against => 0,
                VoteType::For => 1,
                VoteType::Abstain => 2,
            }
        }
    }

    #[starknet::storage_node]
    pub struct ProposalVote {
        pub against_votes: u256,
        pub for_votes: u256,
        pub abstain_votes: u256,
        pub has_voted: Map<ContractAddress, bool>,
    }

    pub mod Errors {
        pub const ALREADY_CAST_VOTE: felt252 = 'Already cast vote';
        pub const INVALID_VOTE_TYPE: felt252 = 'Invalid vote type';
    }

    //
    // Extensions
    //

    pub impl GovernorCounting<
        TContractState,
        +GovernorComponent::HasComponent<TContractState>,
        +GovernorComponent::GovernorQuorumTrait<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        impl GovernorCountingSimple: HasComponent<TContractState>,
        +Drop<TContractState>,
    > of GovernorComponent::GovernorCountingTrait<TContractState> {
        /// See `GovernorComponent::GovernorCountingTrait::counting_mode`.
        fn counting_mode(self: @GovernorComponentState<TContractState>) -> ByteArray {
            return "support=bravo&quorum=for,abstain";
        }

        /// See `GovernorComponent::GovernorCountingTrait::count_vote`.
        ///
        /// In this module, the support follows the `VoteType` enum (from Governor Bravo).
        fn count_vote(
            ref self: GovernorComponentState<TContractState>,
            proposal_id: felt252,
            account: ContractAddress,
            support: u8,
            total_weight: u256,
            params: Span<felt252>,
        ) -> u256 {
            let mut contract = self.get_contract_mut();
            let mut this_component = GovernorCountingSimple::get_component_mut(ref contract);

            let proposal_votes = this_component.Governor_proposals_votes.entry(proposal_id);
            assert(!proposal_votes.has_voted.read(account), Errors::ALREADY_CAST_VOTE);

            proposal_votes.has_voted.write(account, true);

            let support: VoteType = support.try_into().expect(Errors::INVALID_VOTE_TYPE);
            match support {
                VoteType::Against => {
                    let current_votes = proposal_votes.against_votes.read();
                    proposal_votes.against_votes.write(current_votes + total_weight);
                },
                VoteType::For => {
                    let current_votes = proposal_votes.for_votes.read();
                    proposal_votes.for_votes.write(current_votes + total_weight);
                },
                VoteType::Abstain => {
                    let current_votes = proposal_votes.abstain_votes.read();
                    proposal_votes.abstain_votes.write(current_votes + total_weight);
                },
            }
            total_weight
        }

        /// See `GovernorComponent::GovernorCountingTrait::has_voted`.
        fn has_voted(
            self: @GovernorComponentState<TContractState>,
            proposal_id: felt252,
            account: ContractAddress,
        ) -> bool {
            let contract = self.get_contract();
            let this_component = GovernorCountingSimple::get_component(contract);
            let proposal_votes = this_component.Governor_proposals_votes.entry(proposal_id);

            proposal_votes.has_voted.read(account)
        }

        /// See `GovernorComponent::GovernorCountingTrait::quorum_reached`.
        ///
        /// In this implementation, both For and Abstain votes count toward quorum.
        fn quorum_reached(
            self: @GovernorComponentState<TContractState>, proposal_id: felt252,
        ) -> bool {
            let contract = self.get_contract();
            let this_component = GovernorCountingSimple::get_component(contract);

            let proposal_votes = this_component.Governor_proposals_votes.entry(proposal_id);
            let snapshot = self._proposal_snapshot(proposal_id);

            self.quorum(snapshot) <= proposal_votes.for_votes.read()
                + proposal_votes.abstain_votes.read()
        }

        /// See `GovernorComponent::GovernorCountingTrait::vote_succeeded`.
        ///
        /// In this module, the `for_votes` must be strictly over the `against_votes`.
        fn vote_succeeded(
            self: @GovernorComponentState<TContractState>, proposal_id: felt252,
        ) -> bool {
            let contract = self.get_contract();
            let this_component = GovernorCountingSimple::get_component(contract);
            let proposal_votes = this_component.Governor_proposals_votes.entry(proposal_id);

            proposal_votes.for_votes.read() > proposal_votes.against_votes.read()
        }
    }
}
