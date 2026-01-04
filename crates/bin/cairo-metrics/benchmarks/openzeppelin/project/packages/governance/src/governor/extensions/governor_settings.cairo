// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0
// (governance/src/governor/extensions/governor_settings.cairo)

/// # GovernorSettings Component
///
/// Extension of GovernorComponent for settings that are updatable through governance.
#[starknet::component]
pub mod GovernorSettingsComponent {
    use openzeppelin_introspection::src5::SRC5Component;
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use crate::governor::GovernorComponent;
    use crate::governor::GovernorComponent::{
        ComponentState as GovernorComponentState, InternalExtendedTrait,
    };
    use crate::governor::extensions::interface::IGovernorSettingsAdmin;

    #[storage]
    pub struct Storage {
        pub Governor_voting_delay: u64,
        pub Governor_voting_period: u64,
        pub Governor_proposal_threshold: u256,
    }

    #[event]
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub enum Event {
        VotingDelayUpdated: VotingDelayUpdated,
        VotingPeriodUpdated: VotingPeriodUpdated,
        ProposalThresholdUpdated: ProposalThresholdUpdated,
    }

    /// Emitted when `Governor_voting_delay` is updated.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct VotingDelayUpdated {
        pub old_voting_delay: u64,
        pub new_voting_delay: u64,
    }

    /// Emitted when `Governor_voting_period` is updated.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct VotingPeriodUpdated {
        pub old_voting_period: u64,
        pub new_voting_period: u64,
    }

    /// Emitted when `Governor_proposal_threshold` is updated.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct ProposalThresholdUpdated {
        pub old_proposal_threshold: u256,
        pub new_proposal_threshold: u256,
    }

    pub mod Errors {
        pub const INVALID_VOTING_PERIOD: felt252 = 'Invalid voting period';
    }

    //
    // Extensions
    //

    pub impl GovernorSettings<
        TContractState,
        +GovernorComponent::HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        impl GovernorSettings: HasComponent<TContractState>,
        +Drop<TContractState>,
    > of GovernorComponent::GovernorSettingsTrait<TContractState> {
        /// See `GovernorComponent::GovernorSettingsTrait::voting_delay`.
        fn voting_delay(self: @GovernorComponentState<TContractState>) -> u64 {
            let contract = self.get_contract();
            let this_component = GovernorSettings::get_component(contract);

            this_component.Governor_voting_delay.read()
        }

        /// See `GovernorComponent::GovernorSettingsTrait::voting_period`.
        fn voting_period(self: @GovernorComponentState<TContractState>) -> u64 {
            let contract = self.get_contract();
            let this_component = GovernorSettings::get_component(contract);

            this_component.Governor_voting_period.read()
        }

        /// See `GovernorComponent::GovernorSettingsTrait::proposal_threshold`.
        fn proposal_threshold(self: @GovernorComponentState<TContractState>) -> u256 {
            let contract = self.get_contract();
            let this_component = GovernorSettings::get_component(contract);

            this_component.Governor_proposal_threshold.read()
        }
    }

    //
    // External
    //

    #[embeddable_as(GovernorSettingsAdminImpl)]
    impl GovernorSettingsAdmin<
        TContractState,
        +HasComponent<TContractState>,
        +GovernorComponent::HasComponent<TContractState>,
        +GovernorComponent::GovernorCountingTrait<TContractState>,
        +GovernorComponent::GovernorExecutionTrait<TContractState>,
        +GovernorComponent::GovernorVotesTrait<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of IGovernorSettingsAdmin<ComponentState<TContractState>> {
        /// Sets the voting delay.
        ///
        /// Requirements:
        ///
        /// - Caller must be the governance executor.
        ///
        /// NOTE: This function does not emit an event if the new voting delay is the same as the
        /// old one.
        ///
        /// May emit a `VotingDelayUpdated` event.
        fn set_voting_delay(ref self: ComponentState<TContractState>, new_voting_delay: u64) {
            self.assert_only_governance();
            self._set_voting_delay(new_voting_delay);
        }

        /// Sets the voting period.
        ///
        /// NOTE: This function does not emit an event if the new voting period is the same as the
        /// old one.
        ///
        /// Requirements:
        ///
        /// - Caller must be the governance executor.
        /// - `new_voting_period` must be greater than 0.
        ///
        /// May emit a `VotingPeriodUpdated` event.
        fn set_voting_period(ref self: ComponentState<TContractState>, new_voting_period: u64) {
            self.assert_only_governance();
            self._set_voting_period(new_voting_period);
        }

        /// Sets the proposal threshold.
        ///
        /// NOTE: This function does not emit an event if the new proposal threshold is the same as
        /// the old one.
        ///
        /// Requirements:
        ///
        /// - Caller must be the governance executor.
        ///
        /// May emit a `ProposalThresholdUpdated` event.
        fn set_proposal_threshold(
            ref self: ComponentState<TContractState>, new_proposal_threshold: u256,
        ) {
            self.assert_only_governance();
            self._set_proposal_threshold(new_proposal_threshold);
        }
    }

    //
    // Internal
    //

    #[generate_trait]
    pub impl InternalImpl<
        TContractState,
        +HasComponent<TContractState>,
        +GovernorComponent::GovernorCountingTrait<TContractState>,
        +GovernorComponent::GovernorExecutionTrait<TContractState>,
        +GovernorComponent::GovernorVotesTrait<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        impl Governor: GovernorComponent::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of InternalTrait<TContractState> {
        /// Initializes the component by setting the default values.
        ///
        /// Requirements:
        ///
        /// - `new_voting_period` must be greater than 0.
        ///
        /// Emits a `VotingDelayUpdated`, `VotingPeriodUpdated`, and `ProposalThresholdUpdated`
        /// event.
        fn initializer(
            ref self: ComponentState<TContractState>,
            new_voting_delay: u64,
            new_voting_period: u64,
            new_proposal_threshold: u256,
        ) {
            self._set_voting_delay(new_voting_delay);
            self._set_voting_period(new_voting_period);
            self._set_proposal_threshold(new_proposal_threshold);
        }

        /// Wrapper for `Governor::assert_only_governance`.
        fn assert_only_governance(self: @ComponentState<TContractState>) {
            let governor_component = get_dep_component!(self, Governor);
            governor_component.assert_only_governance();
        }

        /// Internal function to update the voting delay.
        ///
        /// NOTE: This function does not emit an event if the new voting delay is the same as the
        /// old one.
        ///
        /// May emit a `VotingDelayUpdated` event.
        fn _set_voting_delay(ref self: ComponentState<TContractState>, new_voting_delay: u64) {
            let old_voting_delay = self.Governor_voting_delay.read();
            if old_voting_delay != new_voting_delay {
                self.emit(VotingDelayUpdated { old_voting_delay, new_voting_delay });
                self.Governor_voting_delay.write(new_voting_delay);
            }
        }

        /// Internal function to update the voting period.
        ///
        /// Requirements:
        ///
        /// - `new_voting_period` must be greater than 0.
        ///
        /// NOTE: This function does not emit an event if the new voting period is the same as the
        /// old one.
        ///
        /// May emit a `VotingPeriodUpdated` event.
        fn _set_voting_period(ref self: ComponentState<TContractState>, new_voting_period: u64) {
            assert(new_voting_period > 0, Errors::INVALID_VOTING_PERIOD);

            let old_voting_period = self.Governor_voting_period.read();
            if old_voting_period != new_voting_period {
                self.emit(VotingPeriodUpdated { old_voting_period, new_voting_period });
                self.Governor_voting_period.write(new_voting_period);
            }
        }

        /// Internal function to update the proposal threshold.
        ///
        /// NOTE: This function does not emit an event if the new proposal threshold is the same as
        /// the old one.
        ///
        /// May emit a `ProposalThresholdUpdated` event.
        fn _set_proposal_threshold(
            ref self: ComponentState<TContractState>, new_proposal_threshold: u256,
        ) {
            let old_proposal_threshold = self.Governor_proposal_threshold.read();
            if old_proposal_threshold != new_proposal_threshold {
                let event = ProposalThresholdUpdated {
                    old_proposal_threshold, new_proposal_threshold,
                };
                self.emit(event);
                self.Governor_proposal_threshold.write(new_proposal_threshold);
            }
        }
    }
}
