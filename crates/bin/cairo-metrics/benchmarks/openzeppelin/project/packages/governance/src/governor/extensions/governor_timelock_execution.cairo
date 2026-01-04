// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0
// (governance/src/governor/extensions/governor_timelock_execution.cairo)

/// # GovernorTimelockExecution Component
///
/// Extension of GovernorComponent that binds the execution process to an instance of a contract
/// implementing TimelockControllerComponent. This adds a delay, enforced by the TimelockController
/// to all successful proposals (in addition to the voting duration).
///
/// NOTE: The Governor needs the PROPOSER, EXECUTOR, and CANCELLER roles to work properly.
///
/// Using this model means the proposal will be operated by the TimelockController and not by the
/// Governor. Thus, the assets and permissions must be attached to the TimelockController. Any asset
/// sent to the Governor will be inaccessible from a proposal, unless executed via
/// `Governor::relay`.
///
/// WARNING: Setting up the TimelockController to have additional proposers or cancellers besides
/// the governor is very risky, as it grants them the ability to: 1) execute operations as the
/// timelock, and thus possibly performing operations or accessing funds that are expected to only
/// be accessible through a vote, and 2) block governance proposals that have been approved by the
/// voters, effectively executing a Denial of Service attack.
#[starknet::component]
pub mod GovernorTimelockExecutionComponent {
    use core::num::traits::Zero;
    use openzeppelin_introspection::src5::SRC5Component;
    use starknet::ContractAddress;
    use starknet::account::Call;
    use starknet::storage::{
        Map, StorageMapReadAccess, StorageMapWriteAccess, StoragePointerReadAccess,
        StoragePointerWriteAccess,
    };
    use crate::governor::GovernorComponent;
    use crate::governor::GovernorComponent::{
        ComponentState as GovernorComponentState, InternalExtendedTrait,
    };
    use crate::governor::extensions::interface::ITimelocked;
    use crate::governor::interface::ProposalState;
    use crate::timelock::interface::{ITimelockDispatcher, ITimelockDispatcherTrait, OperationState};

    type ProposalId = felt252;
    type TimelockProposalId = felt252;

    /// This is P - 1 = 2**251 + 17 * 2 ** 192
    const MAX_FELT: felt252 = 0 - 1;

    #[storage]
    pub struct Storage {
        pub Governor_timelock_controller: ContractAddress,
        pub Governor_timelock_ids: Map<ProposalId, TimelockProposalId>,
    }

    #[event]
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub enum Event {
        TimelockUpdated: TimelockUpdated,
    }

    /// Emitted when the timelock controller used for proposal execution is modified.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct TimelockUpdated {
        pub old_timelock: ContractAddress,
        pub new_timelock: ContractAddress,
    }

    pub mod Errors {
        pub const INVALID_TIMELOCK_CONTROLLER: felt252 = 'Invalid timelock controller';
    }

    //
    // Extensions
    //

    /// NOTE: Some of these functions can reenter through the external calls to the timelock, but we
    /// assume the timelock is trusted and well behaved (according to TimelockController) and this
    /// will not happen.
    pub impl GovernorExecution<
        TContractState,
        +GovernorComponent::HasComponent<TContractState>,
        +GovernorComponent::GovernorSettingsTrait<TContractState>,
        +GovernorComponent::GovernorCountingTrait<TContractState>,
        +GovernorComponent::GovernorVotesTrait<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        impl GovernorCoreExecution: HasComponent<TContractState>,
        +Drop<TContractState>,
    > of GovernorComponent::GovernorExecutionTrait<TContractState> {
        /// See `GovernorComponent::GovernorExecutionTrait::state`.
        ///
        /// Requirements:
        ///
        /// - The proposal must exist.
        fn state(
            self: @GovernorComponentState<TContractState>, proposal_id: felt252,
        ) -> ProposalState {
            let current_state = self._state(proposal_id);

            if current_state != ProposalState::Queued {
                return current_state;
            }

            let contract = self.get_contract();
            let this_component = GovernorCoreExecution::get_component(contract);

            let queue_id = this_component.Governor_timelock_ids.read(proposal_id);
            let timelock_dispatcher = this_component.get_timelock_dispatcher();
            let operation_state = timelock_dispatcher.get_operation_state(queue_id);

            let is_timelock_operation_pending = operation_state == OperationState::Waiting
                || operation_state == OperationState::Ready;
            let is_timelock_operation_done = operation_state == OperationState::Done;

            if is_timelock_operation_pending {
                ProposalState::Queued
            } else if is_timelock_operation_done {
                // This can happen if the proposal is executed directly on the timelock.
                ProposalState::Executed
            } else {
                // This can happen if the proposal is canceled directly on the timelock.
                ProposalState::Canceled
            }
        }

        /// See `GovernorComponent::GovernorExecutionTrait::executor`.
        ///
        /// In this module, the executor is the timelock controller.
        fn executor(self: @GovernorComponentState<TContractState>) -> ContractAddress {
            let contract = self.get_contract();
            let this_component = GovernorCoreExecution::get_component(contract);

            this_component.timelock()
        }

        /// See `GovernorComponent::GovernorExecutionTrait::execute_operations`.
        ///
        /// Runs the already queued proposal through the timelock.
        fn execute_operations(
            ref self: GovernorComponentState<TContractState>,
            proposal_id: felt252,
            calls: Span<Call>,
            description_hash: felt252,
        ) {
            let mut contract = self.get_contract_mut();
            let mut this_component = GovernorCoreExecution::get_component_mut(ref contract);

            let timelock_dispatcher = this_component.get_timelock_dispatcher();

            timelock_dispatcher
                .execute_batch(calls, 0, this_component.timelock_salt(description_hash));

            // Cleanup
            this_component.Governor_timelock_ids.write(proposal_id, 0);
        }

        /// See `GovernorComponent::GovernorExecutionTrait::queue_operations`.
        ///
        /// Queue a proposal to the timelock.
        fn queue_operations(
            ref self: GovernorComponentState<TContractState>,
            proposal_id: felt252,
            calls: Span<Call>,
            description_hash: felt252,
        ) -> u64 {
            let mut contract = self.get_contract_mut();
            let mut this_component = GovernorCoreExecution::get_component_mut(ref contract);

            let timelock_dispatcher = this_component.get_timelock_dispatcher();

            let delay = timelock_dispatcher.get_min_delay();
            let salt = this_component.timelock_salt(description_hash);

            let queue_id = timelock_dispatcher.hash_operation_batch(calls, 0, salt);
            this_component.Governor_timelock_ids.write(proposal_id, queue_id);

            timelock_dispatcher.schedule_batch(calls, 0, salt, delay);

            starknet::get_block_timestamp() + delay
        }

        /// See `GovernorComponent::GovernorExecutionTrait::proposal_needs_queuing`.
        fn proposal_needs_queuing(
            self: @GovernorComponentState<TContractState>, proposal_id: felt252,
        ) -> bool {
            true
        }

        /// See `GovernorComponent::GovernorExecutionTrait::cancel_operations`.
        ///
        /// Cancels the timelocked proposal if it has already been queued.
        fn cancel_operations(
            ref self: GovernorComponentState<TContractState>,
            proposal_id: felt252,
            description_hash: felt252,
        ) {
            self._cancel(proposal_id, description_hash);

            let mut contract = self.get_contract_mut();
            let mut this_component = GovernorCoreExecution::get_component_mut(ref contract);

            let timelock_id = this_component.Governor_timelock_ids.read(proposal_id);
            if timelock_id.is_non_zero() {
                let timelock_dispatcher = this_component.get_timelock_dispatcher();

                timelock_dispatcher.cancel(timelock_id);
                this_component.Governor_timelock_ids.write(proposal_id, 0);
            }
        }
    }

    //
    // External
    //

    #[embeddable_as(TimelockedImpl)]
    impl Timelocked<
        TContractState,
        +HasComponent<TContractState>,
        +GovernorComponent::GovernorSettingsTrait<TContractState>,
        +GovernorComponent::GovernorCountingTrait<TContractState>,
        +GovernorComponent::GovernorVotesTrait<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +GovernorComponent::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of ITimelocked<ComponentState<TContractState>> {
        /// Returns the timelock controller address.
        fn timelock(self: @ComponentState<TContractState>) -> ContractAddress {
            self.Governor_timelock_controller.read()
        }

        /// Returns the timelock proposal id for a given proposal id.
        fn get_timelock_id(
            self: @ComponentState<TContractState>, proposal_id: felt252,
        ) -> TimelockProposalId {
            self.Governor_timelock_ids.read(proposal_id)
        }

        /// Updates the associated timelock.
        ///
        /// Requirements:
        ///
        /// - Caller must be the governance.
        ///
        /// Emits a `TimelockUpdated` event.
        fn update_timelock(
            ref self: ComponentState<TContractState>, new_timelock: ContractAddress,
        ) {
            self.assert_only_governance();
            self._update_timelock(new_timelock);
        }
    }

    //
    // Internal
    //

    #[generate_trait]
    pub impl InternalImpl<
        TContractState,
        +HasComponent<TContractState>,
        +GovernorComponent::GovernorSettingsTrait<TContractState>,
        +GovernorComponent::GovernorCountingTrait<TContractState>,
        +GovernorComponent::GovernorVotesTrait<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        impl Governor: GovernorComponent::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of InternalTrait<TContractState> {
        /// Initializes the component by setting the timelock contract address.
        ///
        /// Requirements:
        ///
        /// - `timelock_controller` must not be zero.
        fn initializer(
            ref self: ComponentState<TContractState>, timelock_controller: ContractAddress,
        ) {
            assert(timelock_controller.is_non_zero(), Errors::INVALID_TIMELOCK_CONTROLLER);
            self._update_timelock(timelock_controller);
        }

        /// Wrapper for `Governor::assert_only_governance`.
        fn assert_only_governance(self: @ComponentState<TContractState>) {
            let governor_component = get_dep_component!(self, Governor);
            governor_component.assert_only_governance();
        }

        /// Computes the `TimelockController` operation salt as the XOR of
        /// the governor address and `description_hash`. In the case of overflow, it is
        /// reduced modulo P.
        ///
        /// It is computed with the governor address itself to avoid collisions across
        /// governor instances using the same timelock.
        fn timelock_salt(
            self: @ComponentState<TContractState>, description_hash: felt252,
        ) -> felt252 {
            let description_hash: u256 = description_hash.into();
            let this: felt252 = starknet::get_contract_address().into();
            let max_felt: u256 = MAX_FELT.into();

            let mut value = this.into() ^ description_hash;
            if value > max_felt {
                // Get the value modulo P.
                // Invariant: 2 * max_felt > 2 ** 252 - 1 >= value
                value = value - max_felt - 1;
            }
            // Unwrap is safe since value is less or equal than MAX_FELT.
            value.try_into().unwrap()
        }

        /// Returns the timelock contract address wrapped in a ITimelockDispatcher.
        fn get_timelock_dispatcher(self: @ComponentState<TContractState>) -> ITimelockDispatcher {
            let timelock_controller = self.timelock();
            ITimelockDispatcher { contract_address: timelock_controller }
        }

        /// Updates the timelock contract address.
        ///
        /// Emits a `TimelockUpdated` event.
        fn _update_timelock(
            ref self: ComponentState<TContractState>, new_timelock: ContractAddress,
        ) {
            let old_timelock = self.Governor_timelock_controller.read();
            self.emit(TimelockUpdated { old_timelock, new_timelock });
            self.Governor_timelock_controller.write(new_timelock);
        }
    }
}
