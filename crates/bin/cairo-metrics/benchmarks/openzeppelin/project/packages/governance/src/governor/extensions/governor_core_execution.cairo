// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0
// (governance/src/governor/extensions/governor_core_execution.cairo)

/// # GovernorCoreExecution Component
///
/// Extension of GovernorComponent providing an execution mechanism directly through
/// the Governor itself. For a timelocked execution mechanism, see
/// GovernorTimelockExecutionComponent.
#[starknet::component]
pub mod GovernorCoreExecutionComponent {
    use openzeppelin_introspection::src5::SRC5Component;
    use starknet::account::Call;
    use starknet::{ContractAddress, SyscallResultTrait};
    use crate::governor::GovernorComponent;
    use crate::governor::GovernorComponent::{
        ComponentState as GovernorComponentState, InternalExtendedTrait,
    };
    use crate::governor::interface::ProposalState;

    #[storage]
    pub struct Storage {}

    //
    // Extensions
    //

    pub impl GovernorExecution<
        TContractState,
        +GovernorComponent::HasComponent<TContractState>,
        +GovernorComponent::GovernorCountingTrait<TContractState>,
        +GovernorComponent::GovernorSettingsTrait<TContractState>,
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
            self._state(proposal_id)
        }

        /// See `GovernorComponent::GovernorExecutionTrait::executor`.
        ///
        /// Returns the governor contract address since execution is performed directly through it.
        fn executor(self: @GovernorComponentState<TContractState>) -> ContractAddress {
            starknet::get_contract_address()
        }

        /// See `GovernorComponent::GovernorExecutionTrait::execute_operations`.
        ///
        /// Executes the proposal's operations directly through the governor contract.
        fn execute_operations(
            ref self: GovernorComponentState<TContractState>,
            proposal_id: felt252,
            calls: Span<Call>,
            description_hash: felt252,
        ) {
            for call in calls {
                let Call { to, selector, calldata } = *call;
                starknet::syscalls::call_contract_syscall(to, selector, calldata).unwrap_syscall();
            };
        }

        /// See `GovernorComponent::GovernorExecutionTrait::queue_operations`.
        ///
        /// In this implementation, queuing is not required so it returns 0.
        fn queue_operations(
            ref self: GovernorComponentState<TContractState>,
            proposal_id: felt252,
            calls: Span<Call>,
            description_hash: felt252,
        ) -> u64 {
            0
        }

        /// See `GovernorComponent::GovernorExecutionTrait::proposal_needs_queuing`.
        ///
        /// In this implementation, it always returns false.
        fn proposal_needs_queuing(
            self: @GovernorComponentState<TContractState>, proposal_id: felt252,
        ) -> bool {
            false
        }

        /// See `GovernorComponent::GovernorExecutionTrait::cancel_operations`.
        fn cancel_operations(
            ref self: GovernorComponentState<TContractState>,
            proposal_id: felt252,
            description_hash: felt252,
        ) {
            self._cancel(proposal_id, description_hash);
        }
    }
}
