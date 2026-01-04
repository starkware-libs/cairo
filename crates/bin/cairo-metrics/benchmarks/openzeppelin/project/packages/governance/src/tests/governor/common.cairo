use core::hash::{HashStateExTrait, HashStateTrait};
use core::pedersen::PedersenTrait;
use openzeppelin_test_common::mocks::governor::GovernorMock::SNIP12MetadataImpl;
use openzeppelin_test_common::mocks::governor::{GovernorMock, GovernorTimelockedMock};
use openzeppelin_testing::constants::{ADMIN, OTHER};
use openzeppelin_utils::bytearray::ByteArrayExtTrait;
use snforge_std::{start_cheat_block_timestamp_global, start_mock_call};
use starknet::ContractAddress;
use starknet::account::Call;
use starknet::storage::{StorageMapWriteAccess, StoragePathEntry, StoragePointerWriteAccess};
use crate::governor::GovernorComponent::{InternalExtendedImpl, InternalImpl};
use crate::governor::interface::{IGovernor, ProposalState};
use crate::governor::{DefaultConfig, GovernorComponent, ProposalCore};
use crate::utils::call_impls::{HashCallImpl, HashCallsImpl};

pub type ComponentState = GovernorComponent::ComponentState<GovernorMock::ContractState>;
pub type ComponentStateTimelocked =
    GovernorComponent::ComponentState<GovernorTimelockedMock::ContractState>;

pub fn CONTRACT_STATE() -> GovernorMock::ContractState {
    GovernorMock::contract_state_for_testing()
}

pub fn COMPONENT_STATE() -> ComponentState {
    GovernorComponent::component_state_for_testing()
}

pub fn CONTRACT_STATE_TIMELOCKED() -> GovernorTimelockedMock::ContractState {
    GovernorTimelockedMock::contract_state_for_testing()
}

pub fn COMPONENT_STATE_TIMELOCKED() -> ComponentStateTimelocked {
    GovernorComponent::component_state_for_testing()
}

//
// Helpers
//

pub fn hash_proposal(calls: Span<Call>, description_hash: felt252) -> felt252 {
    PedersenTrait::new(0).update_with(calls).update_with(description_hash).finalize()
}

pub fn get_proposal_info() -> (felt252, ProposalCore) {
    let calls = get_calls(OTHER, false);
    get_proposal_with_id(calls, @"proposal description")
}

pub fn get_proposal_with_id(calls: Span<Call>, description: @ByteArray) -> (felt252, ProposalCore) {
    let timestamp = starknet::get_block_timestamp();
    let vote_start = timestamp + GovernorMock::VOTING_DELAY;
    let vote_duration = GovernorMock::VOTING_PERIOD;

    let proposal_id = hash_proposal(calls, description.hash());
    let proposal = ProposalCore {
        proposer: ADMIN,
        vote_start,
        vote_duration,
        executed: false,
        canceled: false,
        eta_seconds: 0,
    };

    (proposal_id, proposal)
}

pub fn get_calls(to: ContractAddress, mock_syscalls: bool) -> Span<Call> {
    let call1 = Call { to, selector: selector!("test1"), calldata: array![].span() };
    let call2 = Call { to, selector: selector!("test2"), calldata: array![].span() };

    if mock_syscalls {
        start_mock_call(to, selector!("test1"), 'test1');
        start_mock_call(to, selector!("test2"), 'test2');
    }

    array![call1, call2].span()
}

pub fn get_state(
    state: @ComponentState, id: felt252, external_state_version: bool,
) -> ProposalState {
    if external_state_version {
        state.state(id)
    } else {
        state._state(id)
    }
}

pub fn get_mock_state(
    mock_state: @GovernorMock::ContractState, id: felt252, external_state_version: bool,
) -> ProposalState {
    if external_state_version {
        mock_state.governor.state(id)
    } else {
        mock_state.governor._state(id)
    }
}

pub fn set_executor(
    ref mock_state: GovernorTimelockedMock::ContractState, executor: ContractAddress,
) {
    mock_state.governor_timelock_execution.Governor_timelock_controller.write(executor);
}

//
// Setup proposals
//

pub fn setup_pending_proposal(
    ref state: ComponentState, external_state_version: bool,
) -> (felt252, ProposalCore) {
    let (id, proposal) = get_proposal_info();

    state.Governor_proposals.write(id, proposal);

    let current_state = get_state(@state, id, external_state_version);
    let expected = ProposalState::Pending;

    assert_eq!(current_state, expected);

    (id, proposal)
}

pub fn setup_active_proposal(
    ref state: ComponentState, external_state_version: bool,
) -> (felt252, ProposalCore) {
    let (id, proposal) = get_proposal_info();

    state.Governor_proposals.write(id, proposal);

    let deadline = proposal.vote_start + proposal.vote_duration;
    let expected = ProposalState::Active;

    // Is active before deadline
    start_cheat_block_timestamp_global(deadline - 1);
    let current_state = get_state(@state, id, external_state_version);
    assert_eq!(current_state, expected);

    (id, proposal)
}

pub fn setup_queued_proposal(
    ref mock_state: GovernorMock::ContractState, external_state_version: bool,
) -> (felt252, ProposalCore) {
    let (id, mut proposal) = get_proposal_info();

    proposal.eta_seconds = 1;
    mock_state.governor.Governor_proposals.write(id, proposal);

    let deadline = proposal.vote_start + proposal.vote_duration;

    // Quorum reached
    start_cheat_block_timestamp_global(deadline + 1);
    let quorum = mock_state.governor.quorum(0);
    let proposal_votes = mock_state.governor_counting_simple.Governor_proposals_votes.entry(id);
    proposal_votes.for_votes.write(quorum + 1);

    // Vote succeeded
    proposal_votes.against_votes.write(quorum);

    let expected = ProposalState::Queued;
    let current_state = get_mock_state(@mock_state, id, external_state_version);
    assert_eq!(current_state, expected);

    (id, proposal)
}

pub fn setup_canceled_proposal(
    ref state: ComponentState, external_state_version: bool,
) -> (felt252, ProposalCore) {
    let (id, proposal) = get_proposal_info();

    state.Governor_proposals.write(id, proposal);

    state._cancel(id, 0);

    let expected = ProposalState::Canceled;
    let current_state = get_state(@state, id, external_state_version);
    assert_eq!(current_state, expected);

    (id, proposal)
}

pub fn setup_defeated_proposal(
    ref mock_state: GovernorMock::ContractState, external_state_version: bool,
) -> (felt252, ProposalCore) {
    let (id, proposal) = get_proposal_info();

    mock_state.governor.Governor_proposals.write(id, proposal);

    let deadline = proposal.vote_start + proposal.vote_duration;

    // Quorum not reached
    start_cheat_block_timestamp_global(deadline + 1);
    let quorum = mock_state.governor.quorum(0);
    let proposal_votes = mock_state.governor_counting_simple.Governor_proposals_votes.entry(id);
    proposal_votes.for_votes.write(quorum - 1);

    let expected = ProposalState::Defeated;
    let current_state = get_mock_state(@mock_state, id, external_state_version);
    assert_eq!(current_state, expected);

    (id, proposal)
}

pub fn setup_succeeded_proposal(
    ref mock_state: GovernorMock::ContractState, external_state_version: bool,
) -> (felt252, ProposalCore) {
    let (id, proposal) = get_proposal_info();

    mock_state.governor.Governor_proposals.write(id, proposal);

    let deadline = proposal.vote_start + proposal.vote_duration;
    let expected = ProposalState::Succeeded;

    start_cheat_block_timestamp_global(deadline + 1);

    // Quorum reached
    let quorum = mock_state.governor.quorum(0);
    let proposal_votes = mock_state.governor_counting_simple.Governor_proposals_votes.entry(id);
    proposal_votes.for_votes.write(quorum + 1);

    // Vote succeeded
    proposal_votes.against_votes.write(quorum);

    let current_state = get_mock_state(@mock_state, id, external_state_version);
    assert_eq!(current_state, expected);

    (id, proposal)
}

pub fn setup_executed_proposal(
    ref state: ComponentState, external_state_version: bool,
) -> (felt252, ProposalCore) {
    let (id, mut proposal) = get_proposal_info();

    proposal.executed = true;
    state.Governor_proposals.write(id, proposal);

    let current_state = get_state(@state, id, external_state_version);
    let expected = ProposalState::Executed;

    assert_eq!(current_state, expected);

    (id, proposal)
}
