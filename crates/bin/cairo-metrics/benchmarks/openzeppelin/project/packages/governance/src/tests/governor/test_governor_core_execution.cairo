use openzeppelin_test_common::mocks::governor::GovernorMock::SNIP12MetadataImpl;
use openzeppelin_testing::constants::OTHER;
use snforge_std::start_cheat_block_timestamp_global;
use starknet::storage::{StorageMapWriteAccess, StoragePathEntry, StoragePointerWriteAccess};
use crate::governor::DefaultConfig;
use crate::governor::GovernorComponent::InternalImpl;
use crate::governor::extensions::GovernorCoreExecutionComponent::GovernorExecution;
use crate::governor::interface::{IGovernor, ProposalState};
use crate::tests::governor::common::{
    COMPONENT_STATE, CONTRACT_STATE, get_calls, get_proposal_info, setup_active_proposal,
    setup_canceled_proposal, setup_defeated_proposal, setup_executed_proposal,
    setup_pending_proposal, setup_queued_proposal, setup_succeeded_proposal,
};

//
// state
//

#[test]
fn test_state_executed() {
    let mut component_state = COMPONENT_STATE();
    let (id, _) = setup_executed_proposal(ref component_state, false);

    let state = GovernorExecution::state(@component_state, id);
    assert_eq!(state, ProposalState::Executed);
}

#[test]
fn test_state_canceled() {
    let mut component_state = COMPONENT_STATE();
    let (id, _) = setup_canceled_proposal(ref component_state, false);

    let state = GovernorExecution::state(@component_state, id);
    assert_eq!(state, ProposalState::Canceled);
}

#[test]
#[should_panic(expected: 'Nonexistent proposal')]
fn test_state_non_existent() {
    let component_state = COMPONENT_STATE();

    GovernorExecution::state(@component_state, 1);
}

#[test]
fn test_state_pending() {
    let mut component_state = COMPONENT_STATE();
    let (id, _) = setup_pending_proposal(ref component_state, false);

    let state = GovernorExecution::state(@component_state, id);
    assert_eq!(state, ProposalState::Pending);
}

#[test]
fn test_state_active() {
    let mut component_state = COMPONENT_STATE();
    let (id, proposal) = get_proposal_info();

    component_state.Governor_proposals.write(id, proposal);

    let deadline = proposal.vote_start + proposal.vote_duration;
    let expected = ProposalState::Active;

    // Is active before deadline
    start_cheat_block_timestamp_global(deadline - 1);
    let state = GovernorExecution::state(@component_state, id);
    assert_eq!(state, expected);

    // Is active at deadline
    start_cheat_block_timestamp_global(deadline);
    let state = GovernorExecution::state(@component_state, id);
    assert_eq!(state, expected);
}

#[test]
fn test_state_defeated_quorum_not_reached() {
    let mut mock_state = CONTRACT_STATE();
    let component_state = COMPONENT_STATE();
    let (id, proposal) = get_proposal_info();

    mock_state.governor.Governor_proposals.write(id, proposal);

    let deadline = proposal.vote_start + proposal.vote_duration;
    let expected = ProposalState::Defeated;

    start_cheat_block_timestamp_global(deadline + 1);

    // Quorum not reached
    let quorum = mock_state.governor.quorum(0);
    let proposal_votes = mock_state.governor_counting_simple.Governor_proposals_votes.entry(id);
    proposal_votes.for_votes.write(quorum - 1);

    let state = GovernorExecution::state(@component_state, id);
    assert_eq!(state, expected);
}

#[test]
fn test_state_defeated_vote_not_succeeded() {
    let mut mock_state = CONTRACT_STATE();
    let component_state = COMPONENT_STATE();
    let (id, proposal) = get_proposal_info();

    mock_state.governor.Governor_proposals.write(id, proposal);

    let deadline = proposal.vote_start + proposal.vote_duration;
    let expected = ProposalState::Defeated;

    start_cheat_block_timestamp_global(deadline + 1);

    // Quorum reached
    let quorum = mock_state.governor.quorum(0);
    let proposal_votes = mock_state.governor_counting_simple.Governor_proposals_votes.entry(id);
    proposal_votes.for_votes.write(quorum + 1);

    // Vote not succeeded
    proposal_votes.against_votes.write(quorum + 1);

    let state = GovernorExecution::state(@component_state, id);
    assert_eq!(state, expected);
}

#[test]
fn test_state_queued() {
    let mut mock_state = CONTRACT_STATE();
    let component_state = COMPONENT_STATE();
    let (id, _) = setup_queued_proposal(ref mock_state, false);

    let state = GovernorExecution::state(@component_state, id);
    assert_eq!(state, ProposalState::Queued);
}

#[test]
fn test_state_succeeded() {
    let mut mock_state = CONTRACT_STATE();
    let component_state = COMPONENT_STATE();
    let (id, _) = setup_succeeded_proposal(ref mock_state, false);

    let state = GovernorExecution::state(@component_state, id);
    assert_eq!(state, ProposalState::Succeeded);
}

//
// executor
//

#[test]
fn test_executor() {
    let component_state = COMPONENT_STATE();
    let expected = starknet::get_contract_address();

    assert_eq!(GovernorExecution::executor(@component_state), expected);
}

//
// execute_operations
//

#[test]
#[fuzzer]
fn test_execute_operations(id: felt252) {
    let mut component_state = COMPONENT_STATE();
    let calls = get_calls(OTHER, true);
    let description_hash = 'hash';

    GovernorExecution::execute_operations(ref component_state, id, calls, description_hash);
}

#[test]
#[should_panic(expected: "Contract not deployed at address: 0x4f54484552")]
fn test_execute_operations_panics() {
    let mut component_state = COMPONENT_STATE();
    let id = 0;
    let calls = get_calls(OTHER, false);
    let description_hash = 'hash';

    GovernorExecution::execute_operations(ref component_state, id, calls, description_hash);
}

//
// queue_operations
//

#[test]
#[fuzzer]
fn test_queue_operations(id: felt252) {
    let mut component_state = COMPONENT_STATE();
    let calls = array![].span();
    let description_hash = 'hash';

    let eta = GovernorExecution::queue_operations(ref component_state, id, calls, description_hash);

    assert_eq!(eta, 0);
}

//
// proposal_needs_queuing
//

#[test]
#[fuzzer]
fn test_proposal_needs_queuing(id: felt252) {
    let component_state = COMPONENT_STATE();

    assert_eq!(GovernorExecution::proposal_needs_queuing(@component_state, id), false);
}

//
// cancel_operations
//

#[test]
fn test_cancel_operations_pending() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_pending_proposal(ref state, false);

    GovernorExecution::cancel_operations(ref state, id, 0);

    let canceled_proposal = state.get_proposal(id);
    assert_eq!(canceled_proposal.canceled, true);
}

#[test]
fn test_cancel_operations_active() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_active_proposal(ref state, false);

    GovernorExecution::cancel_operations(ref state, id, 0);

    let canceled_proposal = state.get_proposal(id);
    assert_eq!(canceled_proposal.canceled, true);
}

#[test]
fn test_cancel_operations_defeated() {
    let mut mock_state = CONTRACT_STATE();
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_defeated_proposal(ref mock_state, false);

    GovernorExecution::cancel_operations(ref state, id, 0);

    let canceled_proposal = mock_state.governor.get_proposal(id);
    assert_eq!(canceled_proposal.canceled, true);
}

#[test]
fn test_cancel_operations_succeeded() {
    let mut mock_state = CONTRACT_STATE();
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_succeeded_proposal(ref mock_state, false);

    GovernorExecution::cancel_operations(ref state, id, 0);

    let canceled_proposal = mock_state.governor.get_proposal(id);
    assert_eq!(canceled_proposal.canceled, true);
}

#[test]
fn test_cancel_operations_queued() {
    let mut mock_state = CONTRACT_STATE();
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_queued_proposal(ref mock_state, false);

    GovernorExecution::cancel_operations(ref state, id, 0);

    let canceled_proposal = mock_state.governor.get_proposal(id);
    assert_eq!(canceled_proposal.canceled, true);
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cancel_operations_canceled() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_canceled_proposal(ref state, false);

    // Cancel again
    GovernorExecution::cancel_operations(ref state, id, 0);
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cancel_operations_executed() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_executed_proposal(ref state, false);

    GovernorExecution::cancel_operations(ref state, id, 0);
}
