use core::num::traits::{Bounded, Zero};
use openzeppelin_introspection::src5::SRC5Component::SRC5Impl;
use openzeppelin_test_common::mocks::governor::GovernorMock;
use openzeppelin_test_common::mocks::governor::GovernorMock::SNIP12MetadataImpl;
use openzeppelin_test_common::mocks::timelock::{
    IMockContractDispatcher, IMockContractDispatcherTrait,
};
use openzeppelin_testing as utils;
use openzeppelin_testing::constants::{ADMIN, OTHER, VOTES_TOKEN, ZERO};
use openzeppelin_testing::{AsAddressTrait, EventSpyExt, EventSpyQueue as EventSpy, spy_events};
use openzeppelin_utils::bytearray::ByteArrayExtTrait;
use openzeppelin_utils::cryptography::snip12::OffchainMessageHash;
use snforge_std::signature::stark_curve::{StarkCurveKeyPairImpl, StarkCurveSignerImpl};
use snforge_std::{
    start_cheat_block_timestamp_global, start_cheat_caller_address, start_cheat_chain_id_global,
    start_mock_call, test_address,
};
use starknet::ContractAddress;
use starknet::account::Call;
use starknet::storage::{StorageMapWriteAccess, StoragePathEntry, StoragePointerWriteAccess};
use crate::governor::GovernorComponent::{InternalExtendedImpl, InternalImpl};
use crate::governor::interface::{
    IGOVERNOR_ID, IGovernor, IGovernorDispatcher, IGovernorDispatcherTrait, ProposalState,
};
use crate::governor::vote::{Vote, VoteWithReasonAndParams};
use crate::governor::{DefaultConfig, GovernorComponent, ProposalCore};
use crate::tests::governor::common::{
    COMPONENT_STATE, CONTRACT_STATE, get_calls, get_mock_state, get_proposal_info, get_state,
    hash_proposal, setup_active_proposal, setup_canceled_proposal, setup_defeated_proposal,
    setup_executed_proposal, setup_pending_proposal, setup_queued_proposal,
    setup_succeeded_proposal,
};

//
// Dispatchers
//

fn deploy_governor() -> IGovernorDispatcher {
    let mut calldata = array![VOTES_TOKEN.into()];

    let address = utils::declare_and_deploy("GovernorMock", calldata);
    IGovernorDispatcher { contract_address: address }
}

fn deploy_mock_target() -> IMockContractDispatcher {
    let mut calldata = array![];

    let address = utils::declare_and_deploy("MockContract", calldata);
    IMockContractDispatcher { contract_address: address }
}

fn setup_dispatchers() -> (IGovernorDispatcher, IMockContractDispatcher) {
    let governor = deploy_governor();
    let target = deploy_mock_target();

    (governor, target)
}

fn setup_account(public_key: felt252) -> ContractAddress {
    let mut calldata = array![public_key];
    utils::declare_and_deploy("SnakeAccountMock", calldata)
}

//
// External
//

#[test]
fn test_name() {
    let state = @COMPONENT_STATE();
    let name = state.name();
    assert_eq!(name, 'DAPP_NAME');
}

#[test]
fn test_version() {
    let state = COMPONENT_STATE();
    let version = state.version();
    assert_eq!(version, 'DAPP_VERSION');
}

#[test]
fn test_counting_mode() {
    let state = COMPONENT_STATE();
    let counting_mode = state.COUNTING_MODE();
    assert_eq!(counting_mode, "support=bravo&quorum=for,abstain");
}

#[test]
fn test_hash_proposal() {
    let state = COMPONENT_STATE();
    let calls = get_calls(ZERO, false);
    let description = @"proposal description";
    let description_hash = description.hash();

    let expected_hash = hash_proposal(calls, description_hash);
    let hash = state.hash_proposal(calls, description_hash);

    assert_eq!(hash, expected_hash);
}

//
// state
//

#[test]
fn test_state_executed() {
    let mut state = COMPONENT_STATE();

    // The function already asserts the state
    setup_executed_proposal(ref state, true);
}

#[test]
fn test_state_canceled() {
    let mut state = COMPONENT_STATE();

    // The function already asserts the state
    setup_canceled_proposal(ref state, true);
}

#[test]
#[should_panic(expected: 'Nonexistent proposal')]
fn test_state_non_existent() {
    let state = COMPONENT_STATE();

    state._state(1);
}

#[test]
fn test_state_pending() {
    let mut state = COMPONENT_STATE();

    // The function already asserts the state
    setup_pending_proposal(ref state, true);
}

fn test_state_active_external_version(external_state_version: bool) {
    let mut state = COMPONENT_STATE();
    let (id, proposal) = get_proposal_info();

    state.Governor_proposals.write(id, proposal);

    let deadline = proposal.vote_start + proposal.vote_duration;
    let expected = ProposalState::Active;

    // Is active before deadline
    start_cheat_block_timestamp_global(deadline - 1);
    let current_state = get_state(@state, id, external_state_version);
    assert_eq!(current_state, expected);

    // Is active in deadline
    start_cheat_block_timestamp_global(deadline);
    let current_state = get_state(@state, id, external_state_version);
    assert_eq!(current_state, expected);
}

#[test]
fn test_state_active() {
    test_state_active_external_version(true);
}

fn test_state_defeated_quorum_not_reached_external_version(external_state_version: bool) {
    let mut mock_state = CONTRACT_STATE();
    let (id, proposal) = get_proposal_info();

    mock_state.governor.Governor_proposals.write(id, proposal);

    let deadline = proposal.vote_start + proposal.vote_duration;
    let expected = ProposalState::Defeated;

    start_cheat_block_timestamp_global(deadline + 1);

    // Quorum not reached
    let quorum = mock_state.governor.quorum(0);
    let proposal_votes = mock_state.governor_counting_simple.Governor_proposals_votes.entry(id);
    proposal_votes.for_votes.write(quorum - 1);

    let current_state = get_mock_state(@mock_state, id, external_state_version);
    assert_eq!(current_state, expected);
}

#[test]
fn test_state_defeated_quorum_not_reached() {
    test_state_defeated_quorum_not_reached_external_version(true);
}

fn test_state_defeated_vote_not_succeeded_external_version(external_state_version: bool) {
    let mut mock_state = CONTRACT_STATE();
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

    let current_state = get_mock_state(@mock_state, id, external_state_version);
    assert_eq!(current_state, expected);
}

#[test]
fn test_state_defeated_vote_not_succeeded() {
    test_state_defeated_vote_not_succeeded_external_version(true);
}

#[test]
fn test_state_queued() {
    let mut mock_state = CONTRACT_STATE();

    // The function already asserts the state
    setup_queued_proposal(ref mock_state, true);
}

#[test]
fn test_state_succeeded() {
    let mut mock_state = CONTRACT_STATE();

    // The function already asserts the state
    setup_succeeded_proposal(ref mock_state, true);
}

//
// Proposal info
//

#[test]
fn test_proposal_threshold() {
    let state = COMPONENT_STATE();

    let threshold = state.proposal_threshold();
    let expected = GovernorMock::PROPOSAL_THRESHOLD;
    assert_eq!(threshold, expected);
}

#[test]
fn test_proposal_snapshot() {
    let mut state = COMPONENT_STATE();
    let (id, proposal) = get_proposal_info();

    state.Governor_proposals.write(id, proposal);

    let snapshot = state.proposal_snapshot(id);
    let expected = proposal.vote_start;
    assert_eq!(snapshot, expected);
}

#[test]
fn test_proposal_deadline() {
    let mut state = COMPONENT_STATE();
    let (id, proposal) = get_proposal_info();

    state.Governor_proposals.write(id, proposal);

    let deadline = state.proposal_deadline(id);
    let expected = proposal.vote_start + proposal.vote_duration;
    assert_eq!(deadline, expected);
}

#[test]
fn test_proposal_proposer() {
    let mut state = COMPONENT_STATE();
    let (id, proposal) = get_proposal_info();

    state.Governor_proposals.write(id, proposal);

    let proposer = state.proposal_proposer(id);
    let expected = proposal.proposer;
    assert_eq!(proposer, expected);
}

#[test]
fn test_proposal_eta() {
    let mut state = COMPONENT_STATE();
    let (id, proposal) = get_proposal_info();

    state.Governor_proposals.write(id, proposal);

    let eta = state.proposal_eta(id);
    let expected = proposal.eta_seconds;
    assert_eq!(eta, expected);
}

#[test]
fn test_proposal_needs_queuing() {
    let mut state = COMPONENT_STATE();
    let (id, proposal) = get_proposal_info();

    state.Governor_proposals.write(id, proposal);

    let needs_queuing = state.proposal_needs_queuing(id);
    assert_eq!(needs_queuing, false);
}

#[test]
fn test_voting_delay() {
    let state = COMPONENT_STATE();

    let threshold = state.voting_delay();
    let expected = GovernorMock::VOTING_DELAY;
    assert_eq!(threshold, expected);
}

#[test]
fn test_voting_period() {
    let state = COMPONENT_STATE();

    let threshold = state.voting_period();
    let expected = GovernorMock::VOTING_PERIOD;
    assert_eq!(threshold, expected);
}

#[test]
#[fuzzer]
fn test_quorum(timepoint: u64) {
    let state = COMPONENT_STATE();

    let threshold = state.quorum(timepoint);
    let expected = if timepoint == Bounded::MAX {
        Bounded::MAX
    } else {
        GovernorMock::QUORUM
    };
    assert_eq!(threshold, expected);
}

//
// get_votes
//

#[test]
fn test_get_votes() {
    let state = COMPONENT_STATE();
    let timepoint = 0;
    let expected_weight = 100;

    // Mock the get_past_votes call
    start_mock_call(Zero::zero(), selector!("get_past_votes"), expected_weight);

    let votes = state.get_votes(OTHER, timepoint);
    assert_eq!(votes, expected_weight);
}

#[test]
fn test_get_votes_with_params() {
    let state = COMPONENT_STATE();
    let timepoint = 0;
    let expected_weight = 100;
    let params = array!['param'].span();

    // Mock the get_past_votes call
    start_mock_call(Zero::zero(), selector!("get_past_votes"), expected_weight);

    let votes = state.get_votes_with_params(OTHER, timepoint, params);
    assert_eq!(votes, expected_weight);
}

//
// has_voted
//

#[test]
fn test_has_voted() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_active_proposal(ref state, false);

    let reason = "reason";
    let params = array![].span();

    // 1. Assert has not voted
    let has_not_voted = !state.has_voted(id, OTHER);
    assert!(has_not_voted);

    // 2. Cast vote
    start_mock_call(Zero::zero(), selector!("get_past_votes"), 100_u256);
    state._cast_vote(id, OTHER, 0, reason, params);

    // 3. Assert has voted
    let has_voted = state.has_voted(id, OTHER);
    assert!(has_voted);
}

//
// propose
//

fn test_propose_external_version(external_state_version: bool) {
    let mut state = COMPONENT_STATE();
    let mut spy = spy_events();
    let contract_address = test_address();

    let calls = get_calls(OTHER, false);
    let proposer = ADMIN;
    let address = ADMIN.to_byte_array(16, 64);
    let mut description: ByteArray = "proposal description#proposer=0x";
    description.append(@address);
    let description_snap = @description;
    let vote_start = starknet::get_block_timestamp() + GovernorMock::VOTING_DELAY;
    let vote_end = vote_start + GovernorMock::VOTING_PERIOD;

    // 1. Check id
    let id = if external_state_version {
        state.propose(calls, description)
    } else {
        state._propose(calls, description_snap, proposer)
    };
    let expected_id = hash_proposal(calls, description_snap.hash());
    assert_eq!(id, expected_id);

    // 2. Check event
    spy
        .assert_only_event_proposal_created(
            contract_address,
            expected_id,
            proposer,
            calls,
            array![].span(),
            vote_start,
            vote_end,
            description_snap,
        );

    // 3. Check proposal
    let proposal = state.get_proposal(id);
    let expected = ProposalCore {
        proposer: ADMIN,
        vote_start: starknet::get_block_timestamp() + GovernorMock::VOTING_DELAY,
        vote_duration: GovernorMock::VOTING_PERIOD,
        executed: false,
        canceled: false,
        eta_seconds: 0,
    };

    assert_eq!(proposal, expected);
}

#[test]
fn test_propose() {
    let votes = GovernorMock::PROPOSAL_THRESHOLD + 1;

    start_cheat_block_timestamp_global(10);
    start_mock_call(Zero::zero(), selector!("get_past_votes"), votes);
    start_cheat_caller_address(test_address(), ADMIN);

    test_propose_external_version(true);
}

#[test]
#[should_panic(expected: 'Existent proposal')]
fn test_propose_existent_proposal() {
    let mut state = COMPONENT_STATE();
    let calls = get_calls(OTHER, false);
    let description = "proposal description";
    let votes = GovernorMock::PROPOSAL_THRESHOLD + 1;

    start_cheat_block_timestamp_global(10);
    start_mock_call(Zero::zero(), selector!("get_past_votes"), votes);
    start_cheat_caller_address(test_address(), ADMIN);

    state.propose(calls, description.clone());

    // Propose again
    state.propose(calls, description);
}

#[test]
#[should_panic(expected: 'Insufficient votes')]
fn test_propose_insufficient_proposer_votes() {
    let mut state = COMPONENT_STATE();
    let votes = GovernorMock::PROPOSAL_THRESHOLD - 1;

    start_cheat_block_timestamp_global(10);
    start_mock_call(Zero::zero(), selector!("get_past_votes"), votes);

    let calls = get_calls(ZERO, false);
    let description = "proposal description";

    state.propose(calls, description);
}

#[test]
#[should_panic(expected: 'Restricted proposer')]
fn test_propose_restricted_proposer() {
    let mut state = COMPONENT_STATE();

    let calls = get_calls(ZERO, false);
    let description =
        "#proposer=0x04718f5a0fc34cc1af16a1cdee98ffb20c31f5cd61d6ab07201858f4287c938d";

    state.propose(calls, description);
}

//
// execute
//

#[test]
fn test_execute() {
    let (mut governor, target) = setup_dispatchers();
    let new_number = 125;

    let call = Call {
        to: target.contract_address,
        selector: selector!("set_number"),
        calldata: array![new_number].span(),
    };
    let calls = array![call].span();
    let description = "proposal description";

    let number = target.get_number();
    assert_eq!(number, 0);

    // Mock the get_past_votes call
    let quorum = GovernorMock::QUORUM;
    start_mock_call(VOTES_TOKEN, selector!("get_past_votes"), quorum);

    // 1. Propose
    let mut current_time = 10;
    start_cheat_block_timestamp_global(current_time);
    let id = governor.propose(calls, description.clone());

    // 2. Cast vote

    // Fast forward the vote delay
    current_time += GovernorMock::VOTING_DELAY;
    start_cheat_block_timestamp_global(current_time);

    // Cast vote
    governor.cast_vote(id, 1);

    // 3. Execute

    // Fast forward the vote duration
    current_time += (GovernorMock::VOTING_PERIOD + 1);
    start_cheat_block_timestamp_global(current_time);

    let state = governor.state(id);
    assert_eq!(state, ProposalState::Succeeded);

    let mut spy = spy_events();
    governor.execute(calls, (@description).hash());

    // 4. Assertions
    let number = target.get_number();
    assert_eq!(number, new_number);

    let state = governor.state(id);
    assert_eq!(state, ProposalState::Executed);

    spy.assert_only_event_proposal_executed(governor.contract_address, id);
}

#[test]
#[should_panic(expected: 'Expected failure')]
fn test_execute_panics() {
    let (mut governor, target) = setup_dispatchers();

    let call = Call {
        to: target.contract_address,
        selector: selector!("failing_function"),
        calldata: array![].span(),
    };
    let calls = array![call].span();
    let description = "proposal description";

    // Mock the get_past_votes call
    let quorum = GovernorMock::QUORUM;
    start_mock_call(VOTES_TOKEN, selector!("get_past_votes"), quorum);

    // 1. Propose
    let mut current_time = 10;
    start_cheat_block_timestamp_global(current_time);
    let id = governor.propose(calls, description.clone());

    // 2. Cast vote

    // Fast forward the vote delay
    current_time += GovernorMock::VOTING_DELAY;
    start_cheat_block_timestamp_global(current_time);

    // Cast vote
    governor.cast_vote(id, 1);

    // 3. Execute

    // Fast forward the vote duration
    current_time += (GovernorMock::VOTING_PERIOD + 1);
    start_cheat_block_timestamp_global(current_time);

    let state = governor.state(id);
    assert_eq!(state, ProposalState::Succeeded);

    governor.execute(calls, (@description).hash());
}

#[test]
fn test_execute_correct_id() {
    let mut mock_state = CONTRACT_STATE();
    setup_succeeded_proposal(ref mock_state, false);

    let calls = get_calls(OTHER, true);
    let description = @"proposal description";

    let id = mock_state.governor.execute(calls, description.hash());
    let expected_id = hash_proposal(calls, description.hash());
    assert_eq!(id, expected_id);
}

#[test]
fn test_execute_succeeded_passes() {
    let mut mock_state = CONTRACT_STATE();
    setup_succeeded_proposal(ref mock_state, false);

    let calls = get_calls(OTHER, true);
    let description = @"proposal description";

    mock_state.governor.execute(calls, description.hash());
}

#[test]
fn test_execute_queued_passes() {
    let mut mock_state = CONTRACT_STATE();
    setup_queued_proposal(ref mock_state, false);

    let calls = get_calls(OTHER, true);
    let description = @"proposal description";

    mock_state.governor.execute(calls, description.hash());
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_execute_pending() {
    let mut state = COMPONENT_STATE();
    setup_pending_proposal(ref state, false);

    let calls = get_calls(OTHER, false);
    let description = @"proposal description";

    state.execute(calls, description.hash());
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_execute_active() {
    let mut state = COMPONENT_STATE();
    setup_active_proposal(ref state, false);

    let calls = get_calls(OTHER, false);
    let description = @"proposal description";

    state.execute(calls, description.hash());
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_execute_defeated() {
    let mut mock_state = CONTRACT_STATE();
    setup_defeated_proposal(ref mock_state, false);

    let calls = get_calls(OTHER, false);
    let description = @"proposal description";

    mock_state.governor.execute(calls, description.hash());
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_execute_canceled() {
    let mut state = COMPONENT_STATE();
    setup_canceled_proposal(ref state, false);

    let calls = get_calls(OTHER, false);
    let description = @"proposal description";

    state.execute(calls, description.hash());
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_execute_executed() {
    let mut state = COMPONENT_STATE();
    setup_executed_proposal(ref state, false);

    let calls = get_calls(OTHER, false);
    let description = @"proposal description";

    state.execute(calls, description.hash());
}

//
// cancel
//

#[test]
fn test_cancel() {
    let mut state = COMPONENT_STATE();

    let calls = get_calls(OTHER, false);
    let description = @"proposal description";
    let proposer = ADMIN;

    // 1. Propose
    let id = state._propose(calls, description, proposer);

    let proposal_state = state.state(id);
    assert_eq!(proposal_state, ProposalState::Pending);

    // 2. Cancel
    let mut spy = spy_events();

    // Proposer must be the caller
    start_cheat_caller_address(test_address(), ADMIN);

    state.cancel(calls, description.hash());

    // 3. Assertions
    let proposal_state = state.state(id);
    assert_eq!(proposal_state, ProposalState::Canceled);

    spy.assert_only_event_proposal_canceled(test_address(), id);
}

#[test]
fn test_cancel_correct_id() {
    let mut state = COMPONENT_STATE();
    setup_pending_proposal(ref state, false);

    let calls = get_calls(OTHER, false);
    let description = @"proposal description";

    // Proposer must be the caller
    start_cheat_caller_address(test_address(), ADMIN);

    let id = state.cancel(calls, description.hash());
    let expected_id = hash_proposal(calls, description.hash());
    assert_eq!(id, expected_id);
}

#[test]
#[should_panic(expected: 'Proposer only')]
fn test_cancel_invalid_caller() {
    let mut state = COMPONENT_STATE();
    setup_pending_proposal(ref state, false);

    let calls = get_calls(OTHER, false);
    let description = @"proposal description";

    // Proposer must be the caller
    start_cheat_caller_address(test_address(), OTHER);

    state.cancel(calls, description.hash());
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cancel_succeeded() {
    let mut mock_state = CONTRACT_STATE();
    setup_succeeded_proposal(ref mock_state, false);

    let calls = get_calls(OTHER, true);
    let description = @"proposal description";

    mock_state.governor.cancel(calls, description.hash());
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cancel_queued() {
    let mut mock_state = CONTRACT_STATE();
    setup_queued_proposal(ref mock_state, false);

    let calls = get_calls(OTHER, true);
    let description = @"proposal description";

    mock_state.governor.cancel(calls, description.hash());
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cancel_active() {
    let mut state = COMPONENT_STATE();
    setup_active_proposal(ref state, false);

    let calls = get_calls(OTHER, false);
    let description = @"proposal description";

    state.cancel(calls, description.hash());
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cancel_defeated() {
    let mut mock_state = CONTRACT_STATE();
    setup_defeated_proposal(ref mock_state, false);

    let calls = get_calls(OTHER, false);
    let description = @"proposal description";

    mock_state.governor.cancel(calls, description.hash());
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cancel_canceled() {
    let mut state = COMPONENT_STATE();
    setup_canceled_proposal(ref state, false);

    let calls = get_calls(OTHER, false);
    let description = @"proposal description";

    state.cancel(calls, description.hash());
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cancel_executed() {
    let mut state = COMPONENT_STATE();
    setup_executed_proposal(ref state, false);

    let calls = get_calls(OTHER, false);
    let description = @"proposal description";

    state.cancel(calls, description.hash());
}

//
// cast_vote
//

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cast_vote_pending() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_pending_proposal(ref state, false);

    state.cast_vote(id, 0);
}

#[test]
fn test_cast_vote_active() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_active_proposal(ref state, false);
    let mut spy = spy_events();
    let contract_address = test_address();

    let expected_weight = 100;

    // Mock the get_past_votes call
    start_mock_call(Zero::zero(), selector!("get_past_votes"), expected_weight);

    start_cheat_caller_address(contract_address, OTHER);
    let weight = state.cast_vote(id, 0);
    assert_eq!(weight, expected_weight);

    spy.assert_only_event_vote_cast(contract_address, OTHER, id, 0, expected_weight, @"");
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cast_vote_defeated() {
    let mut mock_state = CONTRACT_STATE();
    let (id, _) = setup_defeated_proposal(ref mock_state, false);

    mock_state.governor.cast_vote(id, 0);
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cast_vote_succeeded() {
    let mut mock_state = CONTRACT_STATE();
    let (id, _) = setup_succeeded_proposal(ref mock_state, false);

    mock_state.governor.cast_vote(id, 0);
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cast_vote_queued() {
    let mut mock_state = CONTRACT_STATE();
    let (id, _) = setup_queued_proposal(ref mock_state, false);

    mock_state.governor.cast_vote(id, 0);
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cast_vote_canceled() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_canceled_proposal(ref state, false);

    state.cast_vote(id, 0);
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cast_vote_executed() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_executed_proposal(ref state, false);

    state.cast_vote(id, 0);
}

//
// cast_vote_with_reason
//

#[test]
fn test_cast_vote_with_reason_active() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_active_proposal(ref state, false);
    let mut spy = spy_events();
    let contract_address = test_address();

    let reason = "proposal reason";
    let expected_weight = 100;

    // Mock the get_past_votes call
    start_mock_call(Zero::zero(), selector!("get_past_votes"), expected_weight);

    start_cheat_caller_address(contract_address, OTHER);
    let weight = state.cast_vote_with_reason(id, 0, reason.clone());
    assert_eq!(weight, expected_weight);

    spy.assert_only_event_vote_cast(contract_address, OTHER, id, 0, expected_weight, @reason);
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cast_vote_with_reason_pending() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_pending_proposal(ref state, false);

    state.cast_vote_with_reason(id, 0, "");
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cast_vote_with_reason_defeated() {
    let mut mock_state = CONTRACT_STATE();
    let (id, _) = setup_defeated_proposal(ref mock_state, false);

    mock_state.governor.cast_vote_with_reason(id, 0, "");
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cast_vote_with_reason_succeeded() {
    let mut mock_state = CONTRACT_STATE();
    let (id, _) = setup_succeeded_proposal(ref mock_state, false);

    mock_state.governor.cast_vote_with_reason(id, 0, "");
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cast_vote_with_reason_queued() {
    let mut mock_state = CONTRACT_STATE();
    let (id, _) = setup_queued_proposal(ref mock_state, false);

    mock_state.governor.cast_vote_with_reason(id, 0, "");
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cast_vote_with_reason_canceled() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_canceled_proposal(ref state, false);

    state.cast_vote_with_reason(id, 0, "");
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cast_vote_with_reason_executed() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_executed_proposal(ref state, false);

    state.cast_vote_with_reason(id, 0, "");
}

//
// cast_vote_with_reason_and_params
//

#[test]
fn test_cast_vote_with_reason_and_params_active() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_active_proposal(ref state, false);
    let mut spy = spy_events();
    let contract_address = test_address();

    let params = array!['param1', 'param2'].span();
    let reason = "proposal reason";
    let expected_weight = 100;

    // Mock the get_past_votes call
    start_mock_call(Zero::zero(), selector!("get_past_votes"), expected_weight);

    start_cheat_caller_address(contract_address, OTHER);
    let weight = state.cast_vote_with_reason_and_params(id, 0, reason.clone(), params);
    assert_eq!(weight, expected_weight);

    spy
        .assert_only_event_vote_cast_with_params(
            contract_address, OTHER, id, 0, expected_weight, @reason, params,
        );
}

#[test]
fn test_cast_vote_with_reason_and_params_active_no_params() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_active_proposal(ref state, false);
    let mut spy = spy_events();
    let contract_address = test_address();

    let params = array![].span();
    let reason = "proposal reason";
    let expected_weight = 100;

    // Mock the get_past_votes call
    start_mock_call(Zero::zero(), selector!("get_past_votes"), expected_weight);

    start_cheat_caller_address(contract_address, OTHER);
    let weight = state.cast_vote_with_reason_and_params(id, 0, reason.clone(), params);
    assert_eq!(weight, expected_weight);

    spy.assert_only_event_vote_cast(contract_address, OTHER, id, 0, expected_weight, @reason);
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cast_vote_with_reason_and_params_defeated() {
    let mut mock_state = CONTRACT_STATE();
    let (id, _) = setup_defeated_proposal(ref mock_state, false);

    mock_state.governor.cast_vote_with_reason_and_params(id, 0, "", array![].span());
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cast_vote_with_reason_and_params_succeeded() {
    let mut mock_state = CONTRACT_STATE();
    let (id, _) = setup_succeeded_proposal(ref mock_state, false);

    mock_state.governor.cast_vote_with_reason_and_params(id, 0, "", array![].span());
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cast_vote_with_reason_and_params_queued() {
    let mut mock_state = CONTRACT_STATE();
    let (id, _) = setup_queued_proposal(ref mock_state, false);

    mock_state.governor.cast_vote_with_reason_and_params(id, 0, "", array![].span());
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cast_vote_with_reason_and_params_canceled() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_canceled_proposal(ref state, false);

    state.cast_vote_with_reason_and_params(id, 0, "", array![].span());
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_cast_vote_with_reason_and_params_executed() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_executed_proposal(ref state, false);

    state.cast_vote_with_reason_and_params(id, 0, "", array![].span());
}

//
// cast_vote_by_sig
//

fn prepare_governor_and_signature(
    nonce: felt252,
) -> (IGovernorDispatcher, felt252, felt252, felt252, u8, ContractAddress, u256) {
    let mut governor = deploy_governor();
    let calls = get_calls(OTHER, false);
    let description = "proposal description";

    // Mock the get_past_votes call
    let quorum = GovernorMock::QUORUM;
    start_mock_call(VOTES_TOKEN, selector!("get_past_votes"), quorum);

    // 1. Propose
    let mut current_time = 10;
    start_cheat_block_timestamp_global(current_time);
    let proposal_id = governor.propose(calls, description.clone());

    // 2. Fast forward the vote delay
    current_time += GovernorMock::VOTING_DELAY;
    start_cheat_block_timestamp_global(current_time);

    // 3. Generate a key pair and set up an account
    let key_pair = StarkCurveKeyPairImpl::generate();
    let voter = setup_account(key_pair.public_key);

    // 4. Set up signature parameters
    let support = 1;
    let verifying_contract = governor.contract_address;

    // 5. Create and sign the vote message
    let vote = Vote { verifying_contract, nonce, proposal_id, support, voter };
    let msg_hash = vote.get_message_hash(voter);
    let (r, s) = key_pair.sign(msg_hash).unwrap();

    (governor, r, s, proposal_id, support, voter, quorum)
}

#[test]
fn test_cast_vote_by_sig() {
    let (governor, r, s, proposal_id, support, voter, quorum) = prepare_governor_and_signature(0);

    // Set up event spy and cast vote
    let mut spy = spy_events();
    governor.cast_vote_by_sig(proposal_id, support, voter, array![r, s].span());

    spy
        .assert_only_event_vote_cast(
            governor.contract_address, voter, proposal_id, support, quorum, @"",
        );
}

#[test]
#[should_panic(expected: 'Invalid signature')]
fn test_cast_vote_by_sig_invalid_signature() {
    let (governor, r, s, proposal_id, support, voter, _) = prepare_governor_and_signature(0);

    // Cast vote with invalid signature
    governor.cast_vote_by_sig(proposal_id, support, voter, array![r + 1, s].span());
}

#[test]
#[should_panic(expected: 'Invalid signature')]
fn test_cast_vote_by_sig_invalid_msg_hash() {
    // Use invalid nonce (not the account's current nonce)
    let (governor, r, s, proposal_id, support, voter, _) = prepare_governor_and_signature(1);

    // Cast vote with invalid msg hash
    governor.cast_vote_by_sig(proposal_id, support, voter, array![r, s].span());
}

#[test]
fn test_cast_vote_by_sig_hash_generation() {
    start_cheat_chain_id_global('SN_TEST');

    let verifying_contract = 'VERIFIER'.as_address();
    let nonce = 0;
    let proposal_id = 1;
    let support = 1;
    let voter = 'VOTER'.as_address();

    let vote = Vote { verifying_contract, nonce, proposal_id, support, voter };
    let hash = vote.get_message_hash(voter);

    // This hash was computed using starknet js sdk from the following values:
    // - name: 'DAPP_NAME'
    // - version: 'DAPP_VERSION'
    // - chainId: 'SN_TEST'
    // - account: 'VOTER'
    // - nonce: 0
    // - verifying_contract: 'VERIFIER'
    // - proposal_id: 1
    // - support: 1
    // - voter: 'VOTER'
    // - revision: '1'
    let expected_hash = 0x6541a00fa95d4796bded177fca3cee29d9697174edadebfa4b0b9784379f636;
    assert_eq!(hash, expected_hash);
}

//
// cast_vote_with_reason_and_params_by_sig
//

fn prepare_governor_and_signature_with_reason_and_params(
    reason: @ByteArray, params: Span<felt252>, nonce: felt252,
) -> (IGovernorDispatcher, felt252, felt252, felt252, u8, ContractAddress, u256) {
    let mut governor = deploy_governor();
    let calls = get_calls(OTHER, false);
    let description = "proposal description";

    // Mock the get_past_votes call
    let quorum = GovernorMock::QUORUM;
    start_mock_call(VOTES_TOKEN, selector!("get_past_votes"), quorum);

    // 1. Propose
    let mut current_time = 10;
    start_cheat_block_timestamp_global(current_time);
    let proposal_id = governor.propose(calls, description.clone());

    // 2. Fast forward the vote delay
    current_time += GovernorMock::VOTING_DELAY;
    start_cheat_block_timestamp_global(current_time);

    // 3. Generate a key pair and set up an account
    let key_pair = StarkCurveKeyPairImpl::generate();
    let voter = setup_account(key_pair.public_key);

    // 4. Set up signature parameters
    let support = 1;
    let verifying_contract = governor.contract_address;
    let reason_hash = reason.hash();

    // 5. Create and sign the vote message
    let vote = VoteWithReasonAndParams {
        verifying_contract, nonce, proposal_id, support, voter, reason_hash, params,
    };
    let msg_hash = vote.get_message_hash(voter);
    let (r, s) = key_pair.sign(msg_hash).unwrap();

    (governor, r, s, proposal_id, support, voter, quorum)
}

#[test]
fn test_cast_vote_with_reason_and_params_by_sig() {
    let reason = "proposal reason";
    let params = array!['param'].span();

    let (governor, r, s, proposal_id, support, voter, quorum) =
        prepare_governor_and_signature_with_reason_and_params(
        @reason, params, 0,
    );

    // Set up event spy and cast vote
    let mut spy = spy_events();
    governor
        .cast_vote_with_reason_and_params_by_sig(
            proposal_id, support, voter, reason.clone(), params, array![r, s].span(),
        );

    spy
        .assert_only_event_vote_cast_with_params(
            governor.contract_address, voter, proposal_id, support, quorum, @reason, params,
        );
}

#[test]
fn test_cast_vote_with_reason_and_params_by_sig_empty_params() {
    let reason = "proposal reason";
    let params = array![].span();

    let (governor, r, s, proposal_id, support, voter, quorum) =
        prepare_governor_and_signature_with_reason_and_params(
        @reason, params, 0,
    );

    // Set up event spy and cast vote
    let mut spy = spy_events();
    governor
        .cast_vote_with_reason_and_params_by_sig(
            proposal_id, support, voter, reason.clone(), params, array![r, s].span(),
        );

    spy
        .assert_only_event_vote_cast(
            governor.contract_address, voter, proposal_id, support, quorum, @reason,
        );
}

#[test]
#[should_panic(expected: 'Invalid signature')]
fn test_cast_vote_with_reason_and_params_by_sig_invalid_signature() {
    let reason = "proposal reason";
    let params = array!['param'].span();

    let (governor, r, s, proposal_id, support, voter, _) =
        prepare_governor_and_signature_with_reason_and_params(
        @reason, params, 0,
    );

    // Cast vote with invalid signature
    governor
        .cast_vote_with_reason_and_params_by_sig(
            proposal_id, support, voter, reason.clone(), params, array![r + 1, s].span(),
        );
}

#[test]
#[should_panic(expected: 'Invalid signature')]
fn test_cast_vote_with_reason_and_params_by_sig_invalid_msg_hash() {
    let reason = "proposal reason";
    let params = array!['param'].span();

    // Use invalid nonce (not the account's current nonce)
    let invalid_nonce = 1;
    let (governor, r, s, proposal_id, support, voter, _) =
        prepare_governor_and_signature_with_reason_and_params(
        @reason, params, invalid_nonce,
    );

    // Cast vote with invalid msg hash
    governor
        .cast_vote_with_reason_and_params_by_sig(
            proposal_id, support, voter, reason.clone(), params, array![r, s].span(),
        );
}

#[test]
fn test_cast_vote_with_reason_and_params_by_sig_hash_generation() {
    start_cheat_chain_id_global('SN_TEST');

    let verifying_contract = 'VERIFIER'.as_address();
    let nonce = 0;
    let proposal_id = 1;
    let support = 1;
    let voter = 'VOTER'.as_address();
    let reason_hash = 'hash';
    let params = array!['param'].span();
    let vote = VoteWithReasonAndParams {
        verifying_contract, nonce, proposal_id, support, voter, reason_hash, params,
    };
    let hash = vote.get_message_hash(voter);

    // This hash was computed using starknet js sdk from the following values:
    // - name: 'DAPP_NAME'
    // - version: 'DAPP_VERSION'
    // - chainId: 'SN_TEST'
    // - account: 'VOTER'
    // - nonce: 0
    // - verifying_contract: 'VERIFIER'
    // - proposal_id: 1
    // - support: 1
    // - voter: 'VOTER'
    // - reason_hash: 'hash'
    // - params: ['param']
    // - revision: '1'
    let expected_hash = 0x729b7bd36fcddae615f7e2d7c78270e7f820f0dec9faf7842e0187670d3e84a;
    assert_eq!(hash, expected_hash);
}

//
// nonces
//

#[test]
fn test_nonces() {
    let mut state = COMPONENT_STATE();
    let nonce = state.nonces(OTHER);
    assert_eq!(nonce, 0);

    state.Governor_nonces.write(OTHER, 1);
    let nonce = state.nonces(OTHER);
    assert_eq!(nonce, 1);
}

//
// relay
//

#[test]
fn test_relay() {
    let (mut governor, target) = setup_dispatchers();
    let new_number = 1;
    let contract_address = governor.contract_address;

    let call = Call {
        to: target.contract_address,
        selector: selector!("set_number"),
        calldata: array![new_number].span(),
    };

    let number = target.get_number();
    assert_eq!(number, 0);

    start_cheat_caller_address(contract_address, contract_address);
    governor.relay(call);

    let number = target.get_number();
    assert_eq!(number, new_number);
}

#[test]
#[should_panic(expected: 'Expected failure')]
fn test_relay_panics() {
    let (mut governor, target) = setup_dispatchers();
    let contract_address = governor.contract_address;

    let call = Call {
        to: target.contract_address,
        selector: selector!("failing_function"),
        calldata: array![].span(),
    };

    start_cheat_caller_address(contract_address, contract_address);
    governor.relay(call);
}

#[test]
#[should_panic(expected: 'Executor only')]
fn test_relay_invalid_caller() {
    let mut state = COMPONENT_STATE();
    let call = Call { to: ADMIN, selector: selector!("foo"), calldata: array![].span() };

    start_cheat_caller_address(test_address(), OTHER);
    state.relay(call);
}

//
// Internal
//

#[test]
fn test_initializer() {
    let mut state = COMPONENT_STATE();
    let contract_state = CONTRACT_STATE();

    state.initializer();

    assert!(contract_state.supports_interface(IGOVERNOR_ID));
}

//
// get_proposal
//

#[test]
fn test_get_empty_proposal() {
    let mut state = COMPONENT_STATE();

    let proposal = state.get_proposal(0);

    assert_eq!(proposal.proposer, ZERO);
    assert_eq!(proposal.vote_start, 0);
    assert_eq!(proposal.vote_duration, 0);
    assert_eq!(proposal.executed, false);
    assert_eq!(proposal.canceled, false);
    assert_eq!(proposal.eta_seconds, 0);
}

#[test]
fn test_get_proposal() {
    let mut state = COMPONENT_STATE();
    let (_, expected_proposal) = get_proposal_info();

    state.Governor_proposals.write(1, expected_proposal);

    let proposal = state.get_proposal(1);
    assert_eq!(proposal, expected_proposal);
}

//
// is_valid_description_for_proposer
//

#[test]
fn test_is_valid_description_too_short() {
    let state = COMPONENT_STATE();
    let short_description: ByteArray =
        "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff";
    assert_eq!(short_description.len(), 75);

    let is_valid = state.is_valid_description_for_proposer(ADMIN, @short_description);
    assert!(is_valid);
}

#[test]
fn test_is_valid_description_wrong_suffix() {
    let state = COMPONENT_STATE();
    let description =
        "?proposer=0x04718f5a0fc34cc1af16a1cdee98ffb20c31f5cd61d6ab07201858f4287c938d";

    let is_valid = state.is_valid_description_for_proposer(ADMIN, @description);
    assert!(is_valid);
}

#[test]
fn test_is_valid_description_wrong_proposer() {
    let state = COMPONENT_STATE();
    let description =
        "#proposer=0x04718f5a0fc34cc1af16a1cdee98ffb20c31f5cd61d6ab07201858f4287c938d";

    let is_valid = state.is_valid_description_for_proposer(ADMIN, @description);
    assert!(!is_valid);
}

#[test]
fn test_is_valid_description_valid_proposer() {
    let state = COMPONENT_STATE();
    let address = ADMIN.to_byte_array(16, 64);
    let mut description: ByteArray = "#proposer=0x";

    description.append(@address);

    let is_valid = state.is_valid_description_for_proposer(ADMIN, @description);
    assert!(is_valid);
}

//
// _hash_proposal
//

#[test]
fn test__hash_proposal() {
    let state = COMPONENT_STATE();
    let calls = get_calls(ZERO, false);
    let description = @"proposal description";
    let description_hash = description.hash();

    let expected_hash = hash_proposal(calls, description_hash);
    let hash = state._hash_proposal(calls, description_hash);

    assert_eq!(hash, expected_hash);
}

//
// Proposal info
//

#[test]
fn test__proposal_threshold() {
    let mut state = COMPONENT_STATE();

    let threshold = state._proposal_threshold();
    let expected = GovernorMock::PROPOSAL_THRESHOLD;
    assert_eq!(threshold, expected);
}

#[test]
fn test__proposal_snapshot() {
    let mut state = COMPONENT_STATE();
    let (id, proposal) = get_proposal_info();

    state.Governor_proposals.write(id, proposal);

    let snapshot = state._proposal_snapshot(id);
    let expected = proposal.vote_start;
    assert_eq!(snapshot, expected);
}

#[test]
fn test__proposal_deadline() {
    let mut state = COMPONENT_STATE();
    let (id, proposal) = get_proposal_info();

    state.Governor_proposals.write(id, proposal);

    let deadline = state._proposal_deadline(id);
    let expected = proposal.vote_start + proposal.vote_duration;
    assert_eq!(deadline, expected);
}

#[test]
fn test__proposal_proposer() {
    let mut state = COMPONENT_STATE();
    let (id, proposal) = get_proposal_info();

    state.Governor_proposals.write(id, proposal);

    let proposer = state._proposal_proposer(id);
    let expected = proposal.proposer;
    assert_eq!(proposer, expected);
}

#[test]
fn test__proposal_eta() {
    let mut state = COMPONENT_STATE();
    let (id, proposal) = get_proposal_info();

    state.Governor_proposals.write(id, proposal);

    let eta = state._proposal_eta(id);
    let expected = proposal.eta_seconds;
    assert_eq!(eta, expected);
}

//
// assert_only_governance
//

#[test]
fn test_assert_only_governance() {
    let mut state = COMPONENT_STATE();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, contract_address);

    state.assert_only_governance();
}

#[test]
#[should_panic(expected: 'Executor only')]
fn test_assert_only_governance_not_executor() {
    let mut state = COMPONENT_STATE();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, OTHER);

    state.assert_only_governance();
}

//
// validate_state
//

#[test]
fn test_validate_state() {
    let mut state = COMPONENT_STATE();
    let (id, proposal) = get_proposal_info();

    state.Governor_proposals.write(id, proposal);

    // Current should be Pending
    let current_state = state._state(id);
    assert_eq!(current_state, ProposalState::Pending);

    let valid_states = array![ProposalState::Pending];
    state.validate_state(id, valid_states.span());

    let valid_states = array![ProposalState::Pending, ProposalState::Active];
    state.validate_state(id, valid_states.span());

    let valid_states = array![
        ProposalState::Executed, ProposalState::Active, ProposalState::Pending,
    ];
    state.validate_state(id, valid_states.span());
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test_validate_state_invalid() {
    let mut state = COMPONENT_STATE();
    let (id, proposal) = get_proposal_info();

    state.Governor_proposals.write(id, proposal);

    // Current should be Pending
    let current_state = state._state(id);
    assert_eq!(current_state, ProposalState::Pending);

    let valid_states = array![ProposalState::Active].span();
    state.validate_state(id, valid_states);
}

//
// _get_votes
//

#[test]
fn test__get_votes() {
    let mut state = COMPONENT_STATE();
    let timepoint = 0;
    let expected_weight = 100;
    let params = array!['param'].span();

    // Mock the get_past_votes call
    start_mock_call(Zero::zero(), selector!("get_past_votes"), expected_weight);

    let votes = state._get_votes(OTHER, timepoint, params);
    assert_eq!(votes, expected_weight);
}

//
// _state
//

#[test]
fn test__state_executed() {
    let mut state = COMPONENT_STATE();

    // The function already asserts the state
    setup_executed_proposal(ref state, false);
}

#[test]
fn test__state_canceled() {
    let mut state = COMPONENT_STATE();

    // The function already asserts the state
    setup_canceled_proposal(ref state, false);
}

#[test]
#[should_panic(expected: 'Nonexistent proposal')]
fn test__state_non_existent() {
    let state = COMPONENT_STATE();

    state._state(1);
}

#[test]
fn test__state_pending() {
    let mut state = COMPONENT_STATE();

    // The function already asserts the state
    setup_pending_proposal(ref state, false);
}

#[test]
fn test__state_active() {
    test_state_active_external_version(false);
}

#[test]
fn test__state_defeated_quorum_not_reached() {
    test_state_defeated_quorum_not_reached_external_version(false);
}

#[test]
fn test__state_defeated_vote_not_succeeded() {
    test_state_defeated_vote_not_succeeded_external_version(false);
}

#[test]
fn test__state_queued() {
    let mut mock_state = CONTRACT_STATE();

    // The function already asserts the state
    setup_queued_proposal(ref mock_state, false);
}

#[test]
fn test__state_succeeded() {
    let mut mock_state = CONTRACT_STATE();

    // The function already asserts the state
    setup_succeeded_proposal(ref mock_state, false);
}

//
// _propose
//

#[test]
fn test__propose() {
    test_propose_external_version(false);
}

#[test]
#[should_panic(expected: 'Existent proposal')]
fn test__propose_existent_proposal() {
    let mut state = COMPONENT_STATE();
    let calls = get_calls(OTHER, false);
    let description = @"proposal description";
    let proposer = ADMIN;

    let id = state._propose(calls, description, proposer);
    let expected_id = hash_proposal(calls, description.hash());
    assert_eq!(id, expected_id);

    // Propose again
    state._propose(calls, description, proposer);
}

//
// _cancel
//

#[test]
fn test__cancel_pending() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_pending_proposal(ref state, false);

    state._cancel(id, 0);

    let canceled_proposal = state.get_proposal(id);
    assert_eq!(canceled_proposal.canceled, true);
}

#[test]
fn test__cancel_active() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_active_proposal(ref state, false);

    state._cancel(id, 0);

    let canceled_proposal = state.get_proposal(id);
    assert_eq!(canceled_proposal.canceled, true);
}

#[test]
fn test__cancel_defeated() {
    let mut mock_state = CONTRACT_STATE();
    let (id, _) = setup_defeated_proposal(ref mock_state, false);

    mock_state.governor._cancel(id, 0);

    let canceled_proposal = mock_state.governor.get_proposal(id);
    assert_eq!(canceled_proposal.canceled, true);
}

#[test]
fn test__cancel_succeeded() {
    let mut mock_state = CONTRACT_STATE();
    let (id, _) = setup_succeeded_proposal(ref mock_state, false);

    mock_state.governor._cancel(id, 0);

    let canceled_proposal = mock_state.governor.get_proposal(id);
    assert_eq!(canceled_proposal.canceled, true);
}

#[test]
fn test__cancel_queued() {
    let mut mock_state = CONTRACT_STATE();
    let (id, _) = setup_queued_proposal(ref mock_state, false);

    mock_state.governor._cancel(id, 0);

    let canceled_proposal = mock_state.governor.get_proposal(id);
    assert_eq!(canceled_proposal.canceled, true);
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test__cancel_canceled() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_canceled_proposal(ref state, false);

    // Cancel again
    state._cancel(id, 0);
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test__cancel_executed() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_executed_proposal(ref state, false);

    state._cancel(id, 0);
}

//
// _cast_vote
//

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test__cast_vote_pending() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_pending_proposal(ref state, false);
    let params = array![].span();

    state._cast_vote(id, OTHER, 0, "", params);
}

#[test]
fn test__cast_vote_active_no_params() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_active_proposal(ref state, false);
    let mut spy = spy_events();
    let contract_address = test_address();

    let reason = "reason";
    let params = array![].span();
    let expected_weight = 100;

    // Mock the get_past_votes call
    start_mock_call(Zero::zero(), selector!("get_past_votes"), expected_weight);

    let weight = state._cast_vote(id, OTHER, 0, reason, params);
    assert_eq!(weight, expected_weight);

    spy.assert_only_event_vote_cast(contract_address, OTHER, id, 0, expected_weight, @"reason");
}

#[test]
fn test__cast_vote_active_with_params() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_active_proposal(ref state, false);
    let mut spy = spy_events();
    let contract_address = test_address();

    let reason = "reason";
    let params = array!['param'].span();
    let expected_weight = 100;

    // Mock the get_past_votes call
    start_mock_call(Zero::zero(), selector!("get_past_votes"), expected_weight);

    let weight = state._cast_vote(id, OTHER, 0, reason, params);
    assert_eq!(weight, expected_weight);

    spy
        .assert_event_vote_cast_with_params(
            contract_address, OTHER, id, 0, expected_weight, @"reason", params,
        );
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test__cast_vote_defeated() {
    let mut mock_state = CONTRACT_STATE();
    let (id, _) = setup_defeated_proposal(ref mock_state, false);
    let params = array![].span();

    mock_state.governor._cast_vote(id, OTHER, 0, "", params);
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test__cast_vote_succeeded() {
    let mut mock_state = CONTRACT_STATE();
    let (id, _) = setup_succeeded_proposal(ref mock_state, false);
    let params = array![].span();

    mock_state.governor._cast_vote(id, OTHER, 0, "", params);
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test__cast_vote_queued() {
    let mut mock_state = CONTRACT_STATE();
    let (id, _) = setup_queued_proposal(ref mock_state, false);
    let params = array![].span();

    mock_state.governor._cast_vote(id, OTHER, 0, "", params);
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test__cast_vote_canceled() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_canceled_proposal(ref state, false);
    let params = array![].span();

    state._cast_vote(id, OTHER, 0, "", params);
}

#[test]
#[should_panic(expected: 'Unexpected proposal state')]
fn test__cast_vote_executed() {
    let mut state = COMPONENT_STATE();
    let (id, _) = setup_executed_proposal(ref state, false);
    let params = array![].span();

    state._cast_vote(id, OTHER, 0, "", params);
}

//
// Event helpers
//

#[generate_trait]
pub(crate) impl GovernorSpyHelpersImpl of GovernorSpyHelpers {
    fn assert_event_proposal_created(
        ref self: EventSpy,
        contract: ContractAddress,
        proposal_id: felt252,
        proposer: ContractAddress,
        calls: Span<Call>,
        signatures: Span<Span<felt252>>,
        vote_start: u64,
        vote_end: u64,
        description: @ByteArray,
    ) {
        let expected = GovernorComponent::Event::ProposalCreated(
            GovernorComponent::ProposalCreated {
                proposal_id,
                proposer,
                calls,
                signatures,
                vote_start,
                vote_end,
                description: description.clone(),
            },
        );
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_proposal_created(
        ref self: EventSpy,
        contract: ContractAddress,
        proposal_id: felt252,
        proposer: ContractAddress,
        calls: Span<Call>,
        signatures: Span<Span<felt252>>,
        vote_start: u64,
        vote_end: u64,
        description: @ByteArray,
    ) {
        self
            .assert_event_proposal_created(
                contract,
                proposal_id,
                proposer,
                calls,
                signatures,
                vote_start,
                vote_end,
                description,
            );
        self.assert_no_events_left_from(contract);
    }

    fn assert_event_vote_cast(
        ref self: EventSpy,
        contract: ContractAddress,
        voter: ContractAddress,
        proposal_id: felt252,
        support: u8,
        weight: u256,
        reason: @ByteArray,
    ) {
        let expected = GovernorComponent::Event::VoteCast(
            GovernorComponent::VoteCast {
                voter, proposal_id, support, weight, reason: reason.clone(),
            },
        );
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_vote_cast(
        ref self: EventSpy,
        contract: ContractAddress,
        voter: ContractAddress,
        proposal_id: felt252,
        support: u8,
        weight: u256,
        reason: @ByteArray,
    ) {
        self.assert_event_vote_cast(contract, voter, proposal_id, support, weight, reason);
        self.assert_no_events_left_from(contract);
    }

    fn assert_event_vote_cast_with_params(
        ref self: EventSpy,
        contract: ContractAddress,
        voter: ContractAddress,
        proposal_id: felt252,
        support: u8,
        weight: u256,
        reason: @ByteArray,
        params: Span<felt252>,
    ) {
        let expected = GovernorComponent::Event::VoteCastWithParams(
            GovernorComponent::VoteCastWithParams {
                voter, proposal_id, support, weight, reason: reason.clone(), params,
            },
        );
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_vote_cast_with_params(
        ref self: EventSpy,
        contract: ContractAddress,
        voter: ContractAddress,
        proposal_id: felt252,
        support: u8,
        weight: u256,
        reason: @ByteArray,
        params: Span<felt252>,
    ) {
        self
            .assert_event_vote_cast_with_params(
                contract, voter, proposal_id, support, weight, reason, params,
            );
        self.assert_no_events_left_from(contract);
    }

    fn assert_event_proposal_queued(
        ref self: EventSpy, contract: ContractAddress, proposal_id: felt252, eta_seconds: u64,
    ) {
        let expected = GovernorComponent::Event::ProposalQueued(
            GovernorComponent::ProposalQueued { proposal_id, eta_seconds },
        );
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_proposal_queued(
        ref self: EventSpy, contract: ContractAddress, proposal_id: felt252, eta_seconds: u64,
    ) {
        self.assert_event_proposal_queued(contract, proposal_id, eta_seconds);
        self.assert_no_events_left_from(contract);
    }

    fn assert_event_proposal_executed(
        ref self: EventSpy, contract: ContractAddress, proposal_id: felt252,
    ) {
        let expected = GovernorComponent::Event::ProposalExecuted(
            GovernorComponent::ProposalExecuted { proposal_id },
        );
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_proposal_executed(
        ref self: EventSpy, contract: ContractAddress, proposal_id: felt252,
    ) {
        self.assert_event_proposal_executed(contract, proposal_id);
        self.assert_no_events_left_from(contract);
    }

    fn assert_event_proposal_canceled(
        ref self: EventSpy, contract: ContractAddress, proposal_id: felt252,
    ) {
        let expected = GovernorComponent::Event::ProposalCanceled(
            GovernorComponent::ProposalCanceled { proposal_id },
        );
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_proposal_canceled(
        ref self: EventSpy, contract: ContractAddress, proposal_id: felt252,
    ) {
        self.assert_event_proposal_canceled(contract, proposal_id);
        self.assert_no_events_left_from(contract);
    }
}
