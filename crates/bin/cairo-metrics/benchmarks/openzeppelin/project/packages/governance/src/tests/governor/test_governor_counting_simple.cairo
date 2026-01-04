use core::num::traits::Bounded;
use openzeppelin_test_common::mocks::governor::GovernorMock::SNIP12MetadataImpl;
use openzeppelin_testing::constants::OTHER;
use starknet::storage::{
    StorageMapReadAccess, StorageMapWriteAccess, StoragePathEntry, StoragePointerReadAccess,
    StoragePointerWriteAccess,
};
use crate::governor::DefaultConfig;
use crate::governor::GovernorComponent::InternalImpl;
use crate::governor::extensions::GovernorCountingSimpleComponent::{GovernorCounting, VoteType};
use crate::governor::interface::IGovernor;
use crate::tests::governor::common::{COMPONENT_STATE, CONTRACT_STATE};

//
// try_into
//

#[test]
fn test_try_into_u8_VoteType() {
    let number = 0_u8;
    let vote_type: VoteType = number.try_into().unwrap();
    assert_eq!(vote_type, VoteType::Against);

    let number = 1_u8;
    let vote_type: VoteType = number.try_into().unwrap();
    assert_eq!(vote_type, VoteType::For);

    let number = 2_u8;
    let vote_type: VoteType = number.try_into().unwrap();
    assert_eq!(vote_type, VoteType::Abstain);

    let number = 3_u8;
    let result: Option<VoteType> = number.try_into();
    assert_eq!(result, Option::None);
}

//
// into
//

#[test]
fn test_into_VoteType_u8() {
    let vote_type = VoteType::Against;
    let number: u8 = vote_type.into();
    assert_eq!(number, 0);

    let vote_type = VoteType::For;
    let number: u8 = vote_type.into();
    assert_eq!(number, 1);

    let vote_type = VoteType::Abstain;
    let number: u8 = vote_type.into();
    assert_eq!(number, 2);
}

//
// counting_mode
//

#[test]
fn test_counting_mode() {
    let state = COMPONENT_STATE();
    assert_eq!(GovernorCounting::counting_mode(@state), "support=bravo&quorum=for,abstain");
}

//
// count_vote
//

#[test]
fn test_count_vote_against() {
    let mut mock_state = CONTRACT_STATE();
    let mut state = COMPONENT_STATE();

    let proposal_id = 0;
    let account = OTHER;
    let support = 0;
    let total_weight = 100;
    let params = array![].span();

    let proposal_votes = mock_state
        .governor_counting_simple
        .Governor_proposals_votes
        .entry(proposal_id);
    assert_eq!(proposal_votes.has_voted.read(account), false);
    assert_eq!(proposal_votes.against_votes.read(), 0);

    let weight = GovernorCounting::count_vote(
        ref state, proposal_id, account, support, total_weight, params,
    );
    assert_eq!(weight, total_weight);

    assert_eq!(proposal_votes.has_voted.read(account), true);
    assert_eq!(proposal_votes.against_votes.read(), total_weight);
}

#[test]
fn test_count_vote_for() {
    let mut mock_state = CONTRACT_STATE();
    let mut state = COMPONENT_STATE();

    let proposal_id = 0;
    let account = OTHER;
    let support = 1;
    let total_weight = 100;
    let params = array![].span();

    let proposal_votes = mock_state
        .governor_counting_simple
        .Governor_proposals_votes
        .entry(proposal_id);
    assert_eq!(proposal_votes.has_voted.read(account), false);
    assert_eq!(proposal_votes.for_votes.read(), 0);

    let weight = GovernorCounting::count_vote(
        ref state, proposal_id, account, support, total_weight, params,
    );
    assert_eq!(weight, total_weight);

    assert_eq!(proposal_votes.has_voted.read(account), true);
    assert_eq!(proposal_votes.for_votes.read(), total_weight);
}

#[test]
fn test_count_vote_abstain() {
    let mut mock_state = CONTRACT_STATE();
    let mut state = COMPONENT_STATE();

    let proposal_id = 0;
    let account = OTHER;
    let support = 2;
    let total_weight = 100;
    let params = array![].span();

    let proposal_votes = mock_state
        .governor_counting_simple
        .Governor_proposals_votes
        .entry(proposal_id);
    assert_eq!(proposal_votes.has_voted.read(account), false);
    assert_eq!(proposal_votes.abstain_votes.read(), 0);

    let weight = GovernorCounting::count_vote(
        ref state, proposal_id, account, support, total_weight, params,
    );
    assert_eq!(weight, total_weight);

    assert_eq!(proposal_votes.has_voted.read(account), true);
    assert_eq!(proposal_votes.abstain_votes.read(), total_weight);
}

#[test]
#[should_panic(expected: 'Already cast vote')]
fn test_count_vote_already_voted() {
    let mut mock_state = CONTRACT_STATE();
    let mut state = COMPONENT_STATE();

    let proposal_id = 0;
    let account = OTHER;
    let support = 2;
    let total_weight = 100;
    let params = array![].span();

    let proposal_votes = mock_state
        .governor_counting_simple
        .Governor_proposals_votes
        .entry(proposal_id);
    proposal_votes.has_voted.write(account, true);

    GovernorCounting::count_vote(ref state, proposal_id, account, support, total_weight, params);
}

#[test]
#[should_panic(expected: 'Invalid vote type')]
fn test_count_vote_invalid_vote_type() {
    let mut state = COMPONENT_STATE();

    let proposal_id = 0;
    let account = OTHER;
    let support = 3;
    let total_weight = 100;
    let params = array![].span();

    GovernorCounting::count_vote(ref state, proposal_id, account, support, total_weight, params);
}

//
// has_voted
//

#[test]
fn test_has_voted() {
    let mut mock_state = CONTRACT_STATE();
    let state = COMPONENT_STATE();

    let proposal_id = 0;
    let account = OTHER;

    assert_eq!(GovernorCounting::has_voted(@state, proposal_id, account), false);

    let proposal_votes = mock_state
        .governor_counting_simple
        .Governor_proposals_votes
        .entry(proposal_id);
    proposal_votes.has_voted.write(account, true);

    assert_eq!(GovernorCounting::has_voted(@state, proposal_id, account), true);
}

//
// quorum_reached
//

#[test]
fn test_quorum_reached() {
    let mut mock_state = CONTRACT_STATE();
    let state = COMPONENT_STATE();

    let timepoint = 0;
    let proposal_id = 0;

    let quorum = state.quorum(timepoint);
    let for_votes = quorum - 10;
    let abstain_votes = 10;

    assert_eq!(GovernorCounting::quorum_reached(@state, proposal_id), false);

    let mut proposal_votes = mock_state
        .governor_counting_simple
        .Governor_proposals_votes
        .entry(proposal_id);

    // 1. Set for_votes and abstain_votes sum equal to quorum
    proposal_votes.for_votes.write(for_votes);
    proposal_votes.abstain_votes.write(abstain_votes);

    assert_eq!(GovernorCounting::quorum_reached(@state, proposal_id), true);

    // 2. Set for_votes and abstain_votes sum greater than quorum
    proposal_votes.for_votes.write(for_votes + 1);
    proposal_votes.abstain_votes.write(abstain_votes + 1);

    assert_eq!(GovernorCounting::quorum_reached(@state, proposal_id), true);

    // 3. Set for_votes and abstain_votes sum less than quorum
    proposal_votes.for_votes.write(for_votes - 1);
    proposal_votes.abstain_votes.write(abstain_votes - 1);

    assert_eq!(GovernorCounting::quorum_reached(@state, proposal_id), false);
}

#[test]
fn test_quorum_reached_snapshot_used() {
    let mut mock_state = CONTRACT_STATE();
    let state = COMPONENT_STATE();

    let timepoint = 0;
    let proposal_id = 0;

    let quorum = state.quorum(timepoint);
    let for_votes = quorum + 10;

    assert_eq!(GovernorCounting::quorum_reached(@state, proposal_id), false);

    let mut proposal_votes = mock_state
        .governor_counting_simple
        .Governor_proposals_votes
        .entry(proposal_id);

    // 1. Set for_votes greater than quorum
    proposal_votes.for_votes.write(for_votes);

    assert_eq!(GovernorCounting::quorum_reached(@state, proposal_id), true);

    // 2. Set proposal snapshot to a special timepoint
    let mut proposal = mock_state.governor.Governor_proposals.read(proposal_id);
    proposal.vote_start = Bounded::MAX;
    mock_state.governor.Governor_proposals.write(proposal_id, proposal);

    assert_eq!(GovernorCounting::quorum_reached(@state, proposal_id), false);
}

//
// vote_succeeded
//

#[test]
fn test_vote_succeeded() {
    let mut mock_state = CONTRACT_STATE();
    let state = COMPONENT_STATE();

    let proposal_id = 0;

    let mut proposal_votes = mock_state
        .governor_counting_simple
        .Governor_proposals_votes
        .entry(proposal_id);

    // 1. Set for_votes greater than against_votes
    proposal_votes.for_votes.write(500);
    proposal_votes.against_votes.write(499);

    assert_eq!(GovernorCounting::vote_succeeded(@state, proposal_id), true);

    // 2. Set for_votes less than against_votes
    proposal_votes.for_votes.write(499);
    proposal_votes.against_votes.write(500);

    assert_eq!(GovernorCounting::quorum_reached(@state, proposal_id), false);

    // 3. Set for_votes equal to against_votes
    proposal_votes.for_votes.write(500);
    proposal_votes.abstain_votes.write(500);

    assert_eq!(GovernorCounting::quorum_reached(@state, proposal_id), false);
}
