use openzeppelin_test_common::mocks::governor::GovernorMock::SNIP12MetadataImpl;
use openzeppelin_testing::constants::{OTHER, VOTES_TOKEN, ZERO};
use snforge_std::{start_cheat_block_timestamp_global, start_mock_call, store, test_address};
use crate::governor::DefaultConfig;
use crate::governor::GovernorComponent::InternalImpl;
use crate::governor::extensions::GovernorVotesComponent::{
    GovernorVotes, InternalTrait, VotesTokenImpl,
};
use crate::tests::governor::common::{COMPONENT_STATE, CONTRACT_STATE};

//
// GovernorVotes
//

#[test]
fn test_clock() {
    let component_state = COMPONENT_STATE();
    let timestamp = 10;

    start_cheat_block_timestamp_global(timestamp);
    let clock = GovernorVotes::clock(@component_state);
    assert_eq!(clock, timestamp);
}

#[test]
fn test_clock_mode() {
    let component_state = COMPONENT_STATE();
    let mode = GovernorVotes::clock_mode(@component_state);
    assert_eq!(mode, "mode=timestamp&from=starknet::SN_MAIN");
}

#[test]
fn test_get_votes() {
    let mut component_state = COMPONENT_STATE();
    let timepoint = 0;
    let expected_weight = 100;
    let params = array!['param'].span();

    start_mock_call(ZERO, selector!("get_past_votes"), expected_weight);

    let votes = GovernorVotes::get_votes(@component_state, OTHER, timepoint, params);
    assert_eq!(votes, expected_weight);
}

//
// External
//

#[test]
fn test_token() {
    let mock_state = CONTRACT_STATE();

    store(test_address(), selector!("Governor_token"), array![VOTES_TOKEN.into()].span());
    let token = mock_state.governor_votes.token();
    assert_eq!(token, VOTES_TOKEN);
}

//
// Internal
//

#[test]
fn test_initializer() {
    let mut mock_state = CONTRACT_STATE();

    mock_state.governor_votes.initializer(VOTES_TOKEN);

    let token = mock_state.governor_votes.token();
    assert_eq!(token, VOTES_TOKEN);
}

#[test]
#[should_panic(expected: 'Invalid votes token')]
fn test_initializer_with_zero_token() {
    let mut mock_state = CONTRACT_STATE();
    mock_state.governor_votes.initializer(ZERO);
}
