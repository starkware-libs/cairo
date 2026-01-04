use openzeppelin_test_common::mocks::governor::GovernorQuorumFractionMock;
use openzeppelin_test_common::mocks::governor::GovernorQuorumFractionMock::SNIP12MetadataImpl;
use openzeppelin_testing::constants::{OTHER, VOTES_TOKEN, ZERO};
use openzeppelin_testing::{EventSpyExt, EventSpyQueue as EventSpy, spy_events};
use snforge_std::{start_cheat_block_timestamp_global, start_mock_call, store, test_address};
use starknet::ContractAddress;
use crate::governor::GovernorComponent::InternalImpl;
use crate::governor::extensions::GovernorVotesQuorumFractionComponent;
use crate::governor::extensions::GovernorVotesQuorumFractionComponent::{
    GovernorQuorum, GovernorVotes, InternalTrait, QuorumFractionImpl,
};
use crate::governor::{DefaultConfig, GovernorComponent};

pub type ComponentState =
    GovernorComponent::ComponentState<GovernorQuorumFractionMock::ContractState>;

pub fn CONTRACT_STATE() -> GovernorQuorumFractionMock::ContractState {
    GovernorQuorumFractionMock::contract_state_for_testing()
}

pub fn COMPONENT_STATE() -> ComponentState {
    GovernorComponent::component_state_for_testing()
}

//
// GovernorQuorum
//

#[test]
fn test_quorum() {
    let component_state = COMPONENT_STATE();
    let mock_state = CONTRACT_STATE();
    let past_total_supply = 100;
    let timepoint = 123;

    start_mock_call(ZERO, selector!("get_past_total_supply"), past_total_supply);

    let quorum = GovernorQuorum::quorum(@component_state, timepoint);
    let quorum_numerator = mock_state.governor_votes_quorum_fraction.quorum_numerator(timepoint);
    let quorum_denominator = mock_state.governor_votes_quorum_fraction.quorum_denominator();

    assert_eq!(quorum, quorum_numerator * past_total_supply / quorum_denominator);
}

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
    let token = mock_state.governor_votes_quorum_fraction.token();
    assert_eq!(token, VOTES_TOKEN);
}

#[test]
fn test_quorum_denominator() {
    let mock_state = CONTRACT_STATE();
    let quorum_denominator = mock_state.governor_votes_quorum_fraction.quorum_denominator();
    assert_eq!(quorum_denominator, 1000);
}

//
// Internal
//

#[test]
fn test_initializer() {
    let mut mock_state = CONTRACT_STATE();
    let now = starknet::get_block_timestamp();

    mock_state.governor_votes_quorum_fraction.initializer(VOTES_TOKEN, 600);

    let quorum_numerator = mock_state.governor_votes_quorum_fraction.quorum_numerator(now);
    assert_eq!(quorum_numerator, 600);

    let token = mock_state.governor_votes_quorum_fraction.token();
    assert_eq!(token, VOTES_TOKEN);
}

#[test]
#[should_panic(expected: 'Invalid votes token')]
fn test_initializer_with_zero_token() {
    let mut mock_state = CONTRACT_STATE();
    mock_state.governor_votes_quorum_fraction.initializer(ZERO, 600);
}


#[test]
#[should_panic(expected: 'Invalid quorum fraction')]
fn test_initializer_with_invalid_numerator() {
    let mut mock_state = CONTRACT_STATE();
    mock_state.governor_votes_quorum_fraction.initializer(VOTES_TOKEN, 1001);
}

//
// update_quorum_numerator
//

#[test]
#[should_panic(expected: 'Invalid quorum fraction')]
fn test_update_quorum_numerator_invalid_numerator() {
    let mut mock_state = CONTRACT_STATE();
    mock_state.governor_votes_quorum_fraction.update_quorum_numerator(1001);
}

#[test]
fn test_update_quorum_numerator() {
    let mut mock_state = CONTRACT_STATE();
    let ts1 = '10';
    let ts2 = '20';
    let ts3 = '30';
    let ts4 = '15';
    let ts5 = '35';
    let new_quorum_numerator_1 = 700;
    let new_quorum_numerator_2 = 800;
    let new_quorum_numerator_3 = 900;

    let mut spy = spy_events();
    let contract_address = test_address();

    // 1. Update the numerators
    start_cheat_block_timestamp_global(ts1);
    mock_state.governor_votes_quorum_fraction.update_quorum_numerator(new_quorum_numerator_1);
    spy.assert_only_event_quorum_numerator_updated(contract_address, 0, new_quorum_numerator_1);

    start_cheat_block_timestamp_global(ts2);
    mock_state.governor_votes_quorum_fraction.update_quorum_numerator(new_quorum_numerator_2);
    spy
        .assert_only_event_quorum_numerator_updated(
            contract_address, new_quorum_numerator_1, new_quorum_numerator_2,
        );

    start_cheat_block_timestamp_global(ts3);
    mock_state.governor_votes_quorum_fraction.update_quorum_numerator(new_quorum_numerator_3);
    spy
        .assert_only_event_quorum_numerator_updated(
            contract_address, new_quorum_numerator_2, new_quorum_numerator_3,
        );

    // 2. Check the current quorum numerator
    let current_quorum_numerator = mock_state
        .governor_votes_quorum_fraction
        .current_quorum_numerator();
    assert_eq!(current_quorum_numerator, new_quorum_numerator_3);

    // 3. Check the history
    let history = mock_state.governor_votes_quorum_fraction.quorum_numerator(ts1);
    assert_eq!(history, new_quorum_numerator_1);

    let history = mock_state.governor_votes_quorum_fraction.quorum_numerator(ts2);
    assert_eq!(history, new_quorum_numerator_2);

    let history = mock_state.governor_votes_quorum_fraction.quorum_numerator(ts3);
    assert_eq!(history, new_quorum_numerator_3);

    let history = mock_state.governor_votes_quorum_fraction.quorum_numerator(ts4);
    assert_eq!(history, new_quorum_numerator_1);

    let history = mock_state.governor_votes_quorum_fraction.quorum_numerator(ts5);
    assert_eq!(history, new_quorum_numerator_3);
}

//
// Event helpers
//

#[generate_trait]
pub(crate) impl GovernorSettingsSpyHelpersImpl of GovernorSettingsSpyHelpers {
    fn assert_event_quorum_numerator_updated(
        ref self: EventSpy,
        contract: ContractAddress,
        old_quorum_numerator: u256,
        new_quorum_numerator: u256,
    ) {
        let expected = GovernorVotesQuorumFractionComponent::Event::QuorumNumeratorUpdated(
            GovernorVotesQuorumFractionComponent::QuorumNumeratorUpdated {
                old_quorum_numerator, new_quorum_numerator,
            },
        );
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_quorum_numerator_updated(
        ref self: EventSpy,
        contract: ContractAddress,
        old_quorum_numerator: u256,
        new_quorum_numerator: u256,
    ) {
        self
            .assert_event_quorum_numerator_updated(
                contract, old_quorum_numerator, new_quorum_numerator,
            );
        self.assert_no_events_left_from(contract);
    }
}
