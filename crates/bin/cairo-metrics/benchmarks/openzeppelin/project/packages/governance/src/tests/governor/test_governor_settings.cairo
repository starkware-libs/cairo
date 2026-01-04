use openzeppelin_test_common::mocks::governor::GovernorTimelockedMock::SNIP12MetadataImpl;
use openzeppelin_testing::constants::OTHER;
use openzeppelin_testing::{EventSpyExt, EventSpyQueue as EventSpy, spy_events};
use snforge_std::{start_cheat_caller_address, test_address};
use starknet::ContractAddress;
use starknet::storage::StoragePointerWriteAccess;
use crate::governor::DefaultConfig;
use crate::governor::GovernorComponent::InternalImpl;
use crate::governor::extensions::GovernorSettingsComponent;
use crate::governor::extensions::GovernorSettingsComponent::{
    GovernorSettings, GovernorSettingsAdminImpl, InternalImpl as GovernorSettingsInternalImpl,
};
use crate::tests::governor::common::{
    COMPONENT_STATE_TIMELOCKED as COMPONENT_STATE, CONTRACT_STATE_TIMELOCKED as CONTRACT_STATE,
    set_executor,
};

//
// Extensions
//

#[test]
fn test_voting_delay() {
    let mut mock_state = CONTRACT_STATE();
    let component_state = @COMPONENT_STATE();
    let expected = 15;

    assert_eq!(GovernorSettings::voting_delay(component_state), 0);
    mock_state.governor_settings.Governor_voting_delay.write(expected);
    assert_eq!(GovernorSettings::voting_delay(component_state), expected);
}

#[test]
fn test_voting_period() {
    let mut mock_state = CONTRACT_STATE();
    let component_state = @COMPONENT_STATE();
    let expected = 45;

    assert_eq!(GovernorSettings::voting_period(component_state), 0);
    mock_state.governor_settings.Governor_voting_period.write(expected);
    assert_eq!(GovernorSettings::voting_period(component_state), expected);
}

#[test]
fn test_proposal_threshold() {
    let mut mock_state = CONTRACT_STATE();
    let component_state = @COMPONENT_STATE();
    let expected = 100;

    assert_eq!(GovernorSettings::proposal_threshold(component_state), 0);
    mock_state.governor_settings.Governor_proposal_threshold.write(expected);
    assert_eq!(GovernorSettings::proposal_threshold(component_state), expected);
}

//
// External
//

//
// set_voting_delay
//

#[test]
fn test_set_voting_delay() {
    let mut mock_state = CONTRACT_STATE();
    let component_state = @COMPONENT_STATE();
    let contract_address = test_address();
    let mut spy = spy_events();

    let expected = 15;

    set_executor(ref mock_state, OTHER);
    start_cheat_caller_address(contract_address, OTHER);

    assert_eq!(GovernorSettings::voting_delay(component_state), 0);
    mock_state.governor_settings.set_voting_delay(expected);
    assert_eq!(GovernorSettings::voting_delay(component_state), expected);

    spy.assert_only_event_voting_delay_updated(contract_address, 0, expected);
}

#[test]
fn test_set_voting_delay_no_change() {
    let mut mock_state = CONTRACT_STATE();
    let component_state = @COMPONENT_STATE();
    let mut spy = spy_events();

    let expected = 15;
    mock_state.governor_settings.Governor_voting_delay.write(expected);

    set_executor(ref mock_state, OTHER);
    start_cheat_caller_address(test_address(), OTHER);

    mock_state.governor_settings.set_voting_delay(expected);
    assert_eq!(GovernorSettings::voting_delay(component_state), expected);

    spy.assert_no_events_left();
}

#[test]
#[should_panic(expected: 'Executor only')]
fn test_set_voting_delay_only_governance() {
    let mut mock_state = CONTRACT_STATE();
    let expected = 15;

    set_executor(ref mock_state, OTHER);

    mock_state.governor_settings.set_voting_delay(expected);
}

//
// set_voting_period
//

#[test]
fn test_set_voting_period() {
    let mut mock_state = CONTRACT_STATE();
    let component_state = @COMPONENT_STATE();
    let contract_address = test_address();
    let mut spy = spy_events();

    let expected = 15;

    set_executor(ref mock_state, OTHER);
    start_cheat_caller_address(contract_address, OTHER);

    assert_eq!(GovernorSettings::voting_period(component_state), 0);
    mock_state.governor_settings.set_voting_period(expected);
    assert_eq!(GovernorSettings::voting_period(component_state), expected);

    spy.assert_only_event_voting_period_updated(contract_address, 0, expected);
}

#[test]
fn test_set_voting_period_no_change() {
    let mut mock_state = CONTRACT_STATE();
    let component_state = @COMPONENT_STATE();
    let mut spy = spy_events();

    let expected = 15;
    mock_state.governor_settings.Governor_voting_period.write(expected);

    set_executor(ref mock_state, OTHER);
    start_cheat_caller_address(test_address(), OTHER);

    mock_state.governor_settings.set_voting_period(expected);
    assert_eq!(GovernorSettings::voting_period(component_state), expected);

    spy.assert_no_events_left();
}

#[test]
#[should_panic(expected: 'Executor only')]
fn test_set_voting_period_only_governance() {
    let mut mock_state = CONTRACT_STATE();
    let expected = 15;

    set_executor(ref mock_state, OTHER);

    mock_state.governor_settings.set_voting_period(expected);
}

//
// set_proposal_threshold
//

#[test]
fn test_set_proposal_threshold() {
    let mut mock_state = CONTRACT_STATE();
    let component_state = @COMPONENT_STATE();
    let contract_address = test_address();
    let mut spy = spy_events();

    let expected = 15;

    set_executor(ref mock_state, OTHER);
    start_cheat_caller_address(contract_address, OTHER);

    assert_eq!(GovernorSettings::proposal_threshold(component_state), 0);
    mock_state.governor_settings.set_proposal_threshold(expected);
    assert_eq!(GovernorSettings::proposal_threshold(component_state), expected);

    spy.assert_only_event_proposal_threshold_updated(contract_address, 0, expected);
}

#[test]
fn test_set_proposal_threshold_no_change() {
    let mut mock_state = CONTRACT_STATE();
    let component_state = @COMPONENT_STATE();
    let mut spy = spy_events();

    let expected = 15;
    mock_state.governor_settings.Governor_proposal_threshold.write(expected);

    set_executor(ref mock_state, OTHER);
    start_cheat_caller_address(test_address(), OTHER);

    mock_state.governor_settings.set_proposal_threshold(expected);
    assert_eq!(GovernorSettings::proposal_threshold(component_state), expected);

    spy.assert_no_events_left();
}

#[test]
#[should_panic(expected: 'Executor only')]
fn test_set_proposal_threshold_only_governance() {
    let mut mock_state = CONTRACT_STATE();
    let expected = 15;

    set_executor(ref mock_state, OTHER);

    mock_state.governor_settings.set_proposal_threshold(expected);
}

//
// Internal
//

//
// initializer
//

#[test]
fn test_initializer() {
    let mut mock_state = CONTRACT_STATE();
    let component_state = @COMPONENT_STATE();
    let mut spy = spy_events();

    mock_state.governor_settings.initializer(15, 45, 100);

    assert_eq!(GovernorSettings::voting_delay(component_state), 15);
    spy.assert_event_voting_delay_updated(test_address(), 0, 15);

    assert_eq!(GovernorSettings::voting_period(component_state), 45);
    spy.assert_event_voting_period_updated(test_address(), 0, 45);

    assert_eq!(GovernorSettings::proposal_threshold(component_state), 100);
    spy.assert_only_event_proposal_threshold_updated(test_address(), 0, 100);
}

//
// assert_only_governance
//

#[test]
fn test_assert_only_governance() {
    let mut mock_state = CONTRACT_STATE();

    set_executor(ref mock_state, OTHER);
    start_cheat_caller_address(test_address(), OTHER);

    mock_state.governor_settings.assert_only_governance();
}

#[test]
#[should_panic(expected: 'Executor only')]
fn test_assert_only_governance_not_executor() {
    let mut mock_state = CONTRACT_STATE();

    set_executor(ref mock_state, OTHER);

    mock_state.governor_settings.assert_only_governance();
}


//
// _set_voting_delay
//

#[test]
fn test__set_voting_delay() {
    let mut mock_state = CONTRACT_STATE();
    let component_state = @COMPONENT_STATE();
    let contract_address = test_address();
    let mut spy = spy_events();

    let expected = 15;

    set_executor(ref mock_state, OTHER);
    start_cheat_caller_address(contract_address, OTHER);

    assert_eq!(GovernorSettings::voting_delay(component_state), 0);
    mock_state.governor_settings._set_voting_delay(expected);
    assert_eq!(GovernorSettings::voting_delay(component_state), expected);

    spy.assert_only_event_voting_delay_updated(contract_address, 0, expected);
}

#[test]
fn test__set_voting_delay_no_change() {
    let mut mock_state = CONTRACT_STATE();
    let component_state = @COMPONENT_STATE();
    let mut spy = spy_events();

    let expected = 15;
    mock_state.governor_settings.Governor_voting_delay.write(expected);

    set_executor(ref mock_state, OTHER);
    start_cheat_caller_address(test_address(), OTHER);

    mock_state.governor_settings._set_voting_delay(expected);
    assert_eq!(GovernorSettings::voting_delay(component_state), expected);

    spy.assert_no_events_left();
}

//
// _set_voting_period
//

#[test]
fn test__set_voting_period() {
    let mut mock_state = CONTRACT_STATE();
    let component_state = @COMPONENT_STATE();
    let contract_address = test_address();
    let mut spy = spy_events();

    let expected = 15;

    set_executor(ref mock_state, OTHER);
    start_cheat_caller_address(contract_address, OTHER);

    assert_eq!(GovernorSettings::voting_period(component_state), 0);
    mock_state.governor_settings._set_voting_period(expected);
    assert_eq!(GovernorSettings::voting_period(component_state), expected);

    spy.assert_only_event_voting_period_updated(contract_address, 0, expected);
}

#[test]
fn test__set_voting_period_no_change() {
    let mut mock_state = CONTRACT_STATE();
    let component_state = @COMPONENT_STATE();
    let mut spy = spy_events();

    let expected = 15;
    mock_state.governor_settings.Governor_voting_period.write(expected);

    set_executor(ref mock_state, OTHER);
    start_cheat_caller_address(test_address(), OTHER);

    mock_state.governor_settings._set_voting_period(expected);
    assert_eq!(GovernorSettings::voting_period(component_state), expected);

    spy.assert_no_events_left();
}

//
// _set_proposal_threshold
//

#[test]
fn test__set_proposal_threshold() {
    let mut mock_state = CONTRACT_STATE();
    let component_state = @COMPONENT_STATE();
    let contract_address = test_address();
    let mut spy = spy_events();

    let expected = 15;

    set_executor(ref mock_state, OTHER);
    start_cheat_caller_address(contract_address, OTHER);

    assert_eq!(GovernorSettings::proposal_threshold(component_state), 0);
    mock_state.governor_settings._set_proposal_threshold(expected);
    assert_eq!(GovernorSettings::proposal_threshold(component_state), expected);

    spy.assert_only_event_proposal_threshold_updated(contract_address, 0, expected);
}

#[test]
fn test__set_proposal_threshold_no_change() {
    let mut mock_state = CONTRACT_STATE();
    let component_state = @COMPONENT_STATE();
    let mut spy = spy_events();

    let expected = 15;
    mock_state.governor_settings.Governor_proposal_threshold.write(expected);

    set_executor(ref mock_state, OTHER);
    start_cheat_caller_address(test_address(), OTHER);

    mock_state.governor_settings._set_proposal_threshold(expected);
    assert_eq!(GovernorSettings::proposal_threshold(component_state), expected);

    spy.assert_no_events_left();
}

//
// Event helpers
//

#[generate_trait]
pub(crate) impl GovernorSettingsSpyHelpersImpl of GovernorSettingsSpyHelpers {
    fn assert_event_voting_delay_updated(
        ref self: EventSpy, contract: ContractAddress, old_voting_delay: u64, new_voting_delay: u64,
    ) {
        let expected = GovernorSettingsComponent::Event::VotingDelayUpdated(
            GovernorSettingsComponent::VotingDelayUpdated { old_voting_delay, new_voting_delay },
        );
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_voting_delay_updated(
        ref self: EventSpy, contract: ContractAddress, old_voting_delay: u64, new_voting_delay: u64,
    ) {
        self.assert_event_voting_delay_updated(contract, old_voting_delay, new_voting_delay);
        self.assert_no_events_left_from(contract);
    }

    fn assert_event_voting_period_updated(
        ref self: EventSpy,
        contract: ContractAddress,
        old_voting_period: u64,
        new_voting_period: u64,
    ) {
        let expected = GovernorSettingsComponent::Event::VotingPeriodUpdated(
            GovernorSettingsComponent::VotingPeriodUpdated { old_voting_period, new_voting_period },
        );
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_voting_period_updated(
        ref self: EventSpy,
        contract: ContractAddress,
        old_voting_period: u64,
        new_voting_period: u64,
    ) {
        self.assert_event_voting_period_updated(contract, old_voting_period, new_voting_period);
        self.assert_no_events_left_from(contract);
    }

    fn assert_event_proposal_threshold_updated(
        ref self: EventSpy,
        contract: ContractAddress,
        old_proposal_threshold: u256,
        new_proposal_threshold: u256,
    ) {
        let expected = GovernorSettingsComponent::Event::ProposalThresholdUpdated(
            GovernorSettingsComponent::ProposalThresholdUpdated {
                old_proposal_threshold, new_proposal_threshold,
            },
        );
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_proposal_threshold_updated(
        ref self: EventSpy,
        contract: ContractAddress,
        old_proposal_threshold: u256,
        new_proposal_threshold: u256,
    ) {
        self
            .assert_event_proposal_threshold_updated(
                contract, old_proposal_threshold, new_proposal_threshold,
            );
        self.assert_no_events_left_from(contract);
    }
}
