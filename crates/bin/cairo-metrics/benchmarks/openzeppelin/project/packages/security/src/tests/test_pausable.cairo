use openzeppelin_test_common::mocks::security::PausableMock;
use openzeppelin_testing::constants::CALLER;
use openzeppelin_testing::{EventSpyExt, EventSpyQueue as EventSpy, spy_events};
use snforge_std::{start_cheat_caller_address, test_address};
use starknet::ContractAddress;
use crate::PausableComponent;
use crate::PausableComponent::{InternalImpl, PausableImpl, Paused, Unpaused};

type ComponentState = PausableComponent::ComponentState<PausableMock::ContractState>;

fn COMPONENT_STATE() -> ComponentState {
    PausableComponent::component_state_for_testing()
}

//
// is_paused
//

#[test]
fn test_is_paused() {
    let mut state = COMPONENT_STATE();
    assert!(!state.is_paused());

    state.pause();
    assert!(state.is_paused());

    state.unpause();
    assert!(!state.is_paused());
}

//
// assert_paused
//

#[test]
fn test_assert_paused_when_paused() {
    let mut state = COMPONENT_STATE();
    state.pause();
    state.assert_paused();
}

#[test]
#[should_panic(expected: 'Pausable: not paused')]
fn test_assert_paused_when_not_paused() {
    let state = COMPONENT_STATE();
    state.assert_paused();
}

//
// assert_not_paused
//

#[test]
#[should_panic(expected: 'Pausable: paused')]
fn test_assert_not_paused_when_paused() {
    let mut state = COMPONENT_STATE();
    state.pause();
    state.assert_not_paused();
}

#[test]
fn test_assert_not_paused_when_not_paused() {
    let state = COMPONENT_STATE();
    state.assert_not_paused();
}

//
// pause
//

#[test]
fn test_pause_when_unpaused() {
    let mut state = COMPONENT_STATE();
    let contract_address = test_address();

    let mut spy = spy_events();
    start_cheat_caller_address(contract_address, CALLER);
    state.pause();

    spy.assert_only_event_paused(contract_address, CALLER);
    assert!(state.is_paused());
}

#[test]
#[should_panic(expected: 'Pausable: paused')]
fn test_pause_when_paused() {
    let mut state = COMPONENT_STATE();
    state.pause();
    state.pause();
}

//
// unpause
//

#[test]
fn test_unpause_when_paused() {
    let mut state = COMPONENT_STATE();
    let contract_address = test_address();

    let mut spy = spy_events();
    start_cheat_caller_address(test_address(), CALLER);
    state.pause();
    state.unpause();

    spy.assert_event_paused(contract_address, CALLER);
    spy.assert_only_event_unpaused(contract_address, CALLER);
    assert!(!state.is_paused());
}

#[test]
#[should_panic(expected: 'Pausable: not paused')]
fn test_unpause_when_unpaused() {
    let mut state = COMPONENT_STATE();
    assert!(!state.is_paused());
    state.unpause();
}

//
// Helpers
//

#[generate_trait]
impl PausableSpyHelpersImpl of PausableSpyHelpers {
    fn assert_event_paused(
        ref self: EventSpy, contract: ContractAddress, account: ContractAddress,
    ) {
        let expected = PausableComponent::Event::Paused(Paused { account });
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_paused(
        ref self: EventSpy, contract: ContractAddress, account: ContractAddress,
    ) {
        self.assert_event_paused(contract, account);
        self.assert_no_events_left_from(contract);
    }

    fn assert_event_unpaused(
        ref self: EventSpy, contract: ContractAddress, account: ContractAddress,
    ) {
        let expected = PausableComponent::Event::Unpaused(Unpaused { account });
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_unpaused(
        ref self: EventSpy, contract: ContractAddress, account: ContractAddress,
    ) {
        self.assert_event_unpaused(contract, account);
        self.assert_no_events_left_from(contract);
    }
}
