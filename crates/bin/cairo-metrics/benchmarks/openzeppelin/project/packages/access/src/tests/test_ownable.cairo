use core::num::traits::Zero;
use openzeppelin_test_common::mocks::access::DualCaseOwnableMock;
use openzeppelin_test_common::ownable::OwnableSpyHelpers;
use openzeppelin_testing::constants::{OTHER, OWNER, RECIPIENT, ZERO};
use openzeppelin_testing::spy_events;
use snforge_std::{start_cheat_caller_address, test_address};
use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
use crate::ownable::OwnableComponent;
use crate::ownable::OwnableComponent::InternalTrait;
use crate::ownable::interface::{IOwnable, IOwnableCamelOnly};

//
// Setup
//

type ComponentState = OwnableComponent::ComponentState<DualCaseOwnableMock::ContractState>;

fn COMPONENT_STATE() -> ComponentState {
    OwnableComponent::component_state_for_testing()
}

fn setup() -> ComponentState {
    let mut state = COMPONENT_STATE();
    state.initializer(OWNER);
    state
}

//
// initializer
//

#[test]
fn test_initializer_owner() {
    let mut state = COMPONENT_STATE();
    let mut spy = spy_events();

    let current_owner = state.Ownable_owner.read();
    assert!(current_owner.is_zero());

    state.initializer(OWNER);

    spy.assert_only_event_ownership_transferred(test_address(), ZERO, OWNER);

    let new_owner = state.Ownable_owner.read();
    assert_eq!(new_owner, OWNER);
}

#[test]
#[should_panic(expected: 'New owner is the zero address')]
fn test_initializer_zero_owner() {
    let mut state = COMPONENT_STATE();
    state.initializer(ZERO);
}

//
// assert_only_owner
//

#[test]
fn test_assert_only_owner() {
    let state = setup();
    start_cheat_caller_address(test_address(), OWNER);
    state.assert_only_owner();
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_assert_only_owner_when_not_owner() {
    let state = setup();
    start_cheat_caller_address(test_address(), OTHER);
    state.assert_only_owner();
}

//
// _transfer_ownership
//

#[test]
fn test__transfer_ownership() {
    let mut state = setup();
    let mut spy = spy_events();
    state._transfer_ownership(OTHER);

    spy.assert_only_event_ownership_transferred(test_address(), OWNER, OTHER);

    let current_owner = state.Ownable_owner.read();
    assert_eq!(current_owner, OTHER);
}

#[test]
fn test__transfer_ownership_resets_pending_owner() {
    let mut state = setup();

    state.Ownable_pending_owner.write(OTHER);
    let current_pending_owner = state.Ownable_pending_owner.read();
    assert_eq!(current_pending_owner, OTHER);

    state._transfer_ownership(RECIPIENT);

    let current_pending_owner = state.Ownable_pending_owner.read();
    assert!(current_pending_owner.is_zero());
}

//
// transfer_ownership & transferOwnership
//

#[test]
fn test_transfer_ownership() {
    let mut state = setup();
    let mut spy = spy_events();
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, OWNER);
    state.transfer_ownership(OTHER);

    spy.assert_only_event_ownership_transferred(contract_address, OWNER, OTHER);
    assert_eq!(state.owner(), OTHER);
}

#[test]
#[should_panic(expected: 'New owner is the zero address')]
fn test_transfer_ownership_to_zero() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OWNER);
    state.transfer_ownership(ZERO);
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_transfer_ownership_from_nonowner() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OTHER);
    state.transfer_ownership(OTHER);
}

#[test]
fn test_transferOwnership() {
    let mut state = setup();
    let mut spy = spy_events();
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, OWNER);
    state.transferOwnership(OTHER);

    spy.assert_only_event_ownership_transferred(contract_address, OWNER, OTHER);
    assert_eq!(state.owner(), OTHER);
}

#[test]
#[should_panic(expected: 'New owner is the zero address')]
fn test_transferOwnership_to_zero() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OWNER);
    state.transferOwnership(ZERO);
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_transferOwnership_from_nonowner() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OTHER);
    state.transferOwnership(OTHER);
}

//
// renounce_ownership & renounceOwnership
//

#[test]
fn test_renounce_ownership() {
    let mut state = setup();
    let mut spy = spy_events();
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, OWNER);
    state.renounce_ownership();

    spy.assert_only_event_ownership_transferred(contract_address, OWNER, ZERO);
    assert!(state.owner().is_zero());
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_renounce_ownership_from_nonowner() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OTHER);
    state.renounce_ownership();
}

#[test]
fn test_renounceOwnership() {
    let mut state = setup();
    let mut spy = spy_events();
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, OWNER);
    state.renounceOwnership();

    spy.assert_only_event_ownership_transferred(contract_address, OWNER, ZERO);
    assert!(state.owner().is_zero());
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_renounceOwnership_from_nonowner() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OTHER);
    state.renounceOwnership();
}
