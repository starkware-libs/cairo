use core::num::traits::Zero;
use openzeppelin_test_common::mocks::access::DualCaseTwoStepOwnableMock;
use openzeppelin_test_common::ownable::OwnableSpyHelpers;
use openzeppelin_testing::constants::{NEW_OWNER, OTHER, OWNER, ZERO};
use openzeppelin_testing::{EventSpyExt, EventSpyQueue as EventSpy, spy_events};
use snforge_std::{start_cheat_caller_address, test_address};
use starknet::ContractAddress;
use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
use crate::ownable::OwnableComponent;
use crate::ownable::OwnableComponent::{InternalTrait, OwnershipTransferStarted};
use crate::ownable::interface::{IOwnableTwoStep, IOwnableTwoStepCamelOnly};

//
// Setup
//

type ComponentState = OwnableComponent::ComponentState<DualCaseTwoStepOwnableMock::ContractState>;


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
fn test_initializer_owner_pending_owner() {
    let mut state = COMPONENT_STATE();
    let mut spy = spy_events();
    assert!(state.Ownable_owner.read().is_zero());
    assert!(state.Ownable_pending_owner.read().is_zero());
    state.initializer(OWNER);

    spy.assert_only_event_ownership_transferred(test_address(), ZERO, OWNER);

    assert_eq!(state.Ownable_owner.read(), OWNER);
    assert!(state.Ownable_pending_owner.read().is_zero());
}

//
// _propose_owner
//

#[test]
fn test__propose_owner() {
    let mut state = setup();
    let mut spy = spy_events();

    state._propose_owner(OTHER);

    spy.assert_event_ownership_transfer_started(test_address(), OWNER, OTHER);
    assert_eq!(state.owner(), OWNER);
    assert_eq!(state.pending_owner(), OTHER);
}

// transfer_ownership & transferOwnership

#[test]
fn test_transfer_ownership() {
    let mut state = setup();
    let mut spy = spy_events();
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, OWNER);
    state.transfer_ownership(OTHER);

    spy.assert_event_ownership_transfer_started(contract_address, OWNER, OTHER);
    assert_eq!(state.owner(), OWNER);
    assert_eq!(state.pending_owner(), OTHER);

    // Transferring to yet another owner while pending is set should work
    state.transfer_ownership(NEW_OWNER);

    spy.assert_event_ownership_transfer_started(contract_address, OWNER, NEW_OWNER);
    assert_eq!(state.owner(), OWNER);
    assert_eq!(state.pending_owner(), NEW_OWNER);
}

#[test]
fn test_transfer_ownership_to_zero() {
    let mut state = setup();
    let mut spy = spy_events();
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, OWNER);
    state.transfer_ownership(ZERO);

    spy.assert_event_ownership_transfer_started(contract_address, OWNER, ZERO);
    assert_eq!(state.owner(), OWNER);
    assert_eq!(state.pending_owner(), ZERO);
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

    spy.assert_event_ownership_transfer_started(contract_address, OWNER, OTHER);
    assert_eq!(state.owner(), OWNER);
    assert_eq!(state.pendingOwner(), OTHER);

    // Transferring to yet another owner while pending is set should work
    state.transferOwnership(NEW_OWNER);

    spy.assert_event_ownership_transfer_started(contract_address, OWNER, NEW_OWNER);
    assert_eq!(state.owner(), OWNER);
    assert_eq!(state.pendingOwner(), NEW_OWNER);
}

#[test]
fn test_transferOwnership_to_zero() {
    let mut state = setup();
    let mut spy = spy_events();
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, OWNER);
    state.transferOwnership(ZERO);

    spy.assert_event_ownership_transfer_started(contract_address, OWNER, ZERO);
    assert_eq!(state.owner(), OWNER);
    assert!(state.pendingOwner().is_zero());
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_transferOwnership_from_nonowner() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OTHER);
    state.transferOwnership(OTHER);
}

//
// accept_ownership & acceptOwnership
//

#[test]
fn test_accept_ownership() {
    let mut state = setup();
    let mut spy = spy_events();
    let contract_address = test_address();
    state.Ownable_pending_owner.write(OTHER);
    start_cheat_caller_address(contract_address, OTHER);

    state.accept_ownership();

    spy.assert_only_event_ownership_transferred(contract_address, OWNER, OTHER);
    assert_eq!(state.owner(), OTHER);
    assert!(state.pending_owner().is_zero());
}

#[test]
#[should_panic(expected: 'Caller is not the pending owner')]
fn test_accept_ownership_from_nonpending() {
    let mut state = setup();
    state.Ownable_pending_owner.write(NEW_OWNER);
    start_cheat_caller_address(test_address(), OTHER);
    state.accept_ownership();
}

#[test]
fn test_acceptOwnership() {
    let mut state = setup();
    let mut spy = spy_events();
    let contract_address = test_address();
    state.Ownable_pending_owner.write(OTHER);
    start_cheat_caller_address(contract_address, OTHER);

    state.acceptOwnership();

    spy.assert_only_event_ownership_transferred(contract_address, OWNER, OTHER);
    assert_eq!(state.owner(), OTHER);
    assert!(state.pendingOwner().is_zero());
}

#[test]
#[should_panic(expected: 'Caller is not the pending owner')]
fn test_acceptOwnership_from_nonpending() {
    let mut state = setup();
    state.Ownable_pending_owner.write(NEW_OWNER);
    start_cheat_caller_address(test_address(), OTHER);
    state.acceptOwnership();
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
fn test_renounce_ownership_resets_pending_owner() {
    let mut state = setup();
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, OWNER);

    state.Ownable_pending_owner.write(OTHER);
    let current_pending_owner = state.Ownable_pending_owner.read();
    assert_eq!(current_pending_owner, OTHER);

    state.renounce_ownership();

    let current_pending_owner = state.Ownable_pending_owner.read();
    assert!(current_pending_owner.is_zero());
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

#[test]
fn test_full_two_step_transfer() {
    let mut state = setup();
    let mut spy = spy_events();
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, OWNER);
    state.transfer_ownership(OTHER);

    spy.assert_event_ownership_transfer_started(contract_address, OWNER, OTHER);
    assert_eq!(state.owner(), OWNER);
    assert_eq!(state.pending_owner(), OTHER);

    start_cheat_caller_address(contract_address, OTHER);
    state.accept_ownership();

    spy.assert_only_event_ownership_transferred(contract_address, OWNER, OTHER);
    assert_eq!(state.owner(), OTHER);
    assert!(state.pending_owner().is_zero());
}

//
// Helpers
//

#[generate_trait]
impl TwoStepSpyHelpersImpl of TwoStepSpyHelpers {
    fn assert_event_ownership_transfer_started(
        ref self: EventSpy,
        from_address: ContractAddress,
        previous_owner: ContractAddress,
        new_owner: ContractAddress,
    ) {
        let expected = OwnableComponent::Event::OwnershipTransferStarted(
            OwnershipTransferStarted { previous_owner, new_owner },
        );
        self.assert_emitted_single(from_address, expected);
    }
}
