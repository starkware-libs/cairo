use openzeppelin_test_common::mocks::security::{
    IReentrancyMockDispatcher, IReentrancyMockDispatcherTrait, ReentrancyMock,
};
use openzeppelin_testing as utils;
use starknet::storage::StoragePointerReadAccess;
use crate::ReentrancyGuardComponent;
use crate::ReentrancyGuardComponent::InternalImpl;

type ComponentState = ReentrancyGuardComponent::ComponentState<ReentrancyMock::ContractState>;

fn COMPONENT_STATE() -> ComponentState {
    ReentrancyGuardComponent::component_state_for_testing()
}

fn deploy_mock() -> IReentrancyMockDispatcher {
    let calldata = array![];
    let address = utils::declare_and_deploy("ReentrancyMock", calldata);
    IReentrancyMockDispatcher { contract_address: address }
}

//
// ReentrancyGuard direct call tests
//

#[test]
fn test_reentrancy_guard_start() {
    let mut state = COMPONENT_STATE();

    let not_entered = !state.ReentrancyGuard_entered.read();
    assert!(not_entered);

    state.start();

    let entered = state.ReentrancyGuard_entered.read();
    assert!(entered);
}

#[test]
#[should_panic(expected: 'ReentrancyGuard: reentrant call')]
fn test_reentrancy_guard_start_when_started() {
    let mut state = COMPONENT_STATE();

    state.start();
    state.start();
}

#[test]
fn test_reentrancy_guard_end() {
    let mut state = COMPONENT_STATE();

    state.start();

    let entered = state.ReentrancyGuard_entered.read();
    assert!(entered);

    state.end();

    let not_entered = !state.ReentrancyGuard_entered.read();
    assert!(not_entered);
}

//
// Mock implementation tests
//

#[test]
#[should_panic(expected: 'ReentrancyGuard: reentrant call')]
fn test_remote_callback() {
    let contract = deploy_mock();

    // Deploy attacker
    let calldata = array![];
    let attacker_addr = utils::declare_and_deploy("Attacker", calldata);

    contract.count_and_call(attacker_addr);
}

#[test]
#[should_panic(expected: 'ReentrancyGuard: reentrant call')]
fn test_local_recursion() {
    let contract = deploy_mock();
    contract.count_local_recursive(10);
}

#[test]
#[should_panic(expected: 'ReentrancyGuard: reentrant call')]
fn test_external_recursion() {
    let contract = deploy_mock();
    contract.count_external_recursive(10);
}

#[test]
fn test_nonreentrant_function_call() {
    let contract = deploy_mock();
    contract.callback();
    assert_eq!(contract.current_count(), 1, "Call should execute");
}
