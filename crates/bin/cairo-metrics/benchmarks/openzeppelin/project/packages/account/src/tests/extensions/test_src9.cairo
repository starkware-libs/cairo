use openzeppelin_introspection::interface::{ISRC5, ISRC5_ID};
use openzeppelin_test_common::mocks::src9::SRC9AccountMock;
use openzeppelin_testing as utils;
use openzeppelin_testing::constants::{FELT_VALUE, OTHER, OWNER, RECIPIENT};
use openzeppelin_utils::cryptography::snip12::OffchainMessageHash;
use snforge_std::signature::KeyPairTrait;
use snforge_std::signature::stark_curve::{StarkCurveKeyPairImpl, StarkCurveSignerImpl};
use snforge_std::{
    CheatSpan, cheat_caller_address, load, start_cheat_block_timestamp_global,
    start_cheat_caller_address, test_address,
};
use starknet::ContractAddress;
use starknet::account::Call;
use starknet::storage::StorageMapWriteAccess;
use crate::extensions::SRC9Component;
use crate::extensions::SRC9Component::{InternalImpl, OutsideExecutionV2Impl, SNIP12MetadataImpl};
use crate::extensions::src9::interface::{
    ISRC9_V2Dispatcher, ISRC9_V2DispatcherTrait, ISRC9_V2_ID, OutsideExecution,
};
use crate::extensions::src9::snip12_utils::OutsideExecutionStructHash;

//
// Setup
//

type ComponentState = SRC9Component::ComponentState<SRC9AccountMock::ContractState>;

fn CONTRACT_STATE() -> SRC9AccountMock::ContractState {
    SRC9AccountMock::contract_state_for_testing()
}

fn COMPONENT_STATE() -> ComponentState {
    SRC9Component::component_state_for_testing()
}

fn setup() -> ComponentState {
    let mut state = COMPONENT_STATE();
    state.initializer();
    state
}

fn setup_account(public_key: felt252) -> ContractAddress {
    let mut calldata = array![public_key];
    utils::declare_and_deploy("SRC9AccountMock", calldata)
}

//
// initializer
//

#[test]
fn test_initializer() {
    let mut state = COMPONENT_STATE();
    let mock_state = CONTRACT_STATE();

    state.initializer();

    let supports_isrc5 = mock_state.supports_interface(ISRC5_ID);
    assert!(supports_isrc5);

    let supports_ioutside_execution_v2 = mock_state.supports_interface(ISRC9_V2_ID);
    assert!(supports_ioutside_execution_v2);
}

//
// execute_from_outside_v2
//

#[test]
fn test_execute_from_outside_v2_any_caller() {
    let key_pair = KeyPairTrait::generate();
    let account = setup_account(key_pair.public_key);
    let outside_execution = setup_outside_execution(account, false);

    let msg_hash = outside_execution.get_message_hash(account);
    let (r, s) = key_pair.sign(msg_hash).unwrap();

    // Use the dispatcher to simulate the appropriate context
    let dispatcher = ISRC9_V2Dispatcher { contract_address: account };
    dispatcher.execute_from_outside_v2(outside_execution, array![r, s].span());

    assert_value(account, FELT_VALUE);
}

#[test]
fn test_execute_from_outside_v2_specific_caller() {
    let key_pair = KeyPairTrait::generate();
    let account = setup_account(key_pair.public_key);
    let mut outside_execution = setup_outside_execution(account, false);
    outside_execution.caller = OWNER;

    let msg_hash = outside_execution.get_message_hash(account);
    let (r, s) = key_pair.sign(msg_hash).unwrap();

    cheat_caller_address(account, OWNER, CheatSpan::TargetCalls(1));

    // Use the dispatcher to simulate the appropriate context
    let dispatcher = ISRC9_V2Dispatcher { contract_address: account };
    dispatcher.execute_from_outside_v2(outside_execution, array![r, s].span());

    assert_value(account, FELT_VALUE);
}

#[test]
fn test_execute_from_outside_v2_uses_nonce() {
    let key_pair = KeyPairTrait::generate();
    let account = setup_account(key_pair.public_key);
    let outside_execution = setup_outside_execution(account, false);

    // Use the dispatcher to simulate the appropriate context
    let dispatcher = ISRC9_V2Dispatcher { contract_address: account };

    let is_valid_nonce = dispatcher.is_valid_outside_execution_nonce(outside_execution.nonce);
    assert!(is_valid_nonce);

    let msg_hash = outside_execution.get_message_hash(account);
    let (r, s) = key_pair.sign(msg_hash).unwrap();

    dispatcher.execute_from_outside_v2(outside_execution, array![r, s].span());

    assert_value(account, FELT_VALUE);

    let is_invalid_nonce = !dispatcher.is_valid_outside_execution_nonce(outside_execution.nonce);
    assert!(is_invalid_nonce);
}

#[test]
#[should_panic(expected: 'SRC9: invalid caller')]
fn test_execute_from_outside_v2_caller_mismatch() {
    let mut state = setup();
    let mut outside_execution = setup_outside_execution(RECIPIENT, false);
    outside_execution.caller = OWNER;

    start_cheat_caller_address(test_address(), OTHER);

    state.execute_from_outside_v2(outside_execution, array![].span());
}

#[test]
#[should_panic(expected: 'SRC9: now >= execute_before')]
fn test_execute_from_outside_v2_call_after_execute_before() {
    let mut state = setup();
    let outside_execution = setup_outside_execution(RECIPIENT, false);

    start_cheat_block_timestamp_global(25);

    state.execute_from_outside_v2(outside_execution, array![].span());
}

#[test]
#[should_panic(expected: 'SRC9: now >= execute_before')]
fn test_execute_from_outside_v2_call_equal_to_execute_before() {
    let mut state = setup();
    let outside_execution = setup_outside_execution(RECIPIENT, false);

    start_cheat_block_timestamp_global(20);

    state.execute_from_outside_v2(outside_execution, array![].span());
}

#[test]
#[should_panic(expected: 'SRC9: now <= execute_after')]
fn test_execute_from_outside_v2_call_before_execute_after() {
    let mut state = setup();
    let outside_execution = setup_outside_execution(RECIPIENT, false);

    start_cheat_block_timestamp_global(5);

    state.execute_from_outside_v2(outside_execution, array![].span());
}

#[test]
#[should_panic(expected: 'SRC9: now <= execute_after')]
fn test_execute_from_outside_v2_call_equal_to_execute_after() {
    let mut state = setup();
    let outside_execution = setup_outside_execution(RECIPIENT, false);

    start_cheat_block_timestamp_global(10);

    state.execute_from_outside_v2(outside_execution, array![].span());
}

#[test]
#[should_panic(expected: 'SRC9: duplicated nonce')]
fn test_execute_from_outside_v2_invalid_nonce() {
    let mut state = setup();
    let outside_execution = setup_outside_execution(RECIPIENT, false);

    state.SRC9_nonces.write(outside_execution.nonce, true);

    state.execute_from_outside_v2(outside_execution, array![].span());
}

#[test]
#[should_panic(expected: 'SRC9: invalid signature')]
fn test_execute_from_outside_v2_invalid_signature() {
    let key_pair = KeyPairTrait::generate();
    let account = setup_account(key_pair.public_key);
    let outside_execution = setup_outside_execution(account, false);

    let msg_hash = outside_execution.get_message_hash(account);
    let (r, s) = key_pair.sign(msg_hash).unwrap();
    let invalid_signature = array![r, s + 1].span();

    // Use the dispatcher to simulate the appropriate context
    let dispatcher = ISRC9_V2Dispatcher { contract_address: account };
    dispatcher.execute_from_outside_v2(outside_execution, invalid_signature);
}

#[test]
#[should_panic(expected: "Some error")]
fn test_execute_from_outside_v2_panics_when_inner_call_panic() {
    let key_pair = KeyPairTrait::generate();
    let account = setup_account(key_pair.public_key);
    let outside_execution = setup_outside_execution(account, true);

    let msg_hash = outside_execution.get_message_hash(account);
    let (r, s) = key_pair.sign(msg_hash).unwrap();

    // Use the dispatcher to simulate the appropriate context
    let dispatcher = ISRC9_V2Dispatcher { contract_address: account };
    dispatcher.execute_from_outside_v2(outside_execution, array![r, s].span());
}

//
// Getters
//

#[test]
fn test_is_valid_outside_execution_nonce_valid() {
    let state = setup();

    let nonce = 5;
    let is_valid_nonce = state.is_valid_outside_execution_nonce(nonce);
    assert!(is_valid_nonce);
}

#[test]
fn test_is_valid_outside_execution_nonce_invalid() {
    let mut state = setup();

    let nonce = 5;
    state.SRC9_nonces.write(nonce, true);
    let is_invalid_nonce = !state.is_valid_outside_execution_nonce(nonce);
    assert!(is_invalid_nonce);
}

//
// Helpers
//

fn setup_outside_execution(target: ContractAddress, panic: bool) -> OutsideExecution {
    let call = Call {
        to: target,
        selector: selector!("set_value"),
        calldata: array![FELT_VALUE, panic.into()].span(),
    };
    let caller = 'ANY_CALLER'.try_into().unwrap();
    let nonce = 5;
    let execute_after = 10;
    let execute_before = 20;
    let calls = array![call].span();

    // Set a valid timestamp for the execution time span
    start_cheat_block_timestamp_global(15);

    OutsideExecution { caller, nonce, execute_after, execute_before, calls }
}

fn assert_value(target: ContractAddress, expected_value: felt252) {
    let value = *load(target, selector!("value"), 1).at(0);
    assert_eq!(value, expected_value);
}
