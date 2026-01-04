use core::num::traits::Zero;
use openzeppelin_account::account::AccountComponent::AccountMixinImpl;
use openzeppelin_account::extensions::SRC9Component::{OutsideExecutionV2Impl, SNIP12MetadataImpl};
use openzeppelin_account::extensions::src9::interface::{ISRC9_V2_ID, OutsideExecution};
use openzeppelin_account::extensions::src9::snip12_utils::OutsideExecutionStructHash;
use openzeppelin_account::interface::ISRC6_ID;
use openzeppelin_introspection::interface::ISRC5_ID;
use openzeppelin_test_common::account::{
    AccountSpyHelpers, SIGNED_TX_DATA, SignedTransactionData, get_accept_ownership_signature,
};
use openzeppelin_test_common::erc20::deploy_erc20;
use openzeppelin_test_common::upgrades::UpgradeableSpyHelpers;
use openzeppelin_testing as utils;
use openzeppelin_testing::constants::stark::{KEY_PAIR, KEY_PAIR_2};
use openzeppelin_testing::constants::{
    CALLER, CLASS_HASH_ZERO, FELT_VALUE, MIN_TRANSACTION_VERSION, OTHER, QUERY_OFFSET,
    QUERY_VERSION, RECIPIENT, SALT, ZERO,
};
use openzeppelin_testing::signing::{SerializedSigning, StarkKeyPair};
use openzeppelin_testing::spy_events;
use openzeppelin_token::erc20::interface::IERC20DispatcherTrait;
use openzeppelin_utils::cryptography::snip12::OffchainMessageHash;
use openzeppelin_utils::serde::SerializedAppend;
use snforge_std::{
    CheatSpan, cheat_caller_address, load, start_cheat_block_timestamp_global,
    start_cheat_caller_address, start_cheat_signature_global, start_cheat_transaction_hash_global,
    start_cheat_transaction_version_global, test_address,
};
use starknet::account::Call;
use starknet::{ClassHash, ContractAddress};
use crate::AccountUpgradeable;
use crate::interfaces::account::{
    AccountUpgradeableABISafeDispatcher, AccountUpgradeableABISafeDispatcherTrait,
};
use crate::interfaces::{AccountUpgradeableABIDispatcher, AccountUpgradeableABIDispatcherTrait};

//
// Setup
//

fn declare_v2_class() -> ClassHash {
    utils::declare_class("SnakeAccountMock").class_hash
}

fn setup_dispatcher(key_pair: StarkKeyPair) -> (ContractAddress, AccountUpgradeableABIDispatcher) {
    let calldata = array![key_pair.public_key];
    let account_address = utils::declare_and_deploy("AccountUpgradeable", calldata);
    let dispatcher = AccountUpgradeableABIDispatcher { contract_address: account_address };

    (account_address, dispatcher)
}

fn setup_dispatcher_with_data(
    key_pair: StarkKeyPair, data: SignedTransactionData,
) -> (AccountUpgradeableABIDispatcher, felt252) {
    let account_class = utils::declare_class("AccountUpgradeable");
    let calldata = array![key_pair.public_key];
    let contract_address = utils::deploy(account_class, calldata);
    let account_dispatcher = AccountUpgradeableABIDispatcher { contract_address };

    start_cheat_signature_global(array![data.r, data.s].span());
    start_cheat_transaction_hash_global(data.tx_hash);
    start_cheat_transaction_version_global(MIN_TRANSACTION_VERSION);
    start_cheat_caller_address(contract_address, ZERO);

    (account_dispatcher, account_class.class_hash.into())
}

fn setup_simple_mock() -> ContractAddress {
    utils::declare_and_deploy("SimpleMock", array![])
}

//
// constructor
//

#[test]
fn test_constructor() {
    let mut state = AccountUpgradeable::contract_state_for_testing();
    let mut spy = spy_events();
    let key_pair = KEY_PAIR();
    let account_address = test_address();
    AccountUpgradeable::constructor(ref state, key_pair.public_key);

    spy.assert_only_event_owner_added(account_address, key_pair.public_key);

    let public_key = state.get_public_key();
    assert_eq!(public_key, key_pair.public_key);

    let supports_isrc5 = state.supports_interface(ISRC5_ID);
    assert!(supports_isrc5);

    let supports_isrc6 = state.supports_interface(ISRC6_ID);
    assert!(supports_isrc6);

    let supports_isrc9 = state.supports_interface(ISRC9_V2_ID);
    assert!(supports_isrc9);
}

//
// set_public_key & setPublicKey
//

#[test]
fn test_public_key_setter_and_getter() {
    let key_pair = KEY_PAIR();
    let (account_address, dispatcher) = setup_dispatcher(key_pair);
    let mut spy = spy_events();

    let new_key_pair = KEY_PAIR_2();
    let signature = get_accept_ownership_signature(
        account_address, key_pair.public_key, new_key_pair,
    );
    start_cheat_caller_address(account_address, account_address);
    dispatcher.set_public_key(new_key_pair.public_key, signature);

    assert_eq!(dispatcher.get_public_key(), new_key_pair.public_key);

    spy.assert_event_owner_removed(dispatcher.contract_address, key_pair.public_key);
    spy.assert_only_event_owner_added(dispatcher.contract_address, new_key_pair.public_key);
}

#[test]
fn test_public_key_setter_and_getter_camel() {
    let key_pair = KEY_PAIR();
    let (account_address, dispatcher) = setup_dispatcher(key_pair);
    let mut spy = spy_events();

    let new_key_pair = KEY_PAIR_2();
    let signature = get_accept_ownership_signature(
        account_address, key_pair.public_key, new_key_pair,
    );
    start_cheat_caller_address(account_address, account_address);
    dispatcher.setPublicKey(new_key_pair.public_key, signature);

    assert_eq!(dispatcher.getPublicKey(), new_key_pair.public_key);

    spy.assert_event_owner_removed(account_address, key_pair.public_key);
    spy.assert_only_event_owner_added(account_address, new_key_pair.public_key);
}

#[test]
#[should_panic(expected: 'Account: unauthorized')]
fn test_set_public_key_different_account() {
    let key_pair = KEY_PAIR();
    let (account_address, dispatcher) = setup_dispatcher(key_pair);

    let new_key_pair = KEY_PAIR_2();
    let signature = get_accept_ownership_signature(
        account_address, key_pair.public_key, new_key_pair,
    );
    dispatcher.set_public_key(new_key_pair.public_key, signature);
}

#[test]
#[should_panic(expected: 'Account: unauthorized')]
fn test_setPublicKey_different_account() {
    let key_pair = KEY_PAIR();
    let (account_address, dispatcher) = setup_dispatcher(key_pair);

    let new_key_pair = KEY_PAIR_2();
    let signature = get_accept_ownership_signature(
        account_address, key_pair.public_key, new_key_pair,
    );
    dispatcher.setPublicKey(new_key_pair.public_key, signature);
}

//
// is_valid_signature & isValidSignature
//

fn is_valid_sig_dispatcher() -> (AccountUpgradeableABIDispatcher, felt252, Array<felt252>) {
    let key_pair = KEY_PAIR();
    let (_, dispatcher) = setup_dispatcher(key_pair);

    let data = SIGNED_TX_DATA(key_pair);
    let signature = array![data.r, data.s];
    (dispatcher, data.tx_hash, signature)
}

#[test]
fn test_is_valid_signature() {
    let (dispatcher, hash, signature) = is_valid_sig_dispatcher();

    let is_valid = dispatcher.is_valid_signature(hash, signature);
    assert_eq!(is_valid, starknet::VALIDATED);
}

#[test]
fn test_is_valid_signature_bad_sig() {
    let (dispatcher, tx_hash, _) = is_valid_sig_dispatcher();
    let bad_signature = array!['BAD', 'SIG'];

    let is_valid = dispatcher.is_valid_signature(tx_hash, bad_signature);
    assert!(is_valid.is_zero(), "Should reject invalid signature");
}

#[test]
fn test_is_valid_signature_invalid_len_sig() {
    let (dispatcher, tx_hash, _) = is_valid_sig_dispatcher();
    let invalid_len_sig = array!['INVALID_LEN'];

    let is_valid = dispatcher.is_valid_signature(tx_hash, invalid_len_sig);
    assert!(is_valid.is_zero(), "Should reject signature of invalid length");
}

#[test]
fn test_isValidSignature() {
    let (dispatcher, tx_hash, signature) = is_valid_sig_dispatcher();

    let is_valid = dispatcher.isValidSignature(tx_hash, signature);
    assert_eq!(is_valid, starknet::VALIDATED);
}

#[test]
fn test_isValidSignature_bad_sig() {
    let (dispatcher, tx_hash, _) = is_valid_sig_dispatcher();
    let bad_signature = array!['BAD', 'SIG'];

    let is_valid = dispatcher.isValidSignature(tx_hash, bad_signature);
    assert!(is_valid.is_zero(), "Should reject invalid signature");
}

#[test]
fn test_isValidSignature_invalid_len_sig() {
    let (dispatcher, tx_hash, _) = is_valid_sig_dispatcher();
    let invalid_len_sig = array!['INVALID_LEN'];

    let is_valid = dispatcher.isValidSignature(tx_hash, invalid_len_sig);
    assert!(is_valid.is_zero(), "Should reject signature of invalid length");
}

//
// supports_interface
//

#[test]
fn test_supports_interface() {
    let key_pair = KEY_PAIR();
    let (_, dispatcher) = setup_dispatcher(key_pair);

    let supports_isrc5 = dispatcher.supports_interface(ISRC5_ID);
    assert!(supports_isrc5);

    let supports_isrc6 = dispatcher.supports_interface(ISRC6_ID);
    assert!(supports_isrc6);

    let doesnt_support_dummy_id = !dispatcher.supports_interface('DUMMY_INTERFACE_ID');
    assert!(doesnt_support_dummy_id);
}

//
// Entry points
//

#[test]
fn test_validate_deploy() {
    let key_pair = KEY_PAIR();
    let (account, class_hash) = setup_dispatcher_with_data(key_pair, SIGNED_TX_DATA(key_pair));

    // `__validate_deploy__` does not directly use the passed arguments. Their
    // values are already integrated in the tx hash. The passed arguments in this
    // testing context are decoupled from the signature and have no effect on the test.
    let is_valid = account.__validate_deploy__(class_hash, SALT, key_pair.public_key);
    assert_eq!(is_valid, starknet::VALIDATED);
}

#[test]
#[should_panic(expected: 'Account: invalid signature')]
fn test_validate_deploy_invalid_signature_data() {
    let key_pair = KEY_PAIR();
    let mut data = SIGNED_TX_DATA(key_pair);
    data.tx_hash += 1;
    let (account, class_hash) = setup_dispatcher_with_data(key_pair, data);

    account.__validate_deploy__(class_hash, SALT, key_pair.public_key);
}

#[test]
#[should_panic(expected: 'Account: invalid signature')]
fn test_validate_deploy_invalid_signature_length() {
    let key_pair = KEY_PAIR();
    let (account, class_hash) = setup_dispatcher_with_data(key_pair, SIGNED_TX_DATA(key_pair));

    let invalid_len_sig = array!['INVALID_LEN'];
    start_cheat_signature_global(invalid_len_sig.span());

    account.__validate_deploy__(class_hash, SALT, key_pair.public_key);
}

#[test]
#[should_panic(expected: 'Account: invalid signature')]
fn test_validate_deploy_empty_signature() {
    let key_pair = KEY_PAIR();
    let (account, class_hash) = setup_dispatcher_with_data(key_pair, SIGNED_TX_DATA(key_pair));

    let empty_sig = array![];
    start_cheat_signature_global(empty_sig.span());

    account.__validate_deploy__(class_hash, SALT, key_pair.public_key);
}

#[test]
fn test_validate_declare() {
    let key_pair = KEY_PAIR();
    let (account, class_hash) = setup_dispatcher_with_data(key_pair, SIGNED_TX_DATA(key_pair));

    // `__validate_declare__` does not directly use the class_hash argument. Its
    // value is already integrated in the tx hash. The class_hash argument in this
    // testing context is decoupled from the signature and has no effect on the test.
    let is_valid = account.__validate_declare__(class_hash);
    assert_eq!(is_valid, starknet::VALIDATED);
}

#[test]
#[should_panic(expected: 'Account: invalid signature')]
fn test_validate_declare_invalid_signature_data() {
    let key_pair = KEY_PAIR();
    let mut data = SIGNED_TX_DATA(key_pair);
    data.tx_hash += 1;
    let (account, class_hash) = setup_dispatcher_with_data(key_pair, data);

    account.__validate_declare__(class_hash);
}

#[test]
#[should_panic(expected: 'Account: invalid signature')]
fn test_validate_declare_invalid_signature_length() {
    let key_pair = KEY_PAIR();
    let (account, class_hash) = setup_dispatcher_with_data(key_pair, SIGNED_TX_DATA(key_pair));

    let invalid_len_sig = array!['INVALID_LEN'];
    start_cheat_signature_global(invalid_len_sig.span());

    account.__validate_declare__(class_hash);
}

#[test]
#[should_panic(expected: 'Account: invalid signature')]
fn test_validate_declare_empty_signature() {
    let key_pair = KEY_PAIR();
    let (account, class_hash) = setup_dispatcher_with_data(key_pair, SIGNED_TX_DATA(key_pair));

    let empty_sig = array![];
    start_cheat_signature_global(empty_sig.span());

    account.__validate_declare__(class_hash);
}

fn test_execute_with_version(version: Option<felt252>) {
    let key_pair = KEY_PAIR();
    let data = SIGNED_TX_DATA(key_pair);
    let (account, _) = setup_dispatcher_with_data(key_pair, data);
    let erc20 = deploy_erc20(account.contract_address, 1000);

    // Craft call and add to calls array
    let amount: u256 = 200;

    let recipient = RECIPIENT;
    let mut calldata = array![];
    calldata.append_serde(recipient);
    calldata.append_serde(amount);

    let call = Call {
        to: erc20.contract_address, selector: selector!("transfer"), calldata: calldata.span(),
    };
    let calls = array![call];

    // Handle version for test
    if let Option::Some(version) = version {
        start_cheat_transaction_version_global(version)
    }

    // Execute
    account.__execute__(calls);

    // Assert that the transfer was successful
    assert_eq!(erc20.balance_of(account.contract_address), 800, "Should have remainder");
    assert_eq!(erc20.balance_of(recipient), amount, "Should have transferred");
}

#[test]
fn test_execute() {
    test_execute_with_version(Option::None);
}

#[test]
fn test_execute_future_version() {
    test_execute_with_version(Option::Some(MIN_TRANSACTION_VERSION + 1));
}

#[test]
fn test_execute_query_version() {
    test_execute_with_version(Option::Some(QUERY_VERSION));
}

#[test]
#[should_panic(expected: 'Account: invalid tx version')]
fn test_execute_invalid_query_version() {
    test_execute_with_version(Option::Some(QUERY_OFFSET));
}

#[test]
fn test_execute_future_query_version() {
    test_execute_with_version(Option::Some(QUERY_VERSION + 1));
}

#[test]
#[should_panic(expected: 'Account: invalid tx version')]
fn test_execute_invalid_version() {
    test_execute_with_version(Option::Some(MIN_TRANSACTION_VERSION - 1));
}

#[test]
fn test_validate() {
    let key_pair = KEY_PAIR();
    let (account, _) = setup_dispatcher_with_data(key_pair, SIGNED_TX_DATA(key_pair));

    let calls = array![];
    let is_valid = account.__validate__(calls);
    assert_eq!(is_valid, starknet::VALIDATED);
}

#[test]
#[should_panic(expected: 'Account: invalid signature')]
fn test_validate_invalid() {
    let key_pair = KEY_PAIR();
    let mut data = SIGNED_TX_DATA(key_pair);
    data.tx_hash += 1;
    let (account, _) = setup_dispatcher_with_data(key_pair, data);

    let calls = array![];
    account.__validate__(calls);
}

#[test]
fn test_multicall() {
    let key_pair = KEY_PAIR();
    let (account, _) = setup_dispatcher_with_data(key_pair, SIGNED_TX_DATA(key_pair));
    let erc20 = deploy_erc20(account.contract_address, 1000);
    let recipient1 = RECIPIENT;
    let recipient2 = OTHER;
    let mut calls = array![];

    // Craft 1st call
    let mut calldata1 = array![];
    let amount1: u256 = 300;
    calldata1.append_serde(recipient1);
    calldata1.append_serde(amount1);
    let call1 = Call {
        to: erc20.contract_address, selector: selector!("transfer"), calldata: calldata1.span(),
    };

    // Craft 2nd call
    let mut calldata2 = array![];
    let amount2: u256 = 500;
    calldata2.append_serde(recipient2);
    calldata2.append_serde(amount2);
    let call2 = Call {
        to: erc20.contract_address, selector: selector!("transfer"), calldata: calldata2.span(),
    };

    // Bundle calls and execute
    calls.append(call1);
    calls.append(call2);
    account.__execute__(calls);

    // Assert that the transfers were successful
    assert_eq!(erc20.balance_of(account.contract_address), 200, "Should have remainder");
    assert_eq!(erc20.balance_of(recipient1), 300, "Should have transferred");
    assert_eq!(erc20.balance_of(recipient2), 500, "Should have transferred");
}

#[test]
#[should_panic(expected: 'Account: invalid caller')]
fn test_account_called_from_contract() {
    let key_pair = KEY_PAIR();
    let (account_address, dispatcher) = setup_dispatcher(key_pair);

    let calls = array![];
    start_cheat_caller_address(account_address, CALLER);
    dispatcher.__execute__(calls);
}

//
// upgrade
//

#[test]
#[should_panic(expected: 'Account: unauthorized')]
fn test_upgrade_access_control() {
    let key_pair = KEY_PAIR();
    let (_, v1_dispatcher) = setup_dispatcher(key_pair);

    v1_dispatcher.upgrade(CLASS_HASH_ZERO);
}

#[test]
#[should_panic(expected: 'Class hash cannot be zero')]
fn test_upgrade_with_class_hash_zero() {
    let key_pair = KEY_PAIR();
    let (account_address, v1_dispatcher) = setup_dispatcher(key_pair);

    start_cheat_caller_address(account_address, account_address);
    v1_dispatcher.upgrade(CLASS_HASH_ZERO);
}

#[test]
fn test_upgraded_event() {
    let key_pair = KEY_PAIR();
    let (account_address, v1_dispatcher) = setup_dispatcher(key_pair);
    let mut spy = spy_events();

    let v2_class_hash = declare_v2_class();
    start_cheat_caller_address(account_address, account_address);
    v1_dispatcher.upgrade(v2_class_hash);

    spy.assert_only_event_upgraded(account_address, v2_class_hash);
}

#[test]
#[feature("safe_dispatcher")]
fn test_v2_missing_camel_selector() {
    let key_pair = KEY_PAIR();
    let (account_address, v1_dispatcher) = setup_dispatcher(key_pair);

    let v2_class_hash = declare_v2_class();
    start_cheat_caller_address(account_address, account_address);
    v1_dispatcher.upgrade(v2_class_hash);

    let safe_dispatcher = AccountUpgradeableABISafeDispatcher { contract_address: account_address };
    let result = safe_dispatcher.getPublicKey();

    utils::assert_entrypoint_not_found_error(result, selector!("getPublicKey"), account_address)
}

#[test]
fn test_state_persists_after_upgrade() {
    let key_pair = KEY_PAIR();
    let (account_address, v1_dispatcher) = setup_dispatcher(key_pair);

    let new_key_pair = KEY_PAIR_2();
    let accept_ownership_sig = get_accept_ownership_signature(
        account_address, key_pair.public_key, new_key_pair,
    );
    start_cheat_caller_address(account_address, account_address);
    v1_dispatcher.set_public_key(new_key_pair.public_key, accept_ownership_sig);

    let expected_public_key = new_key_pair.public_key;
    let camel_public_key = v1_dispatcher.getPublicKey();
    assert_eq!(camel_public_key, expected_public_key);

    let v2_class_hash = declare_v2_class();
    v1_dispatcher.upgrade(v2_class_hash);
    let snake_public_key = v1_dispatcher.get_public_key();

    assert_eq!(snake_public_key, expected_public_key);
}

//
// execute_from_outside_v2
//

#[test]
fn test_execute_from_outside_v2_any_caller() {
    let key_pair = KEY_PAIR();
    let (account_address, dispatcher) = setup_dispatcher(key_pair);
    let simple_mock = setup_simple_mock();
    let outside_execution = setup_outside_execution(simple_mock, false);

    let msg_hash = outside_execution.get_message_hash(account_address);
    let signature = key_pair.serialized_sign(msg_hash);

    dispatcher.execute_from_outside_v2(outside_execution, signature.span());

    assert_value(simple_mock, FELT_VALUE);
}

#[test]
fn test_execute_from_outside_v2_specific_caller() {
    let key_pair = KEY_PAIR();
    let (account_address, dispatcher) = setup_dispatcher(key_pair);
    let simple_mock = setup_simple_mock();
    let mut outside_execution = setup_outside_execution(simple_mock, false);
    outside_execution.caller = CALLER;

    let msg_hash = outside_execution.get_message_hash(account_address);
    let signature = key_pair.serialized_sign(msg_hash);

    cheat_caller_address(account_address, CALLER, CheatSpan::TargetCalls(1));

    dispatcher.execute_from_outside_v2(outside_execution, signature.span());

    assert_value(simple_mock, FELT_VALUE);
}

#[test]
fn test_execute_from_outside_v2_uses_nonce() {
    let key_pair = KEY_PAIR();
    let (account_address, dispatcher) = setup_dispatcher(key_pair);
    let simple_mock = setup_simple_mock();
    let outside_execution = setup_outside_execution(simple_mock, false);

    let is_valid_nonce = dispatcher.is_valid_outside_execution_nonce(outside_execution.nonce);
    assert!(is_valid_nonce);

    let msg_hash = outside_execution.get_message_hash(account_address);
    let signature = key_pair.serialized_sign(msg_hash);

    dispatcher.execute_from_outside_v2(outside_execution, signature.span());

    assert_value(simple_mock, FELT_VALUE);

    let is_invalid_nonce = !dispatcher.is_valid_outside_execution_nonce(outside_execution.nonce);
    assert!(is_invalid_nonce);
}

#[test]
#[should_panic(expected: 'SRC9: invalid caller')]
fn test_execute_from_outside_v2_caller_mismatch() {
    let key_pair = KEY_PAIR();
    let (account_address, dispatcher) = setup_dispatcher(key_pair);
    let mut outside_execution = setup_outside_execution(account_address, false);
    outside_execution.caller = CALLER;

    start_cheat_caller_address(account_address, OTHER);

    dispatcher.execute_from_outside_v2(outside_execution, array![].span());
}

#[test]
#[should_panic(expected: 'SRC9: now >= execute_before')]
fn test_execute_from_outside_v2_call_after_execute_before() {
    let key_pair = KEY_PAIR();
    let (account_address, dispatcher) = setup_dispatcher(key_pair);
    let outside_execution = setup_outside_execution(account_address, false);

    start_cheat_block_timestamp_global(25);

    dispatcher.execute_from_outside_v2(outside_execution, array![].span());
}

#[test]
#[should_panic(expected: 'SRC9: now >= execute_before')]
fn test_execute_from_outside_v2_call_equal_to_execute_before() {
    let key_pair = KEY_PAIR();
    let (account_address, dispatcher) = setup_dispatcher(key_pair);
    let outside_execution = setup_outside_execution(account_address, false);

    start_cheat_block_timestamp_global(20);

    dispatcher.execute_from_outside_v2(outside_execution, array![].span());
}

#[test]
#[should_panic(expected: 'SRC9: now <= execute_after')]
fn test_execute_from_outside_v2_call_before_execute_after() {
    let key_pair = KEY_PAIR();
    let (account_address, dispatcher) = setup_dispatcher(key_pair);
    let outside_execution = setup_outside_execution(account_address, false);

    start_cheat_block_timestamp_global(5);

    dispatcher.execute_from_outside_v2(outside_execution, array![].span());
}

#[test]
#[should_panic(expected: 'SRC9: now <= execute_after')]
fn test_execute_from_outside_v2_call_equal_to_execute_after() {
    let key_pair = KEY_PAIR();
    let (account_address, dispatcher) = setup_dispatcher(key_pair);
    let outside_execution = setup_outside_execution(account_address, false);

    start_cheat_block_timestamp_global(10);

    dispatcher.execute_from_outside_v2(outside_execution, array![].span());
}

#[test]
#[should_panic(expected: 'SRC9: duplicated nonce')]
fn test_execute_from_outside_v2_invalid_nonce() {
    let key_pair = KEY_PAIR();
    let (account_address, dispatcher) = setup_dispatcher(key_pair);
    let simple_mock = setup_simple_mock();
    let outside_execution = setup_outside_execution(simple_mock, false);

    let msg_hash = outside_execution.get_message_hash(account_address);
    let signature = key_pair.serialized_sign(msg_hash);

    dispatcher.execute_from_outside_v2(outside_execution, signature.span());
    dispatcher.execute_from_outside_v2(outside_execution, array![].span());
}

#[test]
#[should_panic(expected: 'SRC9: invalid signature')]
fn test_execute_from_outside_v2_invalid_signature() {
    let key_pair = KEY_PAIR();
    let (account_address, dispatcher) = setup_dispatcher(key_pair);
    let outside_execution = setup_outside_execution(account_address, false);

    let msg_hash = outside_execution.get_message_hash(account_address);
    let signature = key_pair.serialized_sign(msg_hash);
    let invalid_signature = array![*signature.at(0), *signature.at(1) + 1];

    dispatcher.execute_from_outside_v2(outside_execution, invalid_signature.span());
}

#[test]
#[should_panic(expected: "Some error")]
fn test_execute_from_outside_v2_panics_when_inner_call_panic() {
    let key_pair = KEY_PAIR();
    let (account_address, dispatcher) = setup_dispatcher(key_pair);
    let simple_mock = setup_simple_mock();
    let outside_execution = setup_outside_execution(simple_mock, true);

    let msg_hash = outside_execution.get_message_hash(account_address);
    let signature = key_pair.serialized_sign(msg_hash);

    dispatcher.execute_from_outside_v2(outside_execution, signature.span());
}

//
// Helpers
//

fn setup_outside_execution(target: ContractAddress, panic: bool) -> OutsideExecution {
    let call = Call {
        to: target,
        selector: selector!("set_balance"),
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
    let value = *load(target, selector!("balance"), 1).at(0);
    assert_eq!(value, expected_value);
}
