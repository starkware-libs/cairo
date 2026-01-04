use core::num::traits::Zero;
use openzeppelin_account::eth_account::EthAccountComponent::EthAccountMixinImpl;
use openzeppelin_account::extensions::SRC9Component::{OutsideExecutionV2Impl, SNIP12MetadataImpl};
use openzeppelin_account::extensions::src9::interface::{ISRC9_V2_ID, OutsideExecution};
use openzeppelin_account::extensions::src9::snip12_utils::OutsideExecutionStructHash;
use openzeppelin_account::interface::ISRC6_ID;
use openzeppelin_account::utils::secp256_point::{DebugSecp256Point, Secp256PointPartialEq};
use openzeppelin_introspection::interface::ISRC5_ID;
use openzeppelin_test_common::erc20::deploy_erc20;
use openzeppelin_test_common::eth_account::{
    EthAccountSpyHelpers, SIGNED_TX_DATA, SignedTransactionData, get_accept_ownership_signature,
};
use openzeppelin_test_common::upgrades::UpgradeableSpyHelpers;
use openzeppelin_testing as utils;
use openzeppelin_testing::constants::secp256k1::{KEY_PAIR, KEY_PAIR_2};
use openzeppelin_testing::constants::{
    CALLER, CLASS_HASH_ZERO, FELT_VALUE, MIN_TRANSACTION_VERSION, OTHER, QUERY_VERSION, RECIPIENT,
    SALT, ZERO,
};
use openzeppelin_testing::signing::{Secp256k1KeyPair, SerializedSigning};
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
use starknet::secp256_trait::Secp256Trait;
use starknet::secp256k1::Secp256k1Point;
use starknet::{ClassHash, ContractAddress, SyscallResultTrait};
use crate::EthAccountUpgradeable;
use crate::interfaces::eth_account::{
    EthAccountUpgradeableABISafeDispatcher, EthAccountUpgradeableABISafeDispatcherTrait,
};
use crate::interfaces::{
    EthAccountUpgradeableABIDispatcher, EthAccountUpgradeableABIDispatcherTrait,
};

fn declare_v2_class() -> ClassHash {
    utils::declare_class("SnakeEthAccountMock").class_hash
}

//
// Setup
//

fn setup_dispatcher(
    key_pair: Secp256k1KeyPair,
) -> (ContractAddress, EthAccountUpgradeableABIDispatcher) {
    let mut calldata = array![];
    calldata.append_serde(key_pair.public_key);

    let contract_address = utils::declare_and_deploy("EthAccountUpgradeable", calldata);
    let dispatcher = EthAccountUpgradeableABIDispatcher { contract_address };

    (contract_address, dispatcher)
}

fn setup_dispatcher_with_data(
    key_pair: Secp256k1KeyPair, data: SignedTransactionData,
) -> (EthAccountUpgradeableABIDispatcher, felt252) {
    let mut calldata = array![];
    calldata.append_serde(key_pair.public_key);
    let contract_class = utils::declare_class("EthAccountUpgradeable");
    let contract_address = utils::deploy(contract_class, calldata);
    let dispatcher = EthAccountUpgradeableABIDispatcher { contract_address };

    let mut serialized_signature = array![];
    data.signature.serialize(ref serialized_signature);
    start_cheat_signature_global(serialized_signature.span());
    start_cheat_transaction_hash_global(data.tx_hash);
    start_cheat_transaction_version_global(MIN_TRANSACTION_VERSION);
    start_cheat_caller_address(contract_address, ZERO);

    (dispatcher, contract_class.class_hash.into())
}

fn setup_simple_mock() -> ContractAddress {
    utils::declare_and_deploy("SimpleMock", array![])
}

//
// constructor
//

#[test]
fn test_constructor() {
    let mut state = EthAccountUpgradeable::contract_state_for_testing();
    let mut spy = spy_events();
    let key_pair = KEY_PAIR();

    EthAccountUpgradeable::constructor(ref state, key_pair.public_key);

    spy.assert_only_event_owner_added(test_address(), key_pair.public_key);

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
    let (_, dispatcher) = setup_dispatcher(key_pair);
    let contract_address = dispatcher.contract_address;
    let mut spy = spy_events();

    let new_key_pair = KEY_PAIR_2();
    let signature = get_accept_ownership_signature(
        contract_address, key_pair.public_key, new_key_pair,
    );
    start_cheat_caller_address(contract_address, contract_address);
    dispatcher.set_public_key(new_key_pair.public_key, signature);

    assert_eq!(dispatcher.get_public_key(), new_key_pair.public_key);

    spy.assert_event_owner_removed(contract_address, key_pair.public_key);
    spy.assert_only_event_owner_added(contract_address, new_key_pair.public_key);
}

#[test]
fn test_public_key_setter_and_getter_camel() {
    let key_pair = KEY_PAIR();
    let (_, dispatcher) = setup_dispatcher(key_pair);
    let contract_address = dispatcher.contract_address;
    let mut spy = spy_events();

    let new_key_pair = KEY_PAIR_2();
    let signature = get_accept_ownership_signature(
        contract_address, key_pair.public_key, new_key_pair,
    );
    start_cheat_caller_address(contract_address, contract_address);
    dispatcher.setPublicKey(new_key_pair.public_key, signature);
    assert_eq!(dispatcher.getPublicKey(), new_key_pair.public_key);

    spy.assert_event_owner_removed(contract_address, key_pair.public_key);
    spy.assert_only_event_owner_added(contract_address, new_key_pair.public_key);
}

#[test]
#[should_panic(expected: 'EthAccount: unauthorized')]
fn test_set_public_key_different_account() {
    let (_, dispatcher) = setup_dispatcher(KEY_PAIR());

    dispatcher.set_public_key(KEY_PAIR_2().public_key, array![].span());
}

#[test]
#[should_panic(expected: 'EthAccount: unauthorized')]
fn test_setPublicKey_different_account() {
    let (_, dispatcher) = setup_dispatcher(KEY_PAIR());

    dispatcher.setPublicKey(KEY_PAIR_2().public_key, array![].span());
}

//
// is_valid_signature & isValidSignature
//

fn is_valid_sig_dispatcher() -> (EthAccountUpgradeableABIDispatcher, felt252, Array<felt252>) {
    let key_pair = KEY_PAIR();
    let (_, dispatcher) = setup_dispatcher(key_pair);
    let data = SIGNED_TX_DATA(key_pair);

    let mut serialized_signature = array![];
    data.signature.serialize(ref serialized_signature);

    (dispatcher, data.tx_hash, serialized_signature)
}

#[test]
fn test_is_valid_signature() {
    let (dispatcher, hash, signature) = is_valid_sig_dispatcher();

    let is_valid = dispatcher.is_valid_signature(hash, signature);
    assert_eq!(is_valid, starknet::VALIDATED);
}

#[test]
fn test_is_valid_signature_bad_sig() {
    let (dispatcher, hash, signature) = is_valid_sig_dispatcher();

    let is_valid = dispatcher.is_valid_signature(hash + 1, signature);
    assert!(is_valid.is_zero(), "Should reject invalid signature");
}

#[test]
fn test_isValidSignature() {
    let (dispatcher, hash, signature) = is_valid_sig_dispatcher();

    let is_valid = dispatcher.isValidSignature(hash, signature);
    assert_eq!(is_valid, starknet::VALIDATED);
}

#[test]
fn test_isValidSignature_bad_sig() {
    let (dispatcher, hash, signature) = is_valid_sig_dispatcher();

    let is_valid = dispatcher.isValidSignature(hash + 1, signature);
    assert!(is_valid.is_zero(), "Should reject invalid signature");
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
#[should_panic(expected: 'EthAccount: invalid signature')]
fn test_validate_deploy_invalid_signature_data() {
    let key_pair = KEY_PAIR();
    let mut data = SIGNED_TX_DATA(key_pair);
    data.tx_hash += 1;
    let (account, class_hash) = setup_dispatcher_with_data(key_pair, data);

    account.__validate_deploy__(class_hash, SALT, key_pair.public_key);
}

#[test]
#[should_panic(expected: 'Signature: Invalid format.')]
fn test_validate_deploy_invalid_signature_length() {
    let key_pair = KEY_PAIR();
    let (account, class_hash) = setup_dispatcher_with_data(key_pair, SIGNED_TX_DATA(key_pair));

    let invalid_len_sig = array!['INVALID_LEN'];
    start_cheat_signature_global(invalid_len_sig.span());
    account.__validate_deploy__(class_hash, SALT, key_pair.public_key);
}

#[test]
#[should_panic(expected: 'Signature: Invalid format.')]
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
#[should_panic(expected: 'EthAccount: invalid signature')]
fn test_validate_declare_invalid_signature_data() {
    let key_pair = KEY_PAIR();
    let mut data = SIGNED_TX_DATA(key_pair);
    data.tx_hash += 1;
    let (account, class_hash) = setup_dispatcher_with_data(key_pair, data);

    account.__validate_declare__(class_hash);
}

#[test]
#[should_panic(expected: 'Signature: Invalid format.')]
fn test_validate_declare_invalid_signature_length() {
    let key_pair = KEY_PAIR();
    let (account, class_hash) = setup_dispatcher_with_data(key_pair, SIGNED_TX_DATA(key_pair));
    let invalid_len_sig = array!['INVALID_LEN'];

    start_cheat_signature_global(invalid_len_sig.span());
    account.__validate_declare__(class_hash);
}

#[test]
#[should_panic(expected: 'Signature: Invalid format.')]
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

    let amount: u256 = 200;

    let mut calldata = array![];
    calldata.append_serde(RECIPIENT);
    calldata.append_serde(amount);

    let call = Call {
        to: erc20.contract_address, selector: selector!("transfer"), calldata: calldata.span(),
    };
    let calls = array![call];

    if let Option::Some(version) = version {
        start_cheat_transaction_version_global(version);
    }

    account.__execute__(calls);

    assert_eq!(erc20.balance_of(account.contract_address), 800, "Should have remainder");
    assert_eq!(erc20.balance_of(RECIPIENT), amount, "Should have transferred");
}

#[test]
fn test_execute() {
    test_execute_with_version(Option::None);
}

#[test]
fn test_execute_query_version() {
    test_execute_with_version(Option::Some(QUERY_VERSION));
}

#[test]
#[should_panic(expected: 'EthAccount: invalid tx version')]
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
#[should_panic(expected: 'EthAccount: invalid signature')]
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

    let calls = array![call1, call2];
    account.__execute__(calls);

    assert_eq!(erc20.balance_of(account.contract_address), 200, "Should have remainder");
    assert_eq!(erc20.balance_of(recipient1), 300, "Should have transferred");
    assert_eq!(erc20.balance_of(recipient2), 500, "Should have transferred");
}

#[test]
#[should_panic(expected: 'EthAccount: invalid caller')]
fn test_account_called_from_contract() {
    let (_, account) = setup_dispatcher(KEY_PAIR());
    let calls = array![];

    start_cheat_caller_address(account.contract_address, CALLER);

    account.__execute__(calls);
}

//
// upgrade
//

#[test]
#[should_panic(expected: 'EthAccount: unauthorized')]
fn test_upgrade_access_control() {
    let (_, v1) = setup_dispatcher(KEY_PAIR());
    v1.upgrade(CLASS_HASH_ZERO);
}

#[test]
#[should_panic(expected: 'Class hash cannot be zero')]
fn test_upgrade_with_class_hash_zero() {
    let (_, v1) = setup_dispatcher(KEY_PAIR());

    start_cheat_caller_address(v1.contract_address, v1.contract_address);
    v1.upgrade(CLASS_HASH_ZERO);
}

#[test]
fn test_upgraded_event() {
    let (_, v1) = setup_dispatcher(KEY_PAIR());
    let contract_address = v1.contract_address;
    let mut spy = spy_events();

    start_cheat_caller_address(contract_address, contract_address);
    let v2_class_hash = declare_v2_class();
    v1.upgrade(v2_class_hash);

    spy.assert_only_event_upgraded(contract_address, v2_class_hash);
}

#[test]
#[feature("safe_dispatcher")]
fn test_v2_missing_camel_selector() {
    let (_, v1) = setup_dispatcher(KEY_PAIR());
    let contract_address = v1.contract_address;
    let v2_class_hash = declare_v2_class();

    start_cheat_caller_address(contract_address, contract_address);
    v1.upgrade(v2_class_hash);

    let safe_dispatcher = EthAccountUpgradeableABISafeDispatcher { contract_address };
    let result = safe_dispatcher.getPublicKey();

    utils::assert_entrypoint_not_found_error(result, selector!("getPublicKey"), contract_address)
}

#[test]
fn test_state_persists_after_upgrade() {
    let key_pair = KEY_PAIR();
    let (_, v1) = setup_dispatcher(key_pair);
    let contract_address = v1.contract_address;

    start_cheat_caller_address(contract_address, contract_address);
    let dispatcher = EthAccountUpgradeableABIDispatcher { contract_address };

    let new_key_pair = KEY_PAIR_2();
    let signature = get_accept_ownership_signature(
        contract_address, key_pair.public_key, new_key_pair,
    );
    dispatcher.set_public_key(new_key_pair.public_key, signature);

    let camel_public_key = dispatcher.getPublicKey();
    assert_eq!(camel_public_key, new_key_pair.public_key);

    let v2_class_hash = declare_v2_class();
    v1.upgrade(v2_class_hash);

    let snake_public_key = dispatcher.get_public_key();
    assert_eq!(snake_public_key, new_key_pair.public_key);
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
    let signature = key_pair.serialized_sign(msg_hash.into());

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
    let signature = key_pair.serialized_sign(msg_hash.into());

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
    let signature = key_pair.serialized_sign(msg_hash.into());

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
    let signature = key_pair.serialized_sign(msg_hash.into());

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
    let signature = key_pair.serialized_sign(msg_hash.into());
    let invalid_signature = array![
        *signature.at(0), *signature.at(1), *signature.at(2), *signature.at(3) + 1,
    ];

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
    let signature = key_pair.serialized_sign(msg_hash.into());

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

fn get_points() -> (Secp256k1Point, Secp256k1Point) {
    let curve_size = Secp256Trait::<Secp256k1Point>::get_curve_size();
    let point_1 = Secp256Trait::secp256_ec_get_point_from_x_syscall(curve_size, true)
        .unwrap_syscall()
        .unwrap();
    let point_2 = Secp256Trait::secp256_ec_get_point_from_x_syscall(curve_size, false)
        .unwrap_syscall()
        .unwrap();

    (point_1, point_2)
}
