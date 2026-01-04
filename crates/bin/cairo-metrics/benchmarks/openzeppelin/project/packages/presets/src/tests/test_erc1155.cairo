use core::num::traits::Zero;
use openzeppelin_test_common::erc1155::{
    ERC1155SpyHelpers, deploy_another_account_at, get_ids_and_split_values, get_ids_and_values,
    setup_account, setup_receiver, setup_src5,
};
use openzeppelin_test_common::ownable::OwnableSpyHelpers;
use openzeppelin_test_common::upgrades::UpgradeableSpyHelpers;
use openzeppelin_testing as utils;
use openzeppelin_testing::constants::{
    CLASS_HASH_ZERO, EMPTY_DATA, OPERATOR, OTHER, OWNER, RECIPIENT, TOKEN_ID, TOKEN_ID_2,
    TOKEN_VALUE, TOKEN_VALUE_2, ZERO,
};
use openzeppelin_testing::{EventSpyExt, EventSpyQueue as EventSpy, spy_events};
use openzeppelin_token::erc1155;
use openzeppelin_token::erc1155::interface::{
    IERC1155CamelSafeDispatcher, IERC1155CamelSafeDispatcherTrait, IERC1155Dispatcher,
    IERC1155DispatcherTrait,
};
use openzeppelin_utils::serde::SerializedAppend;
use snforge_std::start_cheat_caller_address;
use starknet::{ClassHash, ContractAddress};
use crate::interfaces::{ERC1155UpgradeableABIDispatcher, ERC1155UpgradeableABIDispatcherTrait};

fn V2_CLASS_HASH() -> ClassHash {
    utils::declare_class("SnakeERC1155Mock").class_hash
}

//
// Setup
//

fn setup_dispatcher_with_event() -> (EventSpy, ERC1155UpgradeableABIDispatcher, ContractAddress) {
    let uri: ByteArray = "URI";
    let mut calldata = array![];
    let mut token_ids = array![TOKEN_ID, TOKEN_ID_2];
    let mut values = array![TOKEN_VALUE, TOKEN_VALUE_2];

    let owner = setup_account();

    calldata.append_serde(uri);
    calldata.append_serde(owner);
    calldata.append_serde(token_ids);
    calldata.append_serde(values);
    calldata.append_serde(owner);

    let spy = spy_events();
    let address = utils::declare_and_deploy("ERC1155Upgradeable", calldata);
    start_cheat_caller_address(address, owner);
    (spy, ERC1155UpgradeableABIDispatcher { contract_address: address }, owner)
}

fn setup_dispatcher() -> (EventSpy, ERC1155UpgradeableABIDispatcher, ContractAddress) {
    let (mut spy, dispatcher, owner) = setup_dispatcher_with_event();
    spy.drop_all_events();
    (spy, dispatcher, owner)
}

//
// constructor
//

#[test]
fn test_constructor() {
    let (_, dispatcher, owner) = setup_dispatcher_with_event();

    assert_eq!(dispatcher.uri(TOKEN_ID), "URI");
    assert_eq!(dispatcher.balance_of(owner, TOKEN_ID), TOKEN_VALUE);
    assert_eq!(dispatcher.balance_of(owner, TOKEN_ID_2), TOKEN_VALUE_2);

    let supports_ierc1155 = dispatcher.supports_interface(erc1155::interface::IERC1155_ID);
    assert!(supports_ierc1155);

    let supports_ierc1155_metadata_uri = dispatcher
        .supports_interface(erc1155::interface::IERC1155_METADATA_URI_ID);
    assert!(supports_ierc1155_metadata_uri);

    let supports_isrc5 = dispatcher
        .supports_interface(openzeppelin_introspection::interface::ISRC5_ID);
    assert!(supports_isrc5);
}

//
// balance_of & balanceOf
//

#[test]
fn test_balance_of() {
    let (_, dispatcher, owner) = setup_dispatcher();

    let balance = dispatcher.balance_of(owner, TOKEN_ID);
    assert_eq!(balance, TOKEN_VALUE);
}

#[test]
fn test_balanceOf() {
    let (_, dispatcher, owner) = setup_dispatcher();

    let balance = dispatcher.balanceOf(owner, TOKEN_ID);
    assert_eq!(balance, TOKEN_VALUE);
}

//
// balance_of_batch & balanceOfBatch
//

#[test]
fn test_balance_of_batch() {
    let (_, dispatcher, owner) = setup_dispatcher();

    let accounts = array![owner, OTHER].span();
    let token_ids = array![TOKEN_ID, TOKEN_ID].span();

    let balances = dispatcher.balance_of_batch(accounts, token_ids);
    assert_eq!(*balances.at(0), TOKEN_VALUE);
    assert!((*balances.at(1)).is_zero());
}

#[test]
fn test_balanceOfBatch() {
    let (_, dispatcher, owner) = setup_dispatcher();

    let accounts = array![owner, OTHER].span();
    let token_ids = array![TOKEN_ID, TOKEN_ID].span();

    let balances = dispatcher.balanceOfBatch(accounts, token_ids);
    assert_eq!(*balances.at(0), TOKEN_VALUE);
    assert!((*balances.at(1)).is_zero());
}

#[test]
#[should_panic(expected: 'ERC1155: no equal array length')]
fn test_balance_of_batch_invalid_inputs() {
    let (_, dispatcher, owner) = setup_dispatcher();

    let accounts = array![owner, OTHER].span();
    let token_ids = array![TOKEN_ID].span();

    dispatcher.balance_of_batch(accounts, token_ids);
}

#[test]
#[should_panic(expected: 'ERC1155: no equal array length')]
fn test_balanceOfBatch_invalid_inputs() {
    let (_, dispatcher, owner) = setup_dispatcher();

    let accounts = array![owner, OTHER].span();
    let token_ids = array![TOKEN_ID].span();

    dispatcher.balanceOfBatch(accounts, token_ids);
}

//
// safe_transfer_from & safeTransferFrom
//

#[test]
fn test_safe_transfer_from_to_receiver() {
    let (mut spy, dispatcher, owner) = setup_dispatcher();
    let contract = dispatcher.contract_address;
    let recipient = setup_receiver();

    assert_state_before_transfer_single(dispatcher, owner, recipient, TOKEN_ID);

    dispatcher.safe_transfer_from(owner, recipient, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
    spy.assert_only_event_transfer_single(contract, owner, owner, recipient, TOKEN_ID, TOKEN_VALUE);

    assert_state_after_transfer_single(dispatcher, owner, recipient, TOKEN_ID);
}

#[test]
fn test_safeTransferFrom_to_receiver() {
    let (mut spy, dispatcher, owner) = setup_dispatcher();
    let contract = dispatcher.contract_address;
    let recipient = setup_receiver();

    assert_state_before_transfer_single(dispatcher, owner, recipient, TOKEN_ID);

    dispatcher.safeTransferFrom(owner, recipient, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
    spy.assert_only_event_transfer_single(contract, owner, owner, recipient, TOKEN_ID, TOKEN_VALUE);

    assert_state_after_transfer_single(dispatcher, owner, recipient, TOKEN_ID);
}

#[test]
fn test_safe_transfer_from_to_account() {
    let (mut spy, dispatcher, owner) = setup_dispatcher();
    let contract = dispatcher.contract_address;
    let recipient = RECIPIENT;
    deploy_another_account_at(owner, recipient);
    spy.drop_all_events();

    assert_state_before_transfer_single(dispatcher, owner, recipient, TOKEN_ID);

    dispatcher.safe_transfer_from(owner, recipient, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
    spy.assert_only_event_transfer_single(contract, owner, owner, recipient, TOKEN_ID, TOKEN_VALUE);

    assert_state_after_transfer_single(dispatcher, owner, recipient, TOKEN_ID);
}

#[test]
fn test_safeTransferFrom_to_account() {
    let (mut spy, dispatcher, owner) = setup_dispatcher();
    let contract = dispatcher.contract_address;
    let recipient = RECIPIENT;
    deploy_another_account_at(owner, recipient);
    spy.drop_all_events();

    assert_state_before_transfer_single(dispatcher, owner, recipient, TOKEN_ID);

    dispatcher.safeTransferFrom(owner, recipient, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
    spy.assert_only_event_transfer_single(contract, owner, owner, recipient, TOKEN_ID, TOKEN_VALUE);

    assert_state_after_transfer_single(dispatcher, owner, recipient, TOKEN_ID);
}

#[test]
fn test_safe_transfer_from_approved_operator() {
    let (mut spy, dispatcher, owner) = setup_dispatcher();
    let contract = dispatcher.contract_address;
    let operator = OPERATOR;
    let recipient = RECIPIENT;
    deploy_another_account_at(owner, recipient);
    spy.drop_all_events();

    dispatcher.set_approval_for_all(operator, true);
    spy.assert_only_event_approval_for_all(contract, owner, operator, true);

    assert_state_before_transfer_single(dispatcher, owner, recipient, TOKEN_ID);

    start_cheat_caller_address(dispatcher.contract_address, operator);
    dispatcher.safe_transfer_from(owner, recipient, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
    spy
        .assert_only_event_transfer_single(
            contract, operator, owner, recipient, TOKEN_ID, TOKEN_VALUE,
        );

    assert_state_after_transfer_single(dispatcher, owner, recipient, TOKEN_ID);
}

#[test]
fn test_safeTransferFrom_approved_operator() {
    let (mut spy, dispatcher, owner) = setup_dispatcher();
    let contract = dispatcher.contract_address;
    let operator = OPERATOR;
    let recipient = RECIPIENT;
    deploy_another_account_at(owner, recipient);
    spy.drop_all_events();

    dispatcher.set_approval_for_all(operator, true);
    spy.assert_only_event_approval_for_all(contract, owner, operator, true);

    assert_state_before_transfer_single(dispatcher, owner, recipient, TOKEN_ID);

    start_cheat_caller_address(dispatcher.contract_address, operator);
    dispatcher.safeTransferFrom(owner, recipient, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
    spy
        .assert_only_event_transfer_single(
            contract, operator, owner, recipient, TOKEN_ID, TOKEN_VALUE,
        );

    assert_state_after_transfer_single(dispatcher, owner, recipient, TOKEN_ID);
}

#[test]
#[should_panic(expected: 'ERC1155: invalid sender')]
fn test_safe_transfer_from_from_zero() {
    let (_, dispatcher, owner) = setup_dispatcher();

    dispatcher.safe_transfer_from(ZERO, owner, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: invalid sender')]
fn test_safeTransferFrom_from_zero() {
    let (_, dispatcher, owner) = setup_dispatcher();

    dispatcher.safeTransferFrom(ZERO, owner, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: invalid receiver')]
fn test_safe_transfer_from_to_zero() {
    let (_, dispatcher, owner) = setup_dispatcher();

    dispatcher.safe_transfer_from(owner, ZERO, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: invalid receiver')]
fn test_safeTransferFrom_to_zero() {
    let (_, dispatcher, owner) = setup_dispatcher();

    dispatcher.safeTransferFrom(owner, ZERO, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: unauthorized operator')]
fn test_safe_transfer_from_unauthorized() {
    let (_, dispatcher, owner) = setup_dispatcher();

    dispatcher.safe_transfer_from(OTHER, owner, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: unauthorized operator')]
fn test_safeTransferFrom_unauthorized() {
    let (_, dispatcher, owner) = setup_dispatcher();

    dispatcher.safeTransferFrom(OTHER, owner, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: insufficient balance')]
fn test_safe_transfer_from_insufficient_balance() {
    let (_, dispatcher, owner) = setup_dispatcher();

    dispatcher.safe_transfer_from(owner, OTHER, TOKEN_ID, TOKEN_VALUE + 1, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: insufficient balance')]
fn test_safeTransferFrom_insufficient_balance() {
    let (_, dispatcher, owner) = setup_dispatcher();

    dispatcher.safeTransferFrom(owner, OTHER, TOKEN_ID, TOKEN_VALUE + 1, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: safe transfer failed')]
fn test_safe_transfer_from_non_account_non_receiver() {
    let (_, dispatcher, owner) = setup_dispatcher();
    let non_receiver = setup_src5();

    dispatcher.safe_transfer_from(owner, non_receiver, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: safe transfer failed')]
fn test_safeTransferFrom_non_account_non_receiver() {
    let (_, dispatcher, owner) = setup_dispatcher();
    let non_receiver = setup_src5();

    dispatcher.safeTransferFrom(owner, non_receiver, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
}

//
// safe_batch_transfer_from & safeBatchTransferFrom
//

#[test]
fn test_safe_batch_transfer_from_to_receiver() {
    let (mut spy, dispatcher, owner) = setup_dispatcher();
    let contract = dispatcher.contract_address;
    let recipient = setup_receiver();
    let (token_ids, values) = get_ids_and_values();

    assert_state_before_transfer_batch(dispatcher, owner, recipient, token_ids, values);

    dispatcher.safe_batch_transfer_from(owner, recipient, token_ids, values, EMPTY_DATA());
    spy.assert_only_event_transfer_batch(contract, owner, owner, recipient, token_ids, values);

    assert_state_after_transfer_batch(dispatcher, owner, recipient, token_ids, values);
}

#[test]
fn test_safeBatchTransferFrom_to_receiver() {
    let (mut spy, dispatcher, owner) = setup_dispatcher();
    let contract = dispatcher.contract_address;
    let recipient = setup_receiver();
    let (token_ids, values) = get_ids_and_values();

    assert_state_before_transfer_batch(dispatcher, owner, recipient, token_ids, values);

    dispatcher.safeBatchTransferFrom(owner, recipient, token_ids, values, EMPTY_DATA());
    spy.assert_only_event_transfer_batch(contract, owner, owner, recipient, token_ids, values);

    assert_state_after_transfer_batch(dispatcher, owner, recipient, token_ids, values);
}

#[test]
fn test_safe_batch_transfer_from_to_account() {
    let (mut spy, dispatcher, owner) = setup_dispatcher();
    let contract = dispatcher.contract_address;
    let (token_ids, values) = get_ids_and_values();
    let recipient = RECIPIENT;
    deploy_another_account_at(owner, recipient);
    spy.drop_all_events();

    assert_state_before_transfer_batch(dispatcher, owner, recipient, token_ids, values);

    dispatcher.safe_batch_transfer_from(owner, recipient, token_ids, values, EMPTY_DATA());
    spy.assert_only_event_transfer_batch(contract, owner, owner, recipient, token_ids, values);

    assert_state_after_transfer_batch(dispatcher, owner, recipient, token_ids, values);
}

#[test]
fn test_safeBatchTransferFrom_to_account() {
    let (mut spy, dispatcher, owner) = setup_dispatcher();
    let contract = dispatcher.contract_address;
    let (token_ids, values) = get_ids_and_values();
    let recipient = RECIPIENT;
    deploy_another_account_at(owner, recipient);
    spy.drop_all_events();

    assert_state_before_transfer_batch(dispatcher, owner, recipient, token_ids, values);

    dispatcher.safeBatchTransferFrom(owner, recipient, token_ids, values, EMPTY_DATA());
    spy.assert_only_event_transfer_batch(contract, owner, owner, recipient, token_ids, values);

    assert_state_after_transfer_batch(dispatcher, owner, recipient, token_ids, values);
}


#[test]
fn test_safe_batch_transfer_from_approved_operator() {
    let (mut spy, dispatcher, owner) = setup_dispatcher();
    let contract = dispatcher.contract_address;
    let (token_ids, values) = get_ids_and_values();
    let operator = OPERATOR;
    let recipient = RECIPIENT;
    deploy_another_account_at(owner, recipient);
    spy.drop_all_events();

    dispatcher.set_approval_for_all(operator, true);
    spy.assert_only_event_approval_for_all(contract, owner, operator, true);

    assert_state_before_transfer_batch(dispatcher, owner, recipient, token_ids, values);

    start_cheat_caller_address(dispatcher.contract_address, operator);
    dispatcher.safe_batch_transfer_from(owner, recipient, token_ids, values, EMPTY_DATA());
    spy.assert_only_event_transfer_batch(contract, operator, owner, recipient, token_ids, values);

    assert_state_after_transfer_batch(dispatcher, owner, recipient, token_ids, values);
}

#[test]
fn test_safeBatchTransferFrom_approved_operator() {
    let (mut spy, dispatcher, owner) = setup_dispatcher();
    let contract = dispatcher.contract_address;
    let (token_ids, values) = get_ids_and_values();
    let operator = OPERATOR;
    let recipient = RECIPIENT;
    deploy_another_account_at(owner, recipient);
    spy.drop_all_events();

    dispatcher.set_approval_for_all(operator, true);
    spy.assert_only_event_approval_for_all(contract, owner, operator, true);

    assert_state_before_transfer_batch(dispatcher, owner, recipient, token_ids, values);
    start_cheat_caller_address(dispatcher.contract_address, operator);
    dispatcher.safeBatchTransferFrom(owner, recipient, token_ids, values, EMPTY_DATA());
    spy.assert_only_event_transfer_batch(contract, operator, owner, recipient, token_ids, values);

    assert_state_after_transfer_batch(dispatcher, owner, recipient, token_ids, values);
}

#[test]
#[should_panic(expected: 'ERC1155: invalid sender')]
fn test_safe_batch_transfer_from_from_zero() {
    let (_, dispatcher, owner) = setup_dispatcher();
    let (token_ids, values) = get_ids_and_values();

    dispatcher.safe_batch_transfer_from(ZERO, owner, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: invalid sender')]
fn test_safeBatchTransferFrom_from_zero() {
    let (_, dispatcher, owner) = setup_dispatcher();
    let (token_ids, values) = get_ids_and_values();

    dispatcher.safeBatchTransferFrom(ZERO, owner, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: invalid receiver')]
fn test_safe_batch_transfer_from_to_zero() {
    let (_, dispatcher, owner) = setup_dispatcher();
    let (token_ids, values) = get_ids_and_values();

    dispatcher.safe_batch_transfer_from(owner, ZERO, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: invalid receiver')]
fn test_safeBatchTransferFrom_to_zero() {
    let (_, dispatcher, owner) = setup_dispatcher();
    let (token_ids, values) = get_ids_and_values();

    dispatcher.safeBatchTransferFrom(owner, ZERO, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: unauthorized operator')]
fn test_safe_batch_transfer_from_unauthorized() {
    let (_, dispatcher, owner) = setup_dispatcher();
    let (token_ids, values) = get_ids_and_values();

    dispatcher.safe_batch_transfer_from(OTHER, owner, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: unauthorized operator')]
fn test_safeBatchTransferFrom_unauthorized() {
    let (_, dispatcher, owner) = setup_dispatcher();
    let (token_ids, values) = get_ids_and_values();

    dispatcher.safeBatchTransferFrom(OTHER, owner, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: insufficient balance')]
fn test_safe_batch_transfer_from_insufficient_balance() {
    let (_, dispatcher, owner) = setup_dispatcher();
    let token_ids = array![TOKEN_ID, TOKEN_ID_2].span();
    let values = array![TOKEN_VALUE + 1, TOKEN_VALUE_2].span();

    dispatcher.safe_batch_transfer_from(owner, OTHER, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: insufficient balance')]
fn test_safeBatchTransferFrom_insufficient_balance() {
    let (_, dispatcher, owner) = setup_dispatcher();
    let token_ids = array![TOKEN_ID, TOKEN_ID_2].span();
    let values = array![TOKEN_VALUE + 1, TOKEN_VALUE_2].span();

    dispatcher.safeBatchTransferFrom(owner, OTHER, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: safe transfer failed')]
fn test_safe_batch_transfer_from_non_account_non_receiver() {
    let (_, dispatcher, owner) = setup_dispatcher();
    let (token_ids, values) = get_ids_and_split_values(5);
    let non_receiver = setup_src5();

    dispatcher.safe_batch_transfer_from(owner, non_receiver, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: safe transfer failed')]
fn test_safeBatchTransferFrom_non_account_non_receiver() {
    let (_, dispatcher, owner) = setup_dispatcher();
    let (token_ids, values) = get_ids_and_split_values(5);
    let non_receiver = setup_src5();

    dispatcher.safeBatchTransferFrom(owner, non_receiver, token_ids, values, EMPTY_DATA());
}

//
// set_approval_for_all & is_approved_for_all
//

#[test]
fn test_set_approval_for_all_and_is_approved_for_all() {
    let (mut spy, dispatcher, _) = setup_dispatcher();
    let contract = dispatcher.contract_address;
    start_cheat_caller_address(dispatcher.contract_address, OWNER);

    let not_approved_for_all = !dispatcher.is_approved_for_all(OWNER, OPERATOR);
    assert!(not_approved_for_all);

    dispatcher.set_approval_for_all(OPERATOR, true);
    spy.assert_only_event_approval_for_all(contract, OWNER, OPERATOR, true);

    let is_approved_for_all = dispatcher.is_approved_for_all(OWNER, OPERATOR);
    assert!(is_approved_for_all);

    dispatcher.set_approval_for_all(OPERATOR, false);
    spy.assert_only_event_approval_for_all(contract, OWNER, OPERATOR, false);

    let not_approved_for_all = !dispatcher.is_approved_for_all(OWNER, OPERATOR);
    assert!(not_approved_for_all);
}

#[test]
#[should_panic(expected: 'ERC1155: self approval')]
fn test_set_approval_for_all_owner_equal_operator_true() {
    let (_, dispatcher, _) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.set_approval_for_all(OWNER, true);
}

#[test]
#[should_panic(expected: 'ERC1155: self approval')]
fn test_set_approval_for_all_owner_equal_operator_false() {
    let (_, dispatcher, _) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.set_approval_for_all(OWNER, false);
}

//
// setApprovalForAll & isApprovedForAll
//

#[test]
fn test_setApprovalForAll_and_isApprovedForAll() {
    let (mut spy, dispatcher, _) = setup_dispatcher();
    let contract = dispatcher.contract_address;
    start_cheat_caller_address(dispatcher.contract_address, OWNER);

    let not_approved_for_all = !dispatcher.isApprovedForAll(OWNER, OPERATOR);
    assert!(not_approved_for_all);

    dispatcher.setApprovalForAll(OPERATOR, true);
    spy.assert_only_event_approval_for_all(contract, OWNER, OPERATOR, true);

    let is_approved_for_all = dispatcher.isApprovedForAll(OWNER, OPERATOR);
    assert!(is_approved_for_all);

    dispatcher.setApprovalForAll(OPERATOR, false);
    spy.assert_only_event_approval_for_all(contract, OWNER, OPERATOR, false);

    let not_approved_for_all = !dispatcher.isApprovedForAll(OWNER, OPERATOR);
    assert!(not_approved_for_all);
}

#[test]
#[should_panic(expected: 'ERC1155: self approval')]
fn test_setApprovalForAll_owner_equal_operator_true() {
    let (_, dispatcher, _) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.set_approval_for_all(OWNER, true);
}

#[test]
#[should_panic(expected: 'ERC1155: self approval')]
fn test_setApprovalForAll_owner_equal_operator_false() {
    let (_, dispatcher, _) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.setApprovalForAll(OWNER, false);
}

//
// transfer_ownership & transferOwnership
//

#[test]
fn test_transfer_ownership() {
    let (mut spy, dispatcher, owner) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, owner);
    dispatcher.transfer_ownership(OTHER);

    spy.assert_event_ownership_transferred(dispatcher.contract_address, owner, OTHER);
    assert_eq!(dispatcher.owner(), OTHER);
}

#[test]
#[should_panic(expected: 'New owner is the zero address')]
fn test_transfer_ownership_to_zero() {
    let (_, dispatcher, owner) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, owner);
    dispatcher.transfer_ownership(ZERO);
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_transfer_ownership_from_nonowner() {
    let (_, dispatcher, _) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OTHER);
    dispatcher.transfer_ownership(OTHER);
}

#[test]
fn test_transferOwnership() {
    let (mut spy, dispatcher, owner) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, owner);
    dispatcher.transferOwnership(OTHER);

    spy.assert_event_ownership_transferred(dispatcher.contract_address, owner, OTHER);
    assert_eq!(dispatcher.owner(), OTHER);
}

#[test]
#[should_panic(expected: 'New owner is the zero address')]
fn test_transferOwnership_to_zero() {
    let (_, dispatcher, owner) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, owner);
    dispatcher.transferOwnership(ZERO);
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_transferOwnership_from_nonowner() {
    let (_, dispatcher, _) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OTHER);
    dispatcher.transferOwnership(OTHER);
}

//
// renounce_ownership & renounceOwnership
//

#[test]
fn test_renounce_ownership() {
    let (mut spy, dispatcher, owner) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, owner);
    dispatcher.renounce_ownership();

    spy.assert_event_ownership_transferred(dispatcher.contract_address, owner, ZERO);
    assert!(dispatcher.owner().is_zero());
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_renounce_ownership_from_nonowner() {
    let (_, dispatcher, _) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OTHER);
    dispatcher.renounce_ownership();
}

#[test]
fn test_renounceOwnership() {
    let (mut spy, dispatcher, owner) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, owner);
    dispatcher.renounceOwnership();

    spy.assert_event_ownership_transferred(dispatcher.contract_address, owner, ZERO);
    assert!(dispatcher.owner().is_zero());
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_renounceOwnership_from_nonowner() {
    let (_, dispatcher, _) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OTHER);
    dispatcher.renounceOwnership();
}

//
// upgrade
//

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_upgrade_unauthorized() {
    let (_, v1, _) = setup_dispatcher();
    start_cheat_caller_address(v1.contract_address, OTHER);
    v1.upgrade(CLASS_HASH_ZERO);
}

#[test]
#[should_panic(expected: 'Class hash cannot be zero')]
fn test_upgrade_with_class_hash_zero() {
    let (_, v1, owner) = setup_dispatcher();

    start_cheat_caller_address(v1.contract_address, owner);
    v1.upgrade(CLASS_HASH_ZERO);
}

#[test]
fn test_upgraded_event() {
    let (mut spy, v1, owner) = setup_dispatcher();
    let v2_class_hash = V2_CLASS_HASH();

    start_cheat_caller_address(v1.contract_address, owner);
    v1.upgrade(v2_class_hash);

    spy.assert_only_event_upgraded(v1.contract_address, v2_class_hash);
}

#[test]
#[feature("safe_dispatcher")]
fn test_v2_missing_camel_selector() {
    let (_, v1, owner) = setup_dispatcher();
    let v2_class_hash = V2_CLASS_HASH();

    start_cheat_caller_address(v1.contract_address, owner);
    v1.upgrade(v2_class_hash);

    let safe_dispatcher = IERC1155CamelSafeDispatcher { contract_address: v1.contract_address };
    let result = safe_dispatcher.balanceOf(owner, TOKEN_ID);

    utils::assert_entrypoint_not_found_error(result, selector!("balanceOf"), v1.contract_address)
}

#[test]
fn test_state_persists_after_upgrade() {
    let (_, v1, owner) = setup_dispatcher();
    let recipient = setup_receiver();
    let v2_class_hash = V2_CLASS_HASH();

    start_cheat_caller_address(v1.contract_address, owner);
    v1.safeTransferFrom(owner, recipient, TOKEN_ID, TOKEN_VALUE, array![].span());

    // Check RECIPIENT balance v1
    let camel_balance = v1.balanceOf(recipient, TOKEN_ID);
    assert_eq!(camel_balance, TOKEN_VALUE);

    v1.upgrade(v2_class_hash);

    // Check RECIPIENT balance v2
    let v2 = IERC1155Dispatcher { contract_address: v1.contract_address };
    let snake_balance = v2.balance_of(recipient, TOKEN_ID);
    assert_eq!(snake_balance, camel_balance);
}

//
// Helpers
//

fn assert_state_before_transfer_single(
    dispatcher: ERC1155UpgradeableABIDispatcher,
    sender: ContractAddress,
    recipient: ContractAddress,
    token_id: u256,
) {
    assert_eq!(dispatcher.balance_of(sender, token_id), TOKEN_VALUE);
    assert!(dispatcher.balance_of(recipient, token_id).is_zero());
}

fn assert_state_after_transfer_single(
    dispatcher: ERC1155UpgradeableABIDispatcher,
    sender: ContractAddress,
    recipient: ContractAddress,
    token_id: u256,
) {
    assert!(dispatcher.balance_of(sender, token_id).is_zero());
    assert_eq!(dispatcher.balance_of(recipient, token_id), TOKEN_VALUE);
}

fn assert_state_before_transfer_batch(
    dispatcher: ERC1155UpgradeableABIDispatcher,
    sender: ContractAddress,
    recipient: ContractAddress,
    token_ids: Span<u256>,
    values: Span<u256>,
) {
    let mut index = 0;
    while index != token_ids.len() {
        let balance_of_sender = dispatcher.balance_of(sender, *token_ids.at(index));
        assert_eq!(balance_of_sender, *values.at(index));
        let balance_of_recipient = dispatcher.balance_of(recipient, *token_ids.at(index));
        assert!(balance_of_recipient.is_zero());

        index += 1;
    }
}

fn assert_state_after_transfer_batch(
    dispatcher: ERC1155UpgradeableABIDispatcher,
    sender: ContractAddress,
    recipient: ContractAddress,
    token_ids: Span<u256>,
    values: Span<u256>,
) {
    let mut index = 0;
    while index != token_ids.len() {
        let balance_of_sender = dispatcher.balance_of(sender, *token_ids.at(index));
        assert!(balance_of_sender.is_zero());
        let balance_of_recipient = dispatcher.balance_of(recipient, *token_ids.at(index));
        assert_eq!(balance_of_recipient, *values.at(index));

        index += 1;
    }
}
