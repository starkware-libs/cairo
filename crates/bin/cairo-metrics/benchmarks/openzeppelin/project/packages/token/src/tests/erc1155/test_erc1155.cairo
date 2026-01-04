use core::num::traits::Zero;
use openzeppelin_introspection::src5::SRC5Component::SRC5Impl;
use openzeppelin_test_common::erc1155::{
    ERC1155SpyHelpers, deploy_another_account_at, get_ids_and_split_values, get_ids_and_values,
    setup_account, setup_receiver, setup_src5,
};
use openzeppelin_test_common::mocks::erc1155::{DualCaseERC1155Mock, SnakeERC1155MockWithHooks};
use openzeppelin_testing::constants::{
    EMPTY_DATA, OPERATOR, OTHER, OWNER, RECIPIENT, TOKEN_ID, TOKEN_ID_2, TOKEN_VALUE, TOKEN_VALUE_2,
    ZERO,
};
use openzeppelin_testing::{EventSpyExt, EventSpyQueue as EventSpy, spy_events};
use snforge_std::{start_cheat_caller_address, test_address};
use starknet::ContractAddress;
use starknet::storage::StoragePointerReadAccess;
use crate::erc1155;
use crate::erc1155::ERC1155Component;
use crate::erc1155::ERC1155Component::{
    ERC1155CamelImpl, ERC1155Impl, ERC1155MetadataURIImpl, InternalImpl,
};

//
// Setup
//

type ComponentState = ERC1155Component::ComponentState<DualCaseERC1155Mock::ContractState>;
type ComponentStateWithHooks =
    ERC1155Component::ComponentState<SnakeERC1155MockWithHooks::ContractState>;

fn CONTRACT_STATE() -> DualCaseERC1155Mock::ContractState {
    DualCaseERC1155Mock::contract_state_for_testing()
}
fn COMPONENT_STATE() -> ComponentState {
    ERC1155Component::component_state_for_testing()
}
fn COMPONENT_STATE_WITH_HOOKS() -> ComponentStateWithHooks {
    ERC1155Component::component_state_for_testing()
}

fn setup() -> (ComponentState, ContractAddress) {
    let mut state = COMPONENT_STATE();
    state.initializer("URI");

    let owner = setup_account();
    let token_ids = array![TOKEN_ID, TOKEN_ID_2].span();
    let values = array![TOKEN_VALUE, TOKEN_VALUE_2].span();

    state.batch_mint_with_acceptance_check(owner, token_ids, values, array![].span());

    (state, owner)
}

fn setup_with_hooks() -> (ComponentStateWithHooks, ContractAddress) {
    let mut state = COMPONENT_STATE_WITH_HOOKS();
    state.initializer("URI");

    let owner = setup_account();
    let token_ids = array![TOKEN_ID, TOKEN_ID_2].span();
    let values = array![TOKEN_VALUE, TOKEN_VALUE_2].span();

    state.batch_mint_with_acceptance_check(owner, token_ids, values, array![].span());

    (state, owner)
}

//
// Initializers
//

#[test]
fn test_initialize() {
    let mut state = COMPONENT_STATE();
    let mock_state = CONTRACT_STATE();

    state.initializer("URI");

    assert_eq!(state.ERC1155_uri.read(), "URI");
    assert!(state.balance_of(OWNER, TOKEN_ID).is_zero());

    let supports_ierc1155 = mock_state.supports_interface(erc1155::interface::IERC1155_ID);
    assert!(supports_ierc1155);

    let supports_ierc1155_metadata_uri = mock_state
        .supports_interface(erc1155::interface::IERC1155_METADATA_URI_ID);
    assert!(supports_ierc1155_metadata_uri);

    let supports_isrc5 = mock_state
        .supports_interface(openzeppelin_introspection::interface::ISRC5_ID);
    assert!(supports_isrc5);
}

#[test]
fn test_initialize_no_metadata() {
    let mut state = COMPONENT_STATE();
    let mock_state = CONTRACT_STATE();

    state.initializer_no_metadata();

    let empty_str = "";
    assert_eq!(state.ERC1155_uri.read(), empty_str);
    assert!(state.balance_of(OWNER, TOKEN_ID).is_zero());

    let supports_ierc1155 = mock_state.supports_interface(erc1155::interface::IERC1155_ID);
    assert!(supports_ierc1155);

    let does_not_support_ierc1155_metadata_uri = !mock_state
        .supports_interface(erc1155::interface::IERC1155_METADATA_URI_ID);
    assert!(does_not_support_ierc1155_metadata_uri);

    let supports_isrc5 = mock_state
        .supports_interface(openzeppelin_introspection::interface::ISRC5_ID);
    assert!(supports_isrc5);
}

//
// balance_of & balanceOf
//

#[test]
fn test_balance_of() {
    let (state, owner) = setup();
    let balance = state.balance_of(owner, TOKEN_ID);
    assert_eq!(balance, TOKEN_VALUE);
}

#[test]
fn test_balanceOf() {
    let (state, owner) = setup();
    let balance = state.balanceOf(owner, TOKEN_ID);
    assert_eq!(balance, TOKEN_VALUE);
}

//
// balance_of_batch & balanceOfBatch
//

#[test]
fn test_balance_of_batch() {
    let (state, owner) = setup();
    let accounts = array![owner, OTHER].span();
    let token_ids = array![TOKEN_ID, TOKEN_ID].span();

    let balances = state.balance_of_batch(accounts, token_ids);
    assert_eq!(*balances.at(0), TOKEN_VALUE);
    assert!((*balances.at(1)).is_zero());
}

#[test]
fn test_balanceOfBatch() {
    let (state, owner) = setup();
    let accounts = array![owner, OTHER].span();
    let token_ids = array![TOKEN_ID, TOKEN_ID].span();

    let balances = state.balanceOfBatch(accounts, token_ids);
    assert_eq!(*balances.at(0), TOKEN_VALUE);
    assert!((*balances.at(1)).is_zero());
}

#[test]
#[should_panic(expected: 'ERC1155: no equal array length')]
fn test_balance_of_batch_invalid_inputs() {
    let (state, owner) = setup();
    let accounts = array![owner, OTHER].span();
    let token_ids = array![TOKEN_ID].span();

    state.balance_of_batch(accounts, token_ids);
}

#[test]
#[should_panic(expected: 'ERC1155: no equal array length')]
fn test_balanceOfBatch_invalid_inputs() {
    let (state, owner) = setup();
    let accounts = array![owner, OTHER].span();
    let token_ids = array![TOKEN_ID].span();

    state.balanceOfBatch(accounts, token_ids);
}

//
// safe_transfer_from & safeTransferFrom
//

#[test]
fn test_safe_transfer_from_owner_to_receiver() {
    let (mut state, owner) = setup();
    let recipient = setup_receiver();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);

    assert_state_before_transfer_single(owner, recipient, TOKEN_ID);
    state.safe_transfer_from(owner, recipient, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
    spy
        .assert_only_event_transfer_single(
            contract_address, owner, owner, recipient, TOKEN_ID, TOKEN_VALUE,
        );

    assert_state_after_transfer_single(owner, recipient, TOKEN_ID);
}

#[test]
fn test_safeTransferFrom_owner_to_receiver() {
    let (mut state, owner) = setup();
    let recipient = setup_receiver();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);

    assert_state_before_transfer_single(owner, recipient, TOKEN_ID);
    state.safeTransferFrom(owner, recipient, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
    spy
        .assert_only_event_transfer_single(
            contract_address, owner, owner, recipient, TOKEN_ID, TOKEN_VALUE,
        );

    assert_state_after_transfer_single(owner, recipient, TOKEN_ID);
}

#[test]
fn test_safe_transfer_from_owner_to_account() {
    let (mut state, owner) = setup();
    let recipient = RECIPIENT;
    deploy_another_account_at(owner, recipient);
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);

    assert_state_before_transfer_single(owner, recipient, TOKEN_ID);
    state.safe_transfer_from(owner, recipient, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
    spy
        .assert_only_event_transfer_single(
            contract_address, owner, owner, recipient, TOKEN_ID, TOKEN_VALUE,
        );

    assert_state_after_transfer_single(owner, recipient, TOKEN_ID);
}

#[test]
fn test_safeTransferFrom_owner_to_account() {
    let (mut state, owner) = setup();
    let recipient = RECIPIENT;
    deploy_another_account_at(owner, recipient);
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);

    assert_state_before_transfer_single(owner, recipient, TOKEN_ID);
    state.safeTransferFrom(owner, recipient, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
    spy
        .assert_only_event_transfer_single(
            contract_address, owner, owner, recipient, TOKEN_ID, TOKEN_VALUE,
        );

    assert_state_after_transfer_single(owner, recipient, TOKEN_ID);
}

#[test]
fn test_safe_transfer_from_approved_operator() {
    let (mut state, owner) = setup();
    let recipient = RECIPIENT;
    deploy_another_account_at(owner, recipient);
    let operator = OPERATOR;
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);
    state.set_approval_for_all(operator, true);
    spy.assert_only_event_approval_for_all(contract_address, owner, operator, true);

    assert_state_before_transfer_single(owner, recipient, TOKEN_ID);

    start_cheat_caller_address(contract_address, operator);
    state.safe_transfer_from(owner, recipient, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
    spy
        .assert_only_event_transfer_single(
            contract_address, operator, owner, recipient, TOKEN_ID, TOKEN_VALUE,
        );

    assert_state_after_transfer_single(owner, recipient, TOKEN_ID);
}

#[test]
fn test_safeTransferFrom_approved_operator() {
    let (mut state, owner) = setup();
    let recipient = RECIPIENT;
    deploy_another_account_at(owner, recipient);
    let operator = OPERATOR;
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);
    state.set_approval_for_all(operator, true);
    spy.assert_only_event_approval_for_all(contract_address, owner, operator, true);

    assert_state_before_transfer_single(owner, recipient, TOKEN_ID);

    start_cheat_caller_address(contract_address, operator);
    state.safeTransferFrom(owner, recipient, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
    spy
        .assert_only_event_transfer_single(
            contract_address, operator, owner, recipient, TOKEN_ID, TOKEN_VALUE,
        );

    assert_state_after_transfer_single(owner, recipient, TOKEN_ID);
}

#[test]
#[should_panic(expected: 'ERC1155: invalid sender')]
fn test_safe_transfer_from_from_zero() {
    let (mut state, owner) = setup();
    start_cheat_caller_address(test_address(), owner);

    state.safe_transfer_from(ZERO, owner, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: invalid sender')]
fn test_safeTransferFrom_from_zero() {
    let (mut state, owner) = setup();
    start_cheat_caller_address(test_address(), owner);

    state.safeTransferFrom(ZERO, owner, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: invalid receiver')]
fn test_safe_transfer_from_to_zero() {
    let (mut state, owner) = setup();
    start_cheat_caller_address(test_address(), owner);

    state.safe_transfer_from(owner, ZERO, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: invalid receiver')]
fn test_safeTransferFrom_to_zero() {
    let (mut state, owner) = setup();
    start_cheat_caller_address(test_address(), owner);

    state.safeTransferFrom(owner, ZERO, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: unauthorized operator')]
fn test_safe_transfer_from_unauthorized() {
    let (mut state, owner) = setup();
    start_cheat_caller_address(test_address(), owner);

    state.safe_transfer_from(OTHER, owner, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: unauthorized operator')]
fn test_safeTransferFrom_unauthorized() {
    let (mut state, owner) = setup();
    start_cheat_caller_address(test_address(), owner);

    state.safeTransferFrom(OTHER, owner, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: insufficient balance')]
fn test_safe_transfer_from_insufficient_balance() {
    let (mut state, owner) = setup();
    start_cheat_caller_address(test_address(), owner);

    state.safe_transfer_from(owner, OTHER, TOKEN_ID, TOKEN_VALUE + 1, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: insufficient balance')]
fn test_safeTransferFrom_insufficient_balance() {
    let (mut state, owner) = setup();
    start_cheat_caller_address(test_address(), owner);

    state.safeTransferFrom(owner, OTHER, TOKEN_ID, TOKEN_VALUE + 1, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: safe transfer failed')]
fn test_safe_transfer_from_non_account_non_receiver() {
    let (mut state, owner) = setup();
    let non_receiver = setup_src5();
    start_cheat_caller_address(test_address(), owner);

    state.safe_transfer_from(owner, non_receiver, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: safe transfer failed')]
fn test_safeTransferFrom_non_account_non_receiver() {
    let (mut state, owner) = setup();
    let non_receiver = setup_src5();
    start_cheat_caller_address(test_address(), owner);

    state.safeTransferFrom(owner, non_receiver, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
}

//
// safe_batch_transfer_from & safeBatchTransferFrom
//

#[test]
fn test_safe_batch_transfer_from_owner_to_receiver() {
    let (mut state, owner) = setup();
    let recipient = setup_receiver();
    let (token_ids, values) = get_ids_and_values();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);

    assert_state_before_transfer_batch(owner, recipient, token_ids, values);
    state.safe_batch_transfer_from(owner, recipient, token_ids, values, EMPTY_DATA());
    spy
        .assert_only_event_transfer_batch(
            contract_address, owner, owner, recipient, token_ids, values,
        );

    assert_state_after_transfer_batch(owner, recipient, token_ids, values);
}

#[test]
fn test_safeBatchTransferFrom_owner_to_receiver() {
    let (mut state, owner) = setup();
    let recipient = setup_receiver();
    let (token_ids, values) = get_ids_and_values();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);

    assert_state_before_transfer_batch(owner, recipient, token_ids, values);
    state.safeBatchTransferFrom(owner, recipient, token_ids, values, EMPTY_DATA());
    spy
        .assert_only_event_transfer_batch(
            contract_address, owner, owner, recipient, token_ids, values,
        );

    assert_state_after_transfer_batch(owner, recipient, token_ids, values);
}

#[test]
fn test_safe_batch_transfer_from_owner_to_account() {
    let (mut state, owner) = setup();
    let recipient = RECIPIENT;
    deploy_another_account_at(owner, recipient);
    let (token_ids, values) = get_ids_and_values();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);

    assert_state_before_transfer_batch(owner, recipient, token_ids, values);
    state.safe_batch_transfer_from(owner, recipient, token_ids, values, EMPTY_DATA());
    spy
        .assert_only_event_transfer_batch(
            contract_address, owner, owner, recipient, token_ids, values,
        );

    assert_state_after_transfer_batch(owner, recipient, token_ids, values);
}

#[test]
fn test_safeBatchTransferFrom_owner_to_account() {
    let (mut state, owner) = setup();
    let recipient = RECIPIENT;
    deploy_another_account_at(owner, recipient);
    let (token_ids, values) = get_ids_and_values();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);

    assert_state_before_transfer_batch(owner, recipient, token_ids, values);
    state.safeBatchTransferFrom(owner, recipient, token_ids, values, EMPTY_DATA());
    spy
        .assert_only_event_transfer_batch(
            contract_address, owner, owner, recipient, token_ids, values,
        );

    assert_state_after_transfer_batch(owner, recipient, token_ids, values);
}


#[test]
fn test_safe_batch_transfer_from_approved_operator() {
    let (mut state, owner) = setup();
    let recipient = RECIPIENT;
    deploy_another_account_at(owner, recipient);
    let operator = OPERATOR;
    let (token_ids, values) = get_ids_and_values();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);
    state.set_approval_for_all(operator, true);
    spy.assert_only_event_approval_for_all(contract_address, owner, operator, true);

    assert_state_before_transfer_batch(owner, recipient, token_ids, values);

    start_cheat_caller_address(contract_address, operator);
    state.safe_batch_transfer_from(owner, recipient, token_ids, values, EMPTY_DATA());
    spy
        .assert_only_event_transfer_batch(
            contract_address, operator, owner, recipient, token_ids, values,
        );

    assert_state_after_transfer_batch(owner, recipient, token_ids, values);
}

#[test]
fn test_safeBatchTransferFrom_approved_operator() {
    let (mut state, owner) = setup();
    let recipient = RECIPIENT;
    deploy_another_account_at(owner, recipient);
    let operator = OPERATOR;
    let (token_ids, values) = get_ids_and_values();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);
    state.set_approval_for_all(operator, true);
    spy.assert_only_event_approval_for_all(contract_address, owner, operator, true);

    assert_state_before_transfer_batch(owner, recipient, token_ids, values);

    start_cheat_caller_address(contract_address, operator);
    state.safeBatchTransferFrom(owner, recipient, token_ids, values, EMPTY_DATA());
    spy
        .assert_only_event_transfer_batch(
            contract_address, operator, owner, recipient, token_ids, values,
        );

    assert_state_after_transfer_batch(owner, recipient, token_ids, values);
}

#[test]
#[should_panic(expected: 'ERC1155: invalid sender')]
fn test_safe_batch_transfer_from_from_zero() {
    let (mut state, owner) = setup();
    let (token_ids, values) = get_ids_and_values();

    start_cheat_caller_address(test_address(), owner);

    state.safe_batch_transfer_from(ZERO, owner, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: invalid sender')]
fn test_safeBatchTransferFrom_from_zero() {
    let (mut state, owner) = setup();
    let (token_ids, values) = get_ids_and_values();

    start_cheat_caller_address(test_address(), owner);

    state.safeBatchTransferFrom(ZERO, owner, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: invalid receiver')]
fn test_safe_batch_transfer_from_to_zero() {
    let (mut state, owner) = setup();
    let (token_ids, values) = get_ids_and_values();
    start_cheat_caller_address(test_address(), owner);

    state.safe_batch_transfer_from(owner, ZERO, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: invalid receiver')]
fn test_safeBatchTransferFrom_to_zero() {
    let (mut state, owner) = setup();
    let (token_ids, values) = get_ids_and_values();
    start_cheat_caller_address(test_address(), owner);

    state.safeBatchTransferFrom(owner, ZERO, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: unauthorized operator')]
fn test_safe_batch_transfer_from_unauthorized() {
    let (mut state, owner) = setup();
    let (token_ids, values) = get_ids_and_values();
    start_cheat_caller_address(test_address(), owner);

    state.safe_batch_transfer_from(OTHER, owner, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: unauthorized operator')]
fn test_safeBatchTransferFrom_unauthorized() {
    let (mut state, owner) = setup();
    let (token_ids, values) = get_ids_and_values();
    start_cheat_caller_address(test_address(), owner);

    state.safeBatchTransferFrom(OTHER, owner, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: insufficient balance')]
fn test_safe_batch_transfer_from_insufficient_balance() {
    let (mut state, owner) = setup();
    let token_ids = array![TOKEN_ID, TOKEN_ID_2].span();
    let values = array![TOKEN_VALUE + 1, TOKEN_VALUE_2].span();
    start_cheat_caller_address(test_address(), owner);

    state.safe_batch_transfer_from(owner, OTHER, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: insufficient balance')]
fn test_safeBatchTransferFrom_insufficient_balance() {
    let (mut state, owner) = setup();
    let token_ids = array![TOKEN_ID, TOKEN_ID_2].span();
    let values = array![TOKEN_VALUE + 1, TOKEN_VALUE_2].span();
    start_cheat_caller_address(test_address(), owner);

    state.safeBatchTransferFrom(owner, OTHER, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: safe transfer failed')]
fn test_safe_batch_transfer_from_non_account_non_receiver() {
    let (mut state, owner) = setup();
    let (token_ids, values) = get_ids_and_split_values(5);
    let non_receiver = setup_src5();
    start_cheat_caller_address(test_address(), owner);

    state.safe_batch_transfer_from(owner, non_receiver, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: safe transfer failed')]
fn test_safeBatchTransferFrom_non_account_non_receiver() {
    let (mut state, owner) = setup();
    let (token_ids, values) = get_ids_and_split_values(5);
    let non_receiver = setup_src5();
    start_cheat_caller_address(test_address(), owner);

    state.safeBatchTransferFrom(owner, non_receiver, token_ids, values, EMPTY_DATA());
}

//
// set_approval_for_all & is_approved_for_all
//

#[test]
fn test_set_approval_for_all_and_is_approved_for_all() {
    let mut state = COMPONENT_STATE();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, OWNER);

    let not_approved_for_all = !state.is_approved_for_all(OWNER, OPERATOR);
    assert!(not_approved_for_all);

    state.set_approval_for_all(OPERATOR, true);
    spy.assert_only_event_approval_for_all(contract_address, OWNER, OPERATOR, true);

    let is_approved_for_all = state.is_approved_for_all(OWNER, OPERATOR);
    assert!(is_approved_for_all);

    state.set_approval_for_all(OPERATOR, false);
    spy.assert_only_event_approval_for_all(contract_address, OWNER, OPERATOR, false);

    let not_approved_for_all = !state.is_approved_for_all(OWNER, OPERATOR);
    assert!(not_approved_for_all);
}

#[test]
#[should_panic(expected: 'ERC1155: self approval')]
fn test_set_approval_for_all_owner_equal_operator_true() {
    let mut state = COMPONENT_STATE();
    start_cheat_caller_address(test_address(), OWNER);
    state.set_approval_for_all(OWNER, true);
}

#[test]
#[should_panic(expected: 'ERC1155: self approval')]
fn test_set_approval_for_all_owner_equal_operator_false() {
    let mut state = COMPONENT_STATE();
    start_cheat_caller_address(test_address(), OWNER);
    state.set_approval_for_all(OWNER, false);
}

//
// setApprovalForAll & isApprovedForAll
//

#[test]
fn test_setApprovalForAll_and_isApprovedForAll() {
    let mut state = COMPONENT_STATE();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, OWNER);

    let not_approved_for_all = !state.isApprovedForAll(OWNER, OPERATOR);
    assert!(not_approved_for_all);

    state.setApprovalForAll(OPERATOR, true);
    spy.assert_only_event_approval_for_all(contract_address, OWNER, OPERATOR, true);

    let is_approved_for_all = state.isApprovedForAll(OWNER, OPERATOR);
    assert!(is_approved_for_all);

    state.setApprovalForAll(OPERATOR, false);
    spy.assert_only_event_approval_for_all(contract_address, OWNER, OPERATOR, false);

    let not_approved_for_all = !state.isApprovedForAll(OWNER, OPERATOR);
    assert!(not_approved_for_all);
}

#[test]
#[should_panic(expected: 'ERC1155: self approval')]
fn test_setApprovalForAll_owner_equal_operator_true() {
    let mut state = COMPONENT_STATE();
    start_cheat_caller_address(test_address(), OWNER);
    state.set_approval_for_all(OWNER, true);
}

#[test]
#[should_panic(expected: 'ERC1155: self approval')]
fn test_setApprovalForAll_owner_equal_operator_false() {
    let mut state = COMPONENT_STATE();
    start_cheat_caller_address(test_address(), OWNER);
    state.setApprovalForAll(OWNER, false);
}

//
// update
//

#[test]
fn test_update_single_from_non_zero_to_non_zero() {
    let (mut state, owner) = setup();
    let recipient = RECIPIENT;
    let token_ids = array![TOKEN_ID].span();
    let values = array![TOKEN_VALUE].span();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);

    assert_state_before_transfer_single(owner, recipient, TOKEN_ID);
    state.update(owner, recipient, token_ids, values);
    spy
        .assert_only_event_transfer_single(
            contract_address, owner, owner, recipient, TOKEN_ID, TOKEN_VALUE,
        );

    assert_state_after_transfer_single(owner, recipient, TOKEN_ID);
}

#[test]
fn test_update_batch_from_non_zero_to_non_zero() {
    let (mut state, owner) = setup();
    let recipient = RECIPIENT;
    let (token_ids, values) = get_ids_and_values();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);

    assert_state_before_transfer_batch(owner, recipient, token_ids, values);
    state.update(owner, recipient, token_ids, values);
    spy
        .assert_only_event_transfer_batch(
            contract_address, owner, owner, recipient, token_ids, values,
        );

    assert_state_after_transfer_batch(owner, recipient, token_ids, values);
}

#[test]
fn test_update_from_non_zero_to_zero() {
    let (mut state, owner) = setup();
    let recipient = ZERO;
    let (token_ids, values) = get_ids_and_values();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);

    assert_state_before_transfer_batch(owner, recipient, token_ids, values);
    state.update(owner, recipient, token_ids, values);
    spy
        .assert_only_event_transfer_batch(
            contract_address, owner, owner, recipient, token_ids, values,
        );

    assert_state_after_transfer_to_zero_batch(owner, recipient, token_ids);
}

#[test]
fn test_update_from_zero_to_non_zero() {
    let (mut state, owner) = setup();
    let recipient = RECIPIENT;
    let sender = ZERO;
    let (token_ids, values) = get_ids_and_values();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);

    assert_state_before_transfer_from_zero_batch(sender, recipient, token_ids);
    state.update(sender, recipient, token_ids, values);
    spy
        .assert_only_event_transfer_batch(
            contract_address, owner, sender, recipient, token_ids, values,
        );

    assert_state_after_transfer_from_zero_batch(sender, recipient, token_ids, values);
}

#[test]
#[should_panic(expected: 'ERC1155: no equal array length')]
fn test_update_token_ids_len_greater_than_values() {
    let (mut state, owner) = setup();
    let recipient = RECIPIENT;
    let token_ids = array![TOKEN_ID, TOKEN_ID_2].span();
    let values = array![TOKEN_VALUE].span();

    state.update(owner, recipient, token_ids, values);
}

#[test]
#[should_panic(expected: 'ERC1155: no equal array length')]
fn test_update_values_len_greater_than_token_ids() {
    let (mut state, owner) = setup();
    let recipient = RECIPIENT;
    let token_ids = array![TOKEN_ID].span();
    let values = array![TOKEN_VALUE, TOKEN_VALUE_2].span();

    state.update(owner, recipient, token_ids, values);
}

#[test]
#[should_panic(expected: 'ERC1155: insufficient balance')]
fn test_update_insufficient_balance() {
    let (mut state, owner) = setup();
    let recipient = RECIPIENT;
    let token_ids = array![TOKEN_ID].span();
    let values = array![TOKEN_VALUE + 1].span();

    state.update(owner, recipient, token_ids, values);
}

#[test]
fn test_update_calls_before_update_hook() {
    let (mut state, owner) = setup_with_hooks();
    let recipient = RECIPIENT;
    let token_ids = array![TOKEN_ID].span();
    let values = array![TOKEN_VALUE].span();

    let mut spy = spy_events();
    let contract_address = test_address();

    state.update(owner, recipient, token_ids, values);
    spy.assert_event_before_update(contract_address, owner, recipient, token_ids, values);
}

#[test]
fn test_update_calls_after_update_hook() {
    let (mut state, owner) = setup_with_hooks();
    let recipient = RECIPIENT;
    let token_ids = array![TOKEN_ID].span();
    let values = array![TOKEN_VALUE].span();

    let mut spy = spy_events();
    let contract_address = test_address();

    state.update(owner, recipient, token_ids, values);
    spy.assert_event_after_update(contract_address, owner, recipient, token_ids, values);
}


//
// update_with_acceptance_check
//

#[test]
fn test_update_wac_single_from_non_zero_to_non_zero() {
    let (mut state, owner) = setup();
    let recipient = setup_receiver();
    let token_ids = array![TOKEN_ID].span();
    let values = array![TOKEN_VALUE].span();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);

    assert_state_before_transfer_single(owner, recipient, TOKEN_ID);
    state.update_with_acceptance_check(owner, recipient, token_ids, values, EMPTY_DATA());
    spy
        .assert_only_event_transfer_single(
            contract_address, owner, owner, recipient, TOKEN_ID, TOKEN_VALUE,
        );

    assert_state_after_transfer_single(owner, recipient, TOKEN_ID);
}

#[test]
fn test_update_wac_single_from_non_zero_to_non_zero_account() {
    let (mut state, owner) = setup();
    let recipient = RECIPIENT;
    deploy_another_account_at(owner, recipient);
    let token_ids = array![TOKEN_ID].span();
    let values = array![TOKEN_VALUE].span();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);

    assert_state_before_transfer_single(owner, recipient, TOKEN_ID);
    state.update_with_acceptance_check(owner, recipient, token_ids, values, EMPTY_DATA());
    spy
        .assert_only_event_transfer_single(
            contract_address, owner, owner, recipient, TOKEN_ID, TOKEN_VALUE,
        );

    assert_state_after_transfer_single(owner, recipient, TOKEN_ID);
}

#[test]
fn test_update_wac_batch_from_non_zero_to_non_zero() {
    let (mut state, owner) = setup();
    let recipient = setup_receiver();
    let (token_ids, values) = get_ids_and_values();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);

    assert_state_before_transfer_batch(owner, recipient, token_ids, values);
    state.update_with_acceptance_check(owner, recipient, token_ids, values, EMPTY_DATA());
    spy
        .assert_only_event_transfer_batch(
            contract_address, owner, owner, recipient, token_ids, values,
        );

    assert_state_after_transfer_batch(owner, recipient, token_ids, values);
}

#[test]
fn test_update_wac_batch_from_non_zero_to_non_zero_account() {
    let (mut state, owner) = setup();
    let recipient = RECIPIENT;
    deploy_another_account_at(owner, recipient);
    let (token_ids, values) = get_ids_and_values();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);
    assert_state_before_transfer_batch(owner, recipient, token_ids, values);

    state.update_with_acceptance_check(owner, recipient, token_ids, values, EMPTY_DATA());
    spy
        .assert_only_event_transfer_batch(
            contract_address, owner, owner, recipient, token_ids, values,
        );

    assert_state_after_transfer_batch(owner, recipient, token_ids, values);
}

#[test]
#[should_panic(expected: "Contract not deployed at address: 0x0")]
fn test_update_wac_from_non_zero_to_zero() {
    let (mut state, owner) = setup();
    let recipient = ZERO;
    let (token_ids, values) = get_ids_and_values();
    start_cheat_caller_address(test_address(), owner);

    state.update_with_acceptance_check(owner, recipient, token_ids, values, EMPTY_DATA());
}

#[test]
fn test_update_wac_from_zero_to_non_zero() {
    let (mut state, owner) = setup();
    let recipient = setup_receiver();
    let sender = ZERO;
    let (token_ids, values) = get_ids_and_values();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);

    assert_state_before_transfer_from_zero_batch(sender, recipient, token_ids);
    state.update_with_acceptance_check(sender, recipient, token_ids, values, EMPTY_DATA());
    spy
        .assert_only_event_transfer_batch(
            contract_address, owner, sender, recipient, token_ids, values,
        );

    assert_state_after_transfer_from_zero_batch(sender, recipient, token_ids, values);
}

#[test]
fn test_update_wac_from_zero_to_non_zero_account() {
    let (mut state, owner) = setup();
    let recipient = RECIPIENT;
    deploy_another_account_at(owner, recipient);
    let sender = ZERO;
    let (token_ids, values) = get_ids_and_values();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);

    assert_state_before_transfer_from_zero_batch(sender, recipient, token_ids);
    state.update_with_acceptance_check(sender, recipient, token_ids, values, EMPTY_DATA());
    spy
        .assert_only_event_transfer_batch(
            contract_address, owner, sender, recipient, token_ids, values,
        );

    assert_state_after_transfer_from_zero_batch(sender, recipient, token_ids, values);
}

#[test]
#[should_panic(expected: 'ERC1155: no equal array length')]
fn test_update_wac_token_ids_len_greater_than_values() {
    let (mut state, owner) = setup();
    let recipient = RECIPIENT;
    let token_ids = array![TOKEN_ID, TOKEN_ID_2].span();
    let values = array![TOKEN_VALUE].span();

    state.update_with_acceptance_check(owner, recipient, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: no equal array length')]
fn test_update_wac_values_len_greater_than_token_ids() {
    let (mut state, owner) = setup();
    let recipient = RECIPIENT;
    let token_ids = array![TOKEN_ID].span();
    let values = array![TOKEN_VALUE, TOKEN_VALUE_2].span();

    state.update_with_acceptance_check(owner, recipient, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: insufficient balance')]
fn test_update_wac_insufficient_balance() {
    let (mut state, owner) = setup();
    let recipient = RECIPIENT;
    let token_ids = array![TOKEN_ID].span();
    let values = array![TOKEN_VALUE + 1].span();

    state.update_with_acceptance_check(owner, recipient, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: safe transfer failed')]
fn test_update_wac_single_to_non_receiver() {
    let (mut state, owner) = setup();
    let recipient = setup_src5();
    let token_ids = array![TOKEN_ID].span();
    let values = array![TOKEN_VALUE].span();
    start_cheat_caller_address(test_address(), owner);

    state.update_with_acceptance_check(owner, recipient, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: safe transfer failed')]
fn test_update_wac_batch_to_non_receiver() {
    let (mut state, owner) = setup();
    let recipient = setup_src5();
    let (token_ids, values) = get_ids_and_values();
    start_cheat_caller_address(test_address(), owner);

    state.update_with_acceptance_check(owner, recipient, token_ids, values, EMPTY_DATA());
}

//
// mint_with_acceptance_check
//

#[test]
fn test_mint_wac_to_receiver() {
    let mut state = COMPONENT_STATE();
    let recipient = setup_receiver();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(test_address(), OTHER);

    let balance_of_recipient = state.balance_of(recipient, TOKEN_ID);
    assert!(balance_of_recipient.is_zero());

    state.mint_with_acceptance_check(recipient, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
    spy
        .assert_only_event_transfer_single(
            contract_address, OTHER, ZERO, recipient, TOKEN_ID, TOKEN_VALUE,
        );

    let balance_of_recipient = state.balance_of(recipient, TOKEN_ID);
    assert_eq!(balance_of_recipient, TOKEN_VALUE);
}

#[test]
fn test_mint_wac_to_account() {
    let mut state = COMPONENT_STATE();
    let recipient = setup_account();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(test_address(), OTHER);

    let balance_of_recipient = state.balance_of(recipient, TOKEN_ID);
    assert!(balance_of_recipient.is_zero());

    state.mint_with_acceptance_check(recipient, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
    spy
        .assert_only_event_transfer_single(
            contract_address, OTHER, ZERO, recipient, TOKEN_ID, TOKEN_VALUE,
        );

    let balance_of_recipient = state.balance_of(recipient, TOKEN_ID);
    assert_eq!(balance_of_recipient, TOKEN_VALUE);
}

#[test]
#[should_panic(expected: 'ERC1155: invalid receiver')]
fn test_mint_wac_to_zero() {
    let mut state = COMPONENT_STATE();
    let recipient = ZERO;

    state.mint_with_acceptance_check(recipient, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: safe transfer failed')]
fn test_mint_wac_to_non_receiver() {
    let mut state = COMPONENT_STATE();
    let recipient = setup_src5();

    state.mint_with_acceptance_check(recipient, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
}

//
// batch_mint_with_acceptance_check
//

#[test]
fn test_batch_mint_wac_to_receiver() {
    let mut state = COMPONENT_STATE();
    let recipient = setup_receiver();
    let (token_ids, values) = get_ids_and_values();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, OTHER);

    let balance_of_recipient_token_1_before = state.balance_of(recipient, TOKEN_ID);
    assert!(balance_of_recipient_token_1_before.is_zero());
    let balance_of_recipient_token_2_before = state.balance_of(recipient, TOKEN_ID_2);
    assert!(balance_of_recipient_token_2_before.is_zero());

    state.batch_mint_with_acceptance_check(recipient, token_ids, values, EMPTY_DATA());
    spy
        .assert_only_event_transfer_batch(
            contract_address, OTHER, ZERO, recipient, token_ids, values,
        );

    let balance_of_recipient_token_1_after = state.balance_of(recipient, TOKEN_ID);
    assert_eq!(balance_of_recipient_token_1_after, TOKEN_VALUE);
    let balance_of_recipient_token_2_after = state.balance_of(recipient, TOKEN_ID_2);
    assert_eq!(balance_of_recipient_token_2_after, TOKEN_VALUE_2);
}

#[test]
fn test_batch_mint_wac_to_account() {
    let mut state = COMPONENT_STATE();
    let recipient = setup_account();
    let (token_ids, values) = get_ids_and_values();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, OTHER);

    let balance_of_recipient_token_1_before = state.balance_of(recipient, TOKEN_ID);
    assert!(balance_of_recipient_token_1_before.is_zero());
    let balance_of_recipient_token_2_before = state.balance_of(recipient, TOKEN_ID_2);
    assert!(balance_of_recipient_token_2_before.is_zero());

    state.batch_mint_with_acceptance_check(recipient, token_ids, values, EMPTY_DATA());
    spy
        .assert_only_event_transfer_batch(
            contract_address, OTHER, ZERO, recipient, token_ids, values,
        );

    let balance_of_recipient_token_1_after = state.balance_of(recipient, TOKEN_ID);
    assert_eq!(balance_of_recipient_token_1_after, TOKEN_VALUE);
    let balance_of_recipient_token_2_after = state.balance_of(recipient, TOKEN_ID_2);
    assert_eq!(balance_of_recipient_token_2_after, TOKEN_VALUE_2);
}

#[test]
#[should_panic(expected: 'ERC1155: invalid receiver')]
fn test_batch_mint_wac_to_zero() {
    let mut state = COMPONENT_STATE();
    let recipient = ZERO;
    let (token_ids, values) = get_ids_and_values();

    state.batch_mint_with_acceptance_check(recipient, token_ids, values, EMPTY_DATA());
}

#[test]
#[should_panic(expected: 'ERC1155: safe transfer failed')]
fn test_batch_mint_wac_to_non_receiver() {
    let mut state = COMPONENT_STATE();
    let recipient = setup_src5();
    let (token_ids, values) = get_ids_and_values();

    state.batch_mint_with_acceptance_check(recipient, token_ids, values, EMPTY_DATA());
}

//
// burn & batch_burn
//

#[test]
fn test_burn() {
    let (mut state, owner) = setup();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);

    let balance_of_owner = state.balance_of(owner, TOKEN_ID);
    assert_eq!(balance_of_owner, TOKEN_VALUE);

    state.burn(owner, TOKEN_ID, TOKEN_VALUE);
    spy
        .assert_only_event_transfer_single(
            contract_address, owner, owner, ZERO, TOKEN_ID, TOKEN_VALUE,
        );

    let balance_of_owner = state.balance_of(owner, TOKEN_ID);
    assert!(balance_of_owner.is_zero());
}

#[test]
#[should_panic(expected: 'ERC1155: invalid sender')]
fn test_burn_from_zero() {
    let mut state = COMPONENT_STATE();
    state.burn(ZERO, TOKEN_ID, TOKEN_VALUE);
}


#[test]
fn test_batch_burn() {
    let (mut state, owner) = setup();
    let (token_ids, values) = get_ids_and_values();
    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, owner);

    let balance_of_owner_token_1_before = state.balance_of(owner, TOKEN_ID);
    assert_eq!(balance_of_owner_token_1_before, TOKEN_VALUE);
    let balance_of_owner_token_2_before = state.balance_of(owner, TOKEN_ID_2);
    assert_eq!(balance_of_owner_token_2_before, TOKEN_VALUE_2);

    state.batch_burn(owner, token_ids, values);
    spy.assert_only_event_transfer_batch(contract_address, owner, owner, ZERO, token_ids, values);

    let balance_of_owner_token_1_after = state.balance_of(owner, TOKEN_ID);
    assert!(balance_of_owner_token_1_after.is_zero());
    let balance_of_owner_token_2_after = state.balance_of(owner, TOKEN_ID_2);
    assert!(balance_of_owner_token_2_after.is_zero());
}

#[test]
#[should_panic(expected: 'ERC1155: invalid sender')]
fn test_batch_burn_from_zero() {
    let mut state = COMPONENT_STATE();
    let (token_ids, values) = get_ids_and_values();
    state.batch_burn(ZERO, token_ids, values);
}

//
// Helpers
//

fn assert_state_before_transfer_single(
    sender: ContractAddress, recipient: ContractAddress, token_id: u256,
) {
    let state = COMPONENT_STATE();
    assert_eq!(state.balance_of(sender, token_id), TOKEN_VALUE);
    assert!(state.balance_of(recipient, token_id).is_zero());
}

fn assert_state_after_transfer_single(
    sender: ContractAddress, recipient: ContractAddress, token_id: u256,
) {
    let state = COMPONENT_STATE();
    assert!(state.balance_of(sender, token_id).is_zero());
    assert_eq!(state.balance_of(recipient, token_id), TOKEN_VALUE);
}

fn assert_state_before_transfer_batch(
    sender: ContractAddress, recipient: ContractAddress, token_ids: Span<u256>, values: Span<u256>,
) {
    let state = COMPONENT_STATE();
    let mut index = 0;
    while index != token_ids.len() {
        let balance_of_sender = state.balance_of(sender, *token_ids.at(index));
        assert_eq!(balance_of_sender, *values.at(index));
        let balance_of_recipient = state.balance_of(recipient, *token_ids.at(index));
        assert!(balance_of_recipient.is_zero());

        index += 1;
    }
}

fn assert_state_before_transfer_from_zero_batch(
    sender: ContractAddress, recipient: ContractAddress, token_ids: Span<u256>,
) {
    let state = COMPONENT_STATE();
    let mut index = 0;
    while index != token_ids.len() {
        let balance_of_sender = state.balance_of(sender, *token_ids.at(index));
        assert!(balance_of_sender.is_zero());
        let balance_of_recipient = state.balance_of(recipient, *token_ids.at(index));
        assert!(balance_of_recipient.is_zero());

        index += 1;
    }
}

fn assert_state_after_transfer_batch(
    sender: ContractAddress, recipient: ContractAddress, token_ids: Span<u256>, values: Span<u256>,
) {
    let state = COMPONENT_STATE();
    let mut index = 0;
    while index != token_ids.len() {
        let balance_of_sender = state.balance_of(sender, *token_ids.at(index));
        assert!(balance_of_sender.is_zero());
        let balance_of_recipient = state.balance_of(recipient, *token_ids.at(index));
        assert_eq!(balance_of_recipient, *values.at(index));

        index += 1;
    }
}

fn assert_state_after_transfer_to_zero_batch(
    sender: ContractAddress, recipient: ContractAddress, token_ids: Span<u256>,
) {
    let state = COMPONENT_STATE();
    let mut index = 0;
    while index != token_ids.len() {
        let balance_of_sender = state.balance_of(sender, *token_ids.at(index));
        assert!(balance_of_sender.is_zero());
        let balance_of_recipient = state.balance_of(recipient, *token_ids.at(index));
        assert!(balance_of_recipient.is_zero());

        index += 1;
    }
}

fn assert_state_after_transfer_from_zero_batch(
    sender: ContractAddress, recipient: ContractAddress, token_ids: Span<u256>, values: Span<u256>,
) {
    let state = COMPONENT_STATE();
    let mut index = 0;
    while index != token_ids.len() {
        let balance_of_sender = state.balance_of(sender, *token_ids.at(index));
        assert!(balance_of_sender.is_zero());
        let balance_of_recipient = state.balance_of(recipient, *token_ids.at(index));
        assert_eq!(balance_of_recipient, *values.at(index));

        index += 1;
    }
}

#[generate_trait]
impl ERC1155HooksSpyHelpersImpl of ERC1155HooksSpyHelpers {
    fn assert_event_before_update(
        ref self: EventSpy,
        contract: ContractAddress,
        from: ContractAddress,
        to: ContractAddress,
        token_ids: Span<u256>,
        values: Span<u256>,
    ) {
        let expected = SnakeERC1155MockWithHooks::Event::BeforeUpdate(
            SnakeERC1155MockWithHooks::BeforeUpdate { from, to, token_ids, values },
        );
        self.assert_emitted_single(contract, expected);
    }

    fn assert_event_after_update(
        ref self: EventSpy,
        contract: ContractAddress,
        from: ContractAddress,
        to: ContractAddress,
        token_ids: Span<u256>,
        values: Span<u256>,
    ) {
        let expected = SnakeERC1155MockWithHooks::Event::AfterUpdate(
            SnakeERC1155MockWithHooks::AfterUpdate { from, to, token_ids, values },
        );
        self.assert_emitted_single(contract, expected);
    }
}
