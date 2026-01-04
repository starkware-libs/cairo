use core::num::traits::Zero;
use openzeppelin_introspection::src5::SRC5Component::SRC5Impl;
use openzeppelin_test_common::erc721::ERC721SpyHelpers;
use openzeppelin_test_common::mocks::erc721::{DualCaseERC721Mock, SnakeERC721MockWithHooks};
use openzeppelin_testing as utils;
use openzeppelin_testing::constants::{
    BASE_URI, BASE_URI_2, CALLER, DATA, NAME, OPERATOR, OTHER, OWNER, PUBKEY, RECIPIENT, SPENDER,
    SYMBOL, TOKEN_ID, TOKEN_ID_2, ZERO,
};
use openzeppelin_testing::{EventSpyExt, EventSpyQueue as EventSpy, spy_events};
use snforge_std::{start_cheat_caller_address, test_address};
use starknet::ContractAddress;
use starknet::storage::StorageMapReadAccess;
use crate::erc721;
use crate::erc721::ERC721Component;
use crate::erc721::ERC721Component::{
    ERC721CamelOnlyImpl, ERC721Impl, ERC721MetadataImpl, InternalImpl,
};

//
// Setup
//

type ComponentState = ERC721Component::ComponentState<DualCaseERC721Mock::ContractState>;
type ComponentStateWithHooks =
    ERC721Component::ComponentState<SnakeERC721MockWithHooks::ContractState>;

fn CONTRACT_STATE() -> DualCaseERC721Mock::ContractState {
    DualCaseERC721Mock::contract_state_for_testing()
}
fn COMPONENT_STATE() -> ComponentState {
    ERC721Component::component_state_for_testing()
}
fn COMPONENT_STATE_WITH_HOOKS() -> ComponentStateWithHooks {
    ERC721Component::component_state_for_testing()
}

fn setup() -> ComponentState {
    let mut state = COMPONENT_STATE();
    state.initializer(NAME(), SYMBOL(), BASE_URI());
    state.mint(OWNER, TOKEN_ID);
    state
}

fn setup_with_hooks() -> ComponentStateWithHooks {
    let mut state = COMPONENT_STATE_WITH_HOOKS();
    state.initializer(NAME(), SYMBOL(), BASE_URI());
    state.mint(OWNER, TOKEN_ID);
    state
}

fn setup_receiver() -> ContractAddress {
    utils::declare_and_deploy("DualCaseERC721ReceiverMock", array![])
}

fn setup_account() -> ContractAddress {
    let calldata = array![PUBKEY];
    utils::declare_and_deploy("DualCaseAccountMock", calldata)
}

//
// Initializers
//

#[test]
fn test_initializer() {
    let mut state = COMPONENT_STATE();
    let mock_state = CONTRACT_STATE();

    state.initializer(NAME(), SYMBOL(), BASE_URI());

    assert_eq!(state.name(), NAME());
    assert_eq!(state.symbol(), SYMBOL());
    assert_eq!(state._base_uri(), BASE_URI());
    assert!(state.balance_of(OWNER).is_zero());

    let supports_ierc721 = mock_state.supports_interface(erc721::interface::IERC721_ID);
    assert!(supports_ierc721);

    let supports_ierc721_metadata = mock_state
        .supports_interface(erc721::interface::IERC721_METADATA_ID);
    assert!(supports_ierc721_metadata);

    let supports_isrc5 = mock_state
        .supports_interface(openzeppelin_introspection::interface::ISRC5_ID);
    assert!(supports_isrc5);
}

#[test]
fn test_initializer_no_metadata() {
    let mut state = COMPONENT_STATE();
    let mock_state = CONTRACT_STATE();

    state.initializer_no_metadata();

    let empty_str = "";
    assert_eq!(state.name(), empty_str.clone());
    assert_eq!(state.symbol(), empty_str.clone());
    assert_eq!(state._base_uri(), empty_str.clone());
    assert!(state.balance_of(OWNER).is_zero());

    let supports_ierc721 = mock_state.supports_interface(erc721::interface::IERC721_ID);
    assert!(supports_ierc721);

    let does_not_support_ierc721_metadata = !mock_state
        .supports_interface(erc721::interface::IERC721_METADATA_ID);
    assert!(does_not_support_ierc721_metadata);

    let supports_isrc5 = mock_state
        .supports_interface(openzeppelin_introspection::interface::ISRC5_ID);
    assert!(supports_isrc5);
}

//
// Getters
//

#[test]
fn test_balance_of() {
    let state = setup();
    assert_eq!(state.balance_of(OWNER), 1);
}

#[test]
#[should_panic(expected: 'ERC721: invalid account')]
fn test_balance_of_zero() {
    let state = setup();
    state.balance_of(ZERO);
}

#[test]
fn test_owner_of() {
    let state = setup();
    assert_eq!(state.owner_of(TOKEN_ID), OWNER);
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test_owner_of_non_minted() {
    let state = setup();
    state.owner_of(7);
}

#[test]
fn test_token_uri() {
    let state = setup();

    let uri = state.token_uri(TOKEN_ID);
    let expected = format!("{}{}", BASE_URI(), TOKEN_ID);
    assert_eq!(uri, expected);
}

#[test]
fn test_token_uri_not_set() {
    let mut state = COMPONENT_STATE();

    state.mint(OWNER, TOKEN_ID);
    let uri = state.token_uri(TOKEN_ID);
    let expected: ByteArray = "";
    assert_eq!(uri, expected);
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test_token_uri_non_minted() {
    let state = setup();
    state.token_uri(7);
}

#[test]
fn test_get_approved() {
    let mut state = setup();
    let spender = SPENDER;
    let token_id = TOKEN_ID;

    assert_eq!(state.get_approved(token_id), ZERO);
    state._approve(spender, token_id, ZERO);
    assert_eq!(state.get_approved(token_id), spender);
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test_get_approved_nonexistent() {
    let state = setup();
    state.get_approved(7);
}

//
// approve & _approve
//

#[test]
fn test_approve_from_owner() {
    let mut state = setup();
    let contract_address = test_address();
    let mut spy = spy_events();

    start_cheat_caller_address(contract_address, OWNER);
    state.approve(SPENDER, TOKEN_ID);

    spy.assert_only_event_approval(contract_address, OWNER, SPENDER, TOKEN_ID);

    let approved = state.get_approved(TOKEN_ID);
    assert_eq!(approved, SPENDER);
}

#[test]
fn test_approve_from_operator() {
    let mut state = setup();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, OWNER);
    state.set_approval_for_all(OPERATOR, true);

    let mut spy = spy_events();

    start_cheat_caller_address(contract_address, OPERATOR);
    state.approve(SPENDER, TOKEN_ID);
    spy.assert_only_event_approval(contract_address, OWNER, SPENDER, TOKEN_ID);

    let approved = state.get_approved(TOKEN_ID);
    assert_eq!(approved, SPENDER);
}

#[test]
#[should_panic(expected: 'ERC721: unauthorized caller')]
fn test_approve_from_unauthorized() {
    let mut state = setup();

    start_cheat_caller_address(test_address(), OTHER);
    state.approve(SPENDER, TOKEN_ID);
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test_approve_nonexistent() {
    let mut state = COMPONENT_STATE();
    state.approve(SPENDER, TOKEN_ID);
}

#[test]
fn test__approve() {
    let mut state = setup();
    let contract_address = test_address();
    let mut spy = spy_events();

    state._approve(SPENDER, TOKEN_ID, ZERO);
    spy.assert_only_event_approval(contract_address, OWNER, SPENDER, TOKEN_ID);

    let approved = state.get_approved(TOKEN_ID);
    assert_eq!(approved, SPENDER);
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test__approve_nonexistent() {
    let mut state = COMPONENT_STATE();
    state._approve(SPENDER, TOKEN_ID, ZERO);
}

#[test]
fn test__approve_auth_is_owner() {
    let mut state = setup();
    let contract_address = test_address();
    let mut spy = spy_events();

    state._approve(SPENDER, TOKEN_ID, OWNER);
    spy.assert_only_event_approval(contract_address, OWNER, SPENDER, TOKEN_ID);

    let approved = state.get_approved(TOKEN_ID);
    assert_eq!(approved, SPENDER);
}

#[test]
fn test__approve_auth_is_approved_for_all() {
    let mut state = setup();
    let contract_address = test_address();
    let auth = CALLER;

    start_cheat_caller_address(contract_address, OWNER);
    state.set_approval_for_all(auth, true);

    let mut spy = spy_events();

    state._approve(SPENDER, TOKEN_ID, auth);
    spy.assert_only_event_approval(contract_address, OWNER, SPENDER, TOKEN_ID);

    let approved = state.get_approved(TOKEN_ID);
    assert_eq!(approved, SPENDER);
}

#[test]
#[should_panic(expected: 'ERC721: unauthorized caller')]
fn test__approve_auth_not_authorized() {
    let mut state = setup();
    state._approve(SPENDER, TOKEN_ID, CALLER);
}

//
// set_approval_for_all & _set_approval_for_all
//

#[test]
fn test_set_approval_for_all() {
    let mut state = COMPONENT_STATE();
    let contract_address = test_address();
    let mut spy = spy_events();

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
#[should_panic(expected: 'ERC721: invalid operator')]
fn test_set_approval_for_all_invalid_operator() {
    let mut state = COMPONENT_STATE();
    state.set_approval_for_all(ZERO, true);
}

#[test]
fn test__set_approval_for_all() {
    let mut state = COMPONENT_STATE();
    let contract_address = test_address();
    let mut spy = spy_events();

    let not_approved_for_all = !state.is_approved_for_all(OWNER, OPERATOR);
    assert!(not_approved_for_all);

    state._set_approval_for_all(OWNER, OPERATOR, true);
    spy.assert_only_event_approval_for_all(contract_address, OWNER, OPERATOR, true);

    let is_approved_for_all = state.is_approved_for_all(OWNER, OPERATOR);
    assert!(is_approved_for_all);

    state._set_approval_for_all(OWNER, OPERATOR, false);
    spy.assert_only_event_approval_for_all(contract_address, OWNER, OPERATOR, false);

    let not_approved_for_all = !state.is_approved_for_all(OWNER, OPERATOR);
    assert!(not_approved_for_all);
}

#[test]
#[should_panic(expected: 'ERC721: invalid operator')]
fn test__set_approval_for_all_invalid_operator() {
    let mut state = COMPONENT_STATE();
    state._set_approval_for_all(OWNER, ZERO, true);
}

//
// transfer_from & transferFrom
//

#[test]
fn test_transfer_from_owner() {
    let mut state = setup();
    let contract_address = test_address();
    let token_id = TOKEN_ID;
    let owner = OWNER;
    let recipient = RECIPIENT;

    // set approval to check reset
    state._approve(OTHER, token_id, ZERO);

    assert_state_before_transfer(owner, recipient, token_id);

    let approved = state.get_approved(token_id);
    assert_eq!(approved, OTHER);

    let mut spy = spy_events();

    start_cheat_caller_address(contract_address, owner);
    state.transfer_from(owner, recipient, token_id);
    spy.assert_only_event_transfer(contract_address, owner, recipient, token_id);

    assert_state_after_transfer(owner, recipient, token_id);
}

#[test]
fn test_transferFrom_owner() {
    let mut state = setup();
    let contract_address = test_address();
    let token_id = TOKEN_ID;
    let owner = OWNER;
    let recipient = RECIPIENT;

    // set approval to check reset
    state._approve(OTHER, token_id, ZERO);

    assert_state_before_transfer(owner, recipient, token_id);

    let approved = state.get_approved(token_id);
    assert_eq!(approved, OTHER);

    let mut spy = spy_events();

    start_cheat_caller_address(contract_address, owner);
    state.transferFrom(owner, recipient, token_id);
    spy.assert_only_event_transfer(contract_address, owner, recipient, token_id);

    assert_state_after_transfer(owner, recipient, token_id);
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test_transfer_from_nonexistent() {
    let mut state = COMPONENT_STATE();
    start_cheat_caller_address(test_address(), OWNER);
    state.transfer_from(ZERO, RECIPIENT, TOKEN_ID);
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test_transferFrom_nonexistent() {
    let mut state = COMPONENT_STATE();
    start_cheat_caller_address(test_address(), OWNER);
    state.transferFrom(ZERO, RECIPIENT, TOKEN_ID);
}

#[test]
#[should_panic(expected: 'ERC721: invalid receiver')]
fn test_transfer_from_to_zero() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OWNER);
    state.transfer_from(OWNER, ZERO, TOKEN_ID);
}

#[test]
#[should_panic(expected: 'ERC721: invalid receiver')]
fn test_transferFrom_to_zero() {
    let mut state = setup();

    start_cheat_caller_address(test_address(), OWNER);
    state.transferFrom(OWNER, ZERO, TOKEN_ID);
}

#[test]
fn test_transfer_from_to_owner() {
    let mut state = setup();
    let contract_address = test_address();
    let mut spy = spy_events();

    assert_eq!(state.owner_of(TOKEN_ID), OWNER);
    assert_eq!(state.balance_of(OWNER), 1);

    start_cheat_caller_address(contract_address, OWNER);
    state.transfer_from(OWNER, OWNER, TOKEN_ID);
    spy.assert_only_event_transfer(contract_address, OWNER, OWNER, TOKEN_ID);

    assert_eq!(state.owner_of(TOKEN_ID), OWNER);
    assert_eq!(state.balance_of(OWNER), 1);
}

#[test]
fn test_transferFrom_to_owner() {
    let mut state = setup();
    let contract_address = test_address();
    let mut spy = spy_events();

    assert_eq!(state.owner_of(TOKEN_ID), OWNER);
    assert_eq!(state.balance_of(OWNER), 1);

    start_cheat_caller_address(contract_address, OWNER);
    state.transferFrom(OWNER, OWNER, TOKEN_ID);
    spy.assert_only_event_transfer(contract_address, OWNER, OWNER, TOKEN_ID);

    assert_eq!(state.owner_of(TOKEN_ID), OWNER);
    assert_eq!(state.balance_of(OWNER), 1);
}

#[test]
fn test_transfer_from_approved() {
    let mut state = setup();
    let contract_address = test_address();
    let token_id = TOKEN_ID;
    let owner = OWNER;
    let recipient = RECIPIENT;

    assert_state_before_transfer(owner, recipient, token_id);

    start_cheat_caller_address(contract_address, owner);
    state.approve(OPERATOR, token_id);

    let mut spy = spy_events();

    start_cheat_caller_address(contract_address, OPERATOR);
    state.transfer_from(owner, recipient, token_id);
    spy.assert_only_event_transfer(contract_address, owner, recipient, token_id);

    assert_state_after_transfer(owner, recipient, token_id);
}

#[test]
fn test_transferFrom_approved() {
    let mut state = setup();
    let contract_address = test_address();
    let token_id = TOKEN_ID;
    let owner = OWNER;
    let recipient = RECIPIENT;

    assert_state_before_transfer(owner, recipient, token_id);

    start_cheat_caller_address(contract_address, owner);
    state.approve(OPERATOR, token_id);

    let mut spy = spy_events();

    start_cheat_caller_address(contract_address, OPERATOR);
    state.transferFrom(owner, recipient, token_id);
    spy.assert_only_event_transfer(contract_address, owner, recipient, token_id);

    assert_state_after_transfer(owner, recipient, token_id);
}

#[test]
fn test_transfer_from_approved_for_all() {
    let mut state = setup();
    let contract_address = test_address();
    let token_id = TOKEN_ID;
    let owner = OWNER;
    let recipient = RECIPIENT;

    assert_state_before_transfer(owner, recipient, token_id);

    start_cheat_caller_address(contract_address, owner);
    state.set_approval_for_all(OPERATOR, true);

    let mut spy = spy_events();

    start_cheat_caller_address(contract_address, OPERATOR);
    state.transfer_from(owner, recipient, token_id);
    spy.assert_only_event_transfer(contract_address, owner, recipient, token_id);

    assert_state_after_transfer(owner, recipient, token_id);
}

#[test]
fn test_transferFrom_approved_for_all() {
    let mut state = setup();
    let contract_address = test_address();
    let token_id = TOKEN_ID;
    let owner = OWNER;
    let recipient = RECIPIENT;

    assert_state_before_transfer(owner, recipient, token_id);

    start_cheat_caller_address(contract_address, owner);
    state.set_approval_for_all(OPERATOR, true);

    let mut spy = spy_events();

    start_cheat_caller_address(contract_address, OPERATOR);
    state.transferFrom(owner, recipient, token_id);
    spy.assert_only_event_transfer(contract_address, owner, recipient, token_id);

    assert_state_after_transfer(owner, recipient, token_id);
}

#[test]
#[should_panic(expected: 'ERC721: unauthorized caller')]
fn test_transfer_from_unauthorized() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OTHER);
    state.transfer_from(OWNER, RECIPIENT, TOKEN_ID);
}

#[test]
#[should_panic(expected: 'ERC721: unauthorized caller')]
fn test_transferFrom_unauthorized() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OTHER);
    state.transferFrom(OWNER, RECIPIENT, TOKEN_ID);
}

//
// safe_transfer_from & safeTransferFrom
//

#[test]
fn test_safe_transfer_from_to_account() {
    let mut state = setup();
    let contract_address = test_address();
    let account = setup_account();
    let mut spy = spy_events();
    let token_id = TOKEN_ID;
    let owner = OWNER;

    assert_state_before_transfer(owner, account, token_id);

    start_cheat_caller_address(contract_address, owner);
    state.safe_transfer_from(owner, account, token_id, DATA(true));
    spy.assert_only_event_transfer(contract_address, owner, account, token_id);

    assert_state_after_transfer(owner, account, token_id);
}

#[test]
fn test_safeTransferFrom_to_account() {
    let mut state = setup();
    let contract_address = test_address();
    let account = setup_account();
    let mut spy = spy_events();
    let token_id = TOKEN_ID;
    let owner = OWNER;

    assert_state_before_transfer(owner, account, token_id);

    start_cheat_caller_address(contract_address, owner);
    state.safeTransferFrom(owner, account, token_id, DATA(true));
    spy.assert_only_event_transfer(contract_address, owner, account, token_id);

    assert_state_after_transfer(owner, account, token_id);
}

#[test]
fn test_safe_transfer_from_to_receiver() {
    let mut state = setup();
    let receiver = setup_receiver();
    let contract_address = test_address();
    let mut spy = spy_events();
    let token_id = TOKEN_ID;
    let owner = OWNER;

    assert_state_before_transfer(owner, receiver, token_id);

    start_cheat_caller_address(contract_address, owner);
    state.safe_transfer_from(owner, receiver, token_id, DATA(true));
    spy.assert_only_event_transfer(contract_address, owner, receiver, token_id);

    assert_state_after_transfer(owner, receiver, token_id);
}

#[test]
fn test_safeTransferFrom_to_receiver() {
    let mut state = setup();
    let receiver = setup_receiver();
    let contract_address = test_address();
    let mut spy = spy_events();
    let token_id = TOKEN_ID;
    let owner = OWNER;

    assert_state_before_transfer(owner, receiver, token_id);

    start_cheat_caller_address(contract_address, owner);
    state.safeTransferFrom(owner, receiver, token_id, DATA(true));
    spy.assert_only_event_transfer(contract_address, owner, receiver, token_id);

    assert_state_after_transfer(owner, receiver, token_id);
}

#[test]
#[should_panic(expected: 'ERC721: safe transfer failed')]
fn test_safe_transfer_from_to_receiver_failure() {
    let mut state = setup();
    let receiver = setup_receiver();
    let token_id = TOKEN_ID;
    let owner = OWNER;

    start_cheat_caller_address(test_address(), owner);
    state.safe_transfer_from(owner, receiver, token_id, DATA(false));
}

#[test]
#[should_panic(expected: 'ERC721: safe transfer failed')]
fn test_safeTransferFrom_to_receiver_failure() {
    let mut state = setup();
    let receiver = setup_receiver();
    let token_id = TOKEN_ID;
    let owner = OWNER;

    start_cheat_caller_address(test_address(), owner);
    state.safeTransferFrom(owner, receiver, token_id, DATA(false));
}

#[test]
#[ignore] // REASON: should_panic attribute not fit for complex panic messages.
#[should_panic(expected: 'ENTRYPOINT_NOT_FOUND')]
fn test_safe_transfer_from_to_non_receiver() {
    let mut state = setup();
    let recipient = utils::declare_and_deploy("NonImplementingMock", array![]);
    let token_id = TOKEN_ID;
    let owner = OWNER;

    start_cheat_caller_address(test_address(), owner);
    state.safe_transfer_from(owner, recipient, token_id, DATA(true));
}

#[test]
#[ignore] // REASON: should_panic attribute not fit for complex panic messages.
#[should_panic(expected: 'ENTRYPOINT_NOT_FOUND')]
fn test_safeTransferFrom_to_non_receiver() {
    let mut state = setup();
    let recipient = utils::declare_and_deploy("NonImplementingMock", array![]);
    let token_id = TOKEN_ID;
    let owner = OWNER;

    start_cheat_caller_address(test_address(), owner);
    state.safeTransferFrom(owner, recipient, token_id, DATA(true));
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test_safe_transfer_from_nonexistent() {
    let mut state = COMPONENT_STATE();
    start_cheat_caller_address(test_address(), OWNER);
    state.safe_transfer_from(ZERO, RECIPIENT, TOKEN_ID, DATA(true));
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test_safeTransferFrom_nonexistent() {
    let mut state = COMPONENT_STATE();
    start_cheat_caller_address(test_address(), OWNER);
    state.safeTransferFrom(ZERO, RECIPIENT, TOKEN_ID, DATA(true));
}

#[test]
#[should_panic(expected: 'ERC721: invalid receiver')]
fn test_safe_transfer_from_to_zero() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OWNER);
    state.safe_transfer_from(OWNER, ZERO, TOKEN_ID, DATA(true));
}

#[test]
#[should_panic(expected: 'ERC721: invalid receiver')]
fn test_safeTransferFrom_to_zero() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OWNER);
    state.safeTransferFrom(OWNER, ZERO, TOKEN_ID, DATA(true));
}

#[test]
fn test_safe_transfer_from_to_owner() {
    let mut state = COMPONENT_STATE();
    let contract_address = test_address();
    let token_id = TOKEN_ID;
    let owner = setup_receiver();

    state.initializer(NAME(), SYMBOL(), BASE_URI());
    state.mint(owner, token_id);

    assert_eq!(state.owner_of(token_id), owner);
    assert_eq!(state.balance_of(owner), 1);

    let mut spy = spy_events();

    start_cheat_caller_address(contract_address, owner);
    state.safe_transfer_from(owner, owner, token_id, DATA(true));
    spy.assert_only_event_transfer(contract_address, owner, owner, token_id);

    assert_eq!(state.owner_of(token_id), owner);
    assert_eq!(state.balance_of(owner), 1);
}

#[test]
fn test_safeTransferFrom_to_owner() {
    let mut state = COMPONENT_STATE();
    let contract_address = test_address();
    let token_id = TOKEN_ID;
    let owner = setup_receiver();

    state.initializer(NAME(), SYMBOL(), BASE_URI());
    state.mint(owner, token_id);
    let mut spy = spy_events();

    start_cheat_caller_address(contract_address, owner);
    state.safeTransferFrom(owner, owner, token_id, DATA(true));
    spy.assert_only_event_transfer(contract_address, owner, owner, token_id);

    assert_eq!(state.owner_of(token_id), owner);
    assert_eq!(state.balance_of(owner), 1);
}

#[test]
fn test_safe_transfer_from_approved() {
    let mut state = setup();
    let contract_address = test_address();
    let receiver = setup_receiver();
    let token_id = TOKEN_ID;
    let owner = OWNER;

    assert_state_before_transfer(owner, receiver, token_id);

    start_cheat_caller_address(contract_address, owner);
    state.approve(OPERATOR, token_id);

    let mut spy = spy_events();

    start_cheat_caller_address(contract_address, OPERATOR);
    state.safe_transfer_from(owner, receiver, token_id, DATA(true));
    spy.assert_only_event_transfer(contract_address, owner, receiver, token_id);

    assert_state_after_transfer(owner, receiver, token_id);
}

#[test]
fn test_safeTransferFrom_approved() {
    let mut state = setup();
    let contract_address = test_address();
    let receiver = setup_receiver();
    let token_id = TOKEN_ID;
    let owner = OWNER;

    assert_state_before_transfer(owner, receiver, token_id);

    start_cheat_caller_address(contract_address, owner);
    state.approve(OPERATOR, token_id);

    let mut spy = spy_events();

    start_cheat_caller_address(contract_address, OPERATOR);
    state.safeTransferFrom(owner, receiver, token_id, DATA(true));
    spy.assert_only_event_transfer(contract_address, owner, receiver, token_id);

    assert_state_after_transfer(owner, receiver, token_id);
}

#[test]
fn test_safe_transfer_from_approved_for_all() {
    let mut state = setup();
    let contract_address = test_address();
    let receiver = setup_receiver();
    let token_id = TOKEN_ID;
    let owner = OWNER;

    assert_state_before_transfer(owner, receiver, token_id);

    start_cheat_caller_address(contract_address, owner);
    state.set_approval_for_all(OPERATOR, true);

    let mut spy = spy_events();

    start_cheat_caller_address(contract_address, OPERATOR);
    state.safe_transfer_from(owner, receiver, token_id, DATA(true));
    spy.assert_only_event_transfer(contract_address, owner, receiver, token_id);

    assert_state_after_transfer(owner, receiver, token_id);
}

#[test]
fn test_safeTransferFrom_approved_for_all() {
    let mut state = setup();
    let contract_address = test_address();
    let receiver = setup_receiver();
    let token_id = TOKEN_ID;
    let owner = OWNER;

    assert_state_before_transfer(owner, receiver, token_id);

    start_cheat_caller_address(contract_address, owner);
    state.set_approval_for_all(OPERATOR, true);

    let mut spy = spy_events();

    start_cheat_caller_address(contract_address, OPERATOR);
    state.safeTransferFrom(owner, receiver, token_id, DATA(true));
    spy.assert_only_event_transfer(contract_address, owner, receiver, token_id);

    assert_state_after_transfer(owner, receiver, token_id);
}

#[test]
#[should_panic(expected: 'ERC721: unauthorized caller')]
fn test_safe_transfer_from_unauthorized() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OTHER);
    state.safe_transfer_from(OWNER, RECIPIENT, TOKEN_ID, DATA(true));
}

#[test]
#[should_panic(expected: 'ERC721: unauthorized caller')]
fn test_safeTransferFrom_unauthorized() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OTHER);
    state.safeTransferFrom(OWNER, RECIPIENT, TOKEN_ID, DATA(true));
}

//
// transfer
//

#[test]
fn test__transfer() {
    let mut state = setup();
    let contract_address = test_address();
    let mut spy = spy_events();
    let token_id = TOKEN_ID;
    let owner = OWNER;
    let recipient = RECIPIENT;

    assert_state_before_transfer(owner, recipient, token_id);

    state.transfer(owner, recipient, token_id);
    spy.assert_only_event_transfer(contract_address, owner, recipient, token_id);

    assert_state_after_transfer(owner, recipient, token_id);
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test__transfer_nonexistent() {
    let mut state = COMPONENT_STATE();
    state.transfer(ZERO, RECIPIENT, TOKEN_ID);
}

#[test]
#[should_panic(expected: 'ERC721: invalid receiver')]
fn test__transfer_to_zero() {
    let mut state = setup();
    state.transfer(OWNER, ZERO, TOKEN_ID);
}

#[test]
#[should_panic(expected: 'ERC721: invalid sender')]
fn test__transfer_from_invalid_owner() {
    let mut state = setup();
    state.transfer(RECIPIENT, OWNER, TOKEN_ID);
}

//
// mint
//

#[test]
fn test_mint() {
    let mut state = COMPONENT_STATE();
    let contract_address = test_address();
    let mut spy = spy_events();
    let recipient = RECIPIENT;
    let token_id = TOKEN_ID;

    assert_state_before_mint(recipient);
    state.mint(recipient, TOKEN_ID);
    spy.assert_only_event_transfer(contract_address, ZERO, recipient, token_id);

    assert_state_after_mint(recipient, token_id);
}

#[test]
#[should_panic(expected: 'ERC721: invalid receiver')]
fn test_mint_to_zero() {
    let mut state = COMPONENT_STATE();
    state.mint(ZERO, TOKEN_ID);
}

#[test]
#[should_panic(expected: 'ERC721: token already minted')]
fn test_mint_already_exist() {
    let mut state = setup();
    state.mint(RECIPIENT, TOKEN_ID);
}

//
// safe_mint
//

#[test]
fn test__safe_mint_to_receiver() {
    let mut state = COMPONENT_STATE();
    let contract_address = test_address();
    let mut spy = spy_events();
    let recipient = setup_receiver();
    let token_id = TOKEN_ID;

    assert_state_before_mint(recipient);
    state.safe_mint(recipient, token_id, DATA(true));
    spy.assert_only_event_transfer(contract_address, ZERO, recipient, token_id);

    assert_state_after_mint(recipient, token_id);
}

#[test]
fn test__safe_mint_to_account() {
    let mut state = COMPONENT_STATE();
    let contract_address = test_address();
    let account = setup_account();
    let mut spy = spy_events();
    let token_id = TOKEN_ID;

    assert_state_before_mint(account);
    state.safe_mint(account, token_id, DATA(true));
    spy.assert_only_event_transfer(contract_address, ZERO, account, token_id);

    assert_state_after_mint(account, token_id);
}

#[test]
#[ignore] // REASON: should_panic attribute not fit for complex panic messages.
#[should_panic(expected: 'ENTRYPOINT_NOT_FOUND')]
fn test__safe_mint_to_non_receiver() {
    let mut state = COMPONENT_STATE();
    let recipient = utils::declare_and_deploy("NonImplementingMock", array![]);
    let token_id = TOKEN_ID;

    assert_state_before_mint(recipient);
    state.safe_mint(recipient, token_id, DATA(true));
    assert_state_after_mint(recipient, token_id);
}

#[test]
#[should_panic(expected: 'ERC721: safe mint failed')]
fn test__safe_mint_to_receiver_failure() {
    let mut state = COMPONENT_STATE();
    let recipient = setup_receiver();
    let token_id = TOKEN_ID;

    assert_state_before_mint(recipient);
    state.safe_mint(recipient, token_id, DATA(false));
    assert_state_after_mint(recipient, token_id);
}

#[test]
#[should_panic(expected: 'ERC721: invalid receiver')]
fn test__safe_mint_to_zero() {
    let mut state = COMPONENT_STATE();
    state.safe_mint(ZERO, TOKEN_ID, DATA(true));
}

#[test]
#[should_panic(expected: 'ERC721: token already minted')]
fn test__safe_mint_already_exist() {
    let mut state = setup();
    state.safe_mint(RECIPIENT, TOKEN_ID, DATA(true));
}

//
// burn
//

#[test]
fn test_burn() {
    let mut state = setup();
    let contract_address = test_address();

    state._approve(OTHER, TOKEN_ID, ZERO);

    assert_eq!(state.owner_of(TOKEN_ID), OWNER);
    assert_eq!(state.balance_of(OWNER), 1);
    assert_eq!(state.get_approved(TOKEN_ID), OTHER);

    let mut spy = spy_events();

    state.burn(TOKEN_ID);
    spy.assert_only_event_transfer(contract_address, OWNER, ZERO, TOKEN_ID);

    assert_eq!(state.ERC721_owners.read(TOKEN_ID), ZERO);
    assert_eq!(state.balance_of(OWNER), 0);
    assert_eq!(state.ERC721_token_approvals.read(TOKEN_ID), ZERO);
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test_burn_nonexistent() {
    let mut state = COMPONENT_STATE();
    state.burn(TOKEN_ID);
}

//
// _set_base_uri & _base_uri
//

#[test]
fn test__base_uri_not_set() {
    let mut state = COMPONENT_STATE();

    let base_uri = state._base_uri();
    assert_eq!(base_uri, "");
}

#[test]
fn test__base_uri() {
    let mut state = setup();

    let base_uri = state._base_uri();
    assert_eq!(base_uri, BASE_URI());
}

#[test]
fn test__set_base_uri() {
    let mut state = COMPONENT_STATE();

    state._set_base_uri(BASE_URI());
    let base_uri = state._base_uri();
    assert_eq!(base_uri, BASE_URI());

    state._set_base_uri(BASE_URI_2());
    let base_uri_2 = state._base_uri();
    assert_eq!(base_uri_2, BASE_URI_2());
}

//
// Internals
//

#[test]
fn test__owner_of() {
    let mut state = setup();
    let owner = state._owner_of(TOKEN_ID);
    assert_eq!(owner, OWNER);
}

#[test]
fn test__require_owned() {
    let mut state = setup();
    let owner = state._require_owned(TOKEN_ID);
    assert_eq!(owner, OWNER);
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test__require_owned_non_existent() {
    let mut state = setup();
    state._require_owned(0x123);
}

#[test]
fn test__exists() {
    let mut state = COMPONENT_STATE();
    let token_id = TOKEN_ID;

    let not_exists = !state.exists(token_id);
    assert!(not_exists);

    let mut owner = state.ERC721_owners.read(token_id);
    assert!(owner.is_zero());

    state.mint(RECIPIENT, token_id);

    let exists = state.exists(token_id);
    assert!(exists);

    owner = state.ERC721_owners.read(token_id);
    assert_eq!(owner, RECIPIENT);

    state.burn(token_id);

    let not_exists = !state.exists(token_id);
    assert!(not_exists);

    owner = state.ERC721_owners.read(token_id);
    assert!(owner.is_zero());
}

#[test]
fn test__approve_with_optional_event_emitting() {
    let mut state = setup();
    let contract_address = test_address();
    let mut spy = spy_events();

    state._approve_with_optional_event(SPENDER, TOKEN_ID, ZERO, true);
    spy.assert_only_event_approval(contract_address, OWNER, SPENDER, TOKEN_ID);

    let approved = state.get_approved(TOKEN_ID);
    assert_eq!(approved, SPENDER);
}

#[test]
fn test__approve_with_optional_event_not_emitting() {
    let mut state = setup();
    let mut spy = spy_events();

    state._approve_with_optional_event(SPENDER, TOKEN_ID, ZERO, false);
    spy.assert_no_events_left_from(test_address());

    let approved = state.get_approved(TOKEN_ID);
    assert_eq!(approved, SPENDER);
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test__approve_with_optional_event_nonexistent_emitting() {
    let mut state = COMPONENT_STATE();
    state._approve_with_optional_event(SPENDER, TOKEN_ID, ZERO, true);
}

#[test]
fn test__approve_with_optional_event_nonexistent_not_emitting() {
    let mut state = setup();
    let mut spy = spy_events();

    state._approve_with_optional_event(SPENDER, TOKEN_ID, ZERO, false);
    spy.assert_no_events_left_from(test_address());

    let approved = state.get_approved(TOKEN_ID);
    assert_eq!(approved, SPENDER);
}

#[test]
fn test__approve_with_optional_event_auth_is_owner() {
    let mut state = setup();
    let mut spy = spy_events();

    state._approve_with_optional_event(SPENDER, TOKEN_ID, OWNER, false);
    spy.assert_no_events_left_from(test_address());

    let approved = state.get_approved(TOKEN_ID);
    assert_eq!(approved, SPENDER);
}

#[test]
fn test__approve_with_optional_event_auth_is_approved_for_all() {
    let mut state = setup();
    let auth = CALLER;
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, OWNER);
    state.set_approval_for_all(auth, true);

    let mut spy = spy_events();

    state._approve_with_optional_event(SPENDER, TOKEN_ID, auth, false);
    spy.assert_no_events_left_from(contract_address);

    let approved = state.get_approved(TOKEN_ID);
    assert_eq!(approved, SPENDER);
}

#[test]
#[should_panic(expected: 'ERC721: unauthorized caller')]
fn test__approve_with_optional_event_auth_not_authorized() {
    let mut state = setup();
    state._approve_with_optional_event(SPENDER, TOKEN_ID, CALLER, false);
}

#[test]
fn test__is_authorized_owner() {
    let mut state = setup();
    let authorized = state._is_authorized(OWNER, OWNER, TOKEN_ID);
    assert!(authorized);
}

#[test]
fn test__is_authorized_approved_for_all() {
    let mut state = setup();

    start_cheat_caller_address(test_address(), OWNER);
    state.set_approval_for_all(SPENDER, true);

    let authorized = state._is_authorized(OWNER, SPENDER, TOKEN_ID);
    assert!(authorized);
}

#[test]
fn test__is_authorized_approved() {
    let mut state = setup();

    start_cheat_caller_address(test_address(), OWNER);
    state.approve(SPENDER, TOKEN_ID);

    let authorized = state._is_authorized(OWNER, SPENDER, TOKEN_ID);
    assert!(authorized);
}

#[test]
fn test__is_authorized_not_authorized() {
    let mut state = setup();
    let not_authorized = !state._is_authorized(OWNER, CALLER, TOKEN_ID);
    assert!(not_authorized);
}

#[test]
fn test__is_authorized_zero_address() {
    let mut state = setup();
    let not_authorized = !state._is_authorized(OWNER, ZERO, TOKEN_ID);
    assert!(not_authorized);
}

#[test]
fn test__check_authorized_owner() {
    let mut state = setup();
    state._check_authorized(OWNER, OWNER, TOKEN_ID);
}

#[test]
fn test__check_authorized_approved_for_all() {
    let mut state = setup();

    start_cheat_caller_address(test_address(), OWNER);
    state.set_approval_for_all(SPENDER, true);

    state._check_authorized(OWNER, SPENDER, TOKEN_ID);
}

#[test]
fn test__check_authorized_approved() {
    let mut state = setup();

    start_cheat_caller_address(test_address(), OWNER);
    state.approve(SPENDER, TOKEN_ID);

    state._check_authorized(OWNER, SPENDER, TOKEN_ID);
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test__check_authorized_owner_is_zero() {
    let mut state = setup();
    state._check_authorized(ZERO, OWNER, TOKEN_ID);
}

#[test]
#[should_panic(expected: 'ERC721: unauthorized caller')]
fn test__check_authorized_not_authorized() {
    let mut state = setup();
    state._check_authorized(OWNER, CALLER, TOKEN_ID);
}

#[test]
#[should_panic(expected: 'ERC721: unauthorized caller')]
fn test__check_authorized_zero_address() {
    let mut state = setup();
    state._check_authorized(OWNER, ZERO, TOKEN_ID);
}

#[test]
fn test_update_mint() {
    let mut state = setup();
    let contract_address = test_address();
    let mut spy = spy_events();

    state.update(RECIPIENT, TOKEN_ID_2, ZERO);
    spy.assert_only_event_transfer(contract_address, ZERO, RECIPIENT, TOKEN_ID_2);

    let owner = state.owner_of(TOKEN_ID_2);
    assert_eq!(owner, RECIPIENT);

    let balance = state.balance_of(RECIPIENT);
    assert_eq!(balance, 1);
}

#[test]
fn test_update_burn() {
    let mut state = setup();
    let contract_address = test_address();
    let mut spy = spy_events();

    state.update(ZERO, TOKEN_ID, ZERO);
    spy.assert_only_event_transfer(contract_address, OWNER, ZERO, TOKEN_ID);

    let owner = state._owner_of(TOKEN_ID);
    assert_eq!(owner, ZERO);

    let balance = state.balance_of(OWNER);
    assert_eq!(balance, 0);
}

#[test]
fn test_update_transfer() {
    let mut state = setup();
    let contract_address = test_address();
    let mut spy = spy_events();

    state.update(RECIPIENT, TOKEN_ID, ZERO);
    spy.assert_only_event_transfer(contract_address, OWNER, RECIPIENT, TOKEN_ID);
    assert_state_after_transfer(OWNER, RECIPIENT, TOKEN_ID);
}

#[test]
fn test_update_auth_owner() {
    let mut state = setup();
    let contract_address = test_address();
    let mut spy = spy_events();

    state.update(RECIPIENT, TOKEN_ID, OWNER);
    spy.assert_only_event_transfer(contract_address, OWNER, RECIPIENT, TOKEN_ID);
    assert_state_after_transfer(OWNER, RECIPIENT, TOKEN_ID);
}

#[test]
fn test_update_auth_approved_for_all() {
    let mut state = setup();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, OWNER);
    state.set_approval_for_all(OPERATOR, true);

    let mut spy = spy_events();

    state.update(RECIPIENT, TOKEN_ID, OPERATOR);
    spy.assert_only_event_transfer(contract_address, OWNER, RECIPIENT, TOKEN_ID);
    assert_state_after_transfer(OWNER, RECIPIENT, TOKEN_ID);
}

#[test]
fn test_update_auth_approved() {
    let mut state = setup();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, OWNER);
    state.approve(OPERATOR, TOKEN_ID);

    let mut spy = spy_events();

    state.update(RECIPIENT, TOKEN_ID, OPERATOR);
    spy.assert_only_event_transfer(contract_address, OWNER, RECIPIENT, TOKEN_ID);
    assert_state_after_transfer(OWNER, RECIPIENT, TOKEN_ID);
}

#[test]
#[should_panic(expected: 'ERC721: unauthorized caller')]
fn test_update_auth_not_approved() {
    let mut state = setup();
    state.update(RECIPIENT, TOKEN_ID, CALLER);
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test_update_mint_auth_not_zero() {
    let mut state = setup();
    state.update(RECIPIENT, TOKEN_ID_2, CALLER);
}

#[test]
fn test_update_calls_before_update_hook() {
    let mut state = setup_with_hooks();

    let mut spy = spy_events();
    let contract_address = test_address();

    state.update(RECIPIENT, TOKEN_ID, OWNER);

    spy.assert_event_before_update(contract_address, RECIPIENT, TOKEN_ID, OWNER);
}

#[test]
fn test_update_calls_after_update_hook() {
    let mut state = setup_with_hooks();

    let mut spy = spy_events();
    let contract_address = test_address();

    state.update(RECIPIENT, TOKEN_ID, OWNER);

    spy.assert_event_after_update(contract_address, RECIPIENT, TOKEN_ID, OWNER)
}

//
// Helpers
//

fn assert_state_before_transfer(
    owner: ContractAddress, recipient: ContractAddress, token_id: u256,
) {
    let state = COMPONENT_STATE();
    assert_eq!(state.owner_of(token_id), owner);
    assert_eq!(state.balance_of(owner), 1);
    assert!(state.balance_of(recipient).is_zero());
}

fn assert_state_after_transfer(owner: ContractAddress, recipient: ContractAddress, token_id: u256) {
    let state = COMPONENT_STATE();
    assert_eq!(state.owner_of(token_id), recipient);
    assert_eq!(state.balance_of(owner), 0);
    assert_eq!(state.balance_of(recipient), 1);
    assert!(state.get_approved(token_id).is_zero());
}

fn assert_state_before_mint(recipient: ContractAddress) {
    let state = COMPONENT_STATE();
    assert!(state.balance_of(recipient).is_zero());
}

fn assert_state_after_mint(recipient: ContractAddress, token_id: u256) {
    let state = COMPONENT_STATE();
    assert_eq!(state.owner_of(token_id), recipient);
    assert_eq!(state.balance_of(recipient), 1);
    assert!(state.get_approved(token_id).is_zero());
}

#[generate_trait]
impl ERC721HooksSpyHelpersImpl of ERC721HooksSpyHelpers {
    fn assert_event_before_update(
        ref self: EventSpy,
        contract: ContractAddress,
        to: ContractAddress,
        token_id: u256,
        auth: ContractAddress,
    ) {
        let expected = SnakeERC721MockWithHooks::Event::BeforeUpdate(
            SnakeERC721MockWithHooks::BeforeUpdate { to, token_id, auth },
        );
        self.assert_emitted_single(contract, expected);
    }

    fn assert_event_after_update(
        ref self: EventSpy,
        contract: ContractAddress,
        to: ContractAddress,
        token_id: u256,
        auth: ContractAddress,
    ) {
        let expected = SnakeERC721MockWithHooks::Event::AfterUpdate(
            SnakeERC721MockWithHooks::AfterUpdate { to, token_id, auth },
        );
        self.assert_emitted_single(contract, expected);
    }
}
