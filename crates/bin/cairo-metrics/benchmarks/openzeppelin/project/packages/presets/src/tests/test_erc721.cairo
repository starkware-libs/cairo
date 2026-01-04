use core::num::traits::Zero;
use openzeppelin_introspection::interface::ISRC5_ID;
use openzeppelin_test_common::erc721::ERC721SpyHelpers;
use openzeppelin_test_common::ownable::OwnableSpyHelpers;
use openzeppelin_test_common::upgrades::UpgradeableSpyHelpers;
use openzeppelin_testing as utils;
use openzeppelin_testing::common::IntoBase16String;
use openzeppelin_testing::constants::{
    BASE_URI, CLASS_HASH_ZERO, DATA, NAME, OPERATOR, OTHER, OWNER, PUBKEY, RECIPIENT, SPENDER,
    SYMBOL, ZERO,
};
use openzeppelin_testing::{EventSpyExt, EventSpyQueue as EventSpy, spy_events};
use openzeppelin_token::erc721::ERC721Component::ERC721Impl;
use openzeppelin_token::erc721::interface::{
    IERC721CamelOnlySafeDispatcher, IERC721CamelOnlySafeDispatcherTrait, IERC721Dispatcher,
    IERC721DispatcherTrait, IERC721_ID, IERC721_METADATA_ID,
};
use openzeppelin_utils::serde::SerializedAppend;
use snforge_std::start_cheat_caller_address;
use starknet::{ClassHash, ContractAddress};
use crate::ERC721Upgradeable;
use crate::ERC721Upgradeable::InternalImpl;
use crate::interfaces::{ERC721UpgradeableABIDispatcher, ERC721UpgradeableABIDispatcherTrait};


// Token IDs
const TOKEN_1: u256 = 1;
const TOKEN_2: u256 = 2;
const TOKEN_3: u256 = 3;
const NONEXISTENT: u256 = 9898;

const TOKENS_LEN: u256 = 3;

fn V2_CLASS_HASH() -> ClassHash {
    utils::declare_class("SnakeERC721Mock").class_hash
}

//
// Setup
//

fn setup_dispatcher_with_event() -> (EventSpy, ERC721UpgradeableABIDispatcher) {
    let mut calldata = array![];
    let mut token_ids = array![TOKEN_1, TOKEN_2, TOKEN_3];

    calldata.append_serde(NAME());
    calldata.append_serde(SYMBOL());
    calldata.append_serde(BASE_URI());
    calldata.append_serde(OWNER);
    calldata.append_serde(token_ids);
    calldata.append_serde(OWNER);

    let spy = spy_events();
    let address = utils::declare_and_deploy("ERC721Upgradeable", calldata);
    start_cheat_caller_address(address, OWNER);
    (spy, ERC721UpgradeableABIDispatcher { contract_address: address })
}

fn setup_dispatcher() -> (EventSpy, ERC721UpgradeableABIDispatcher) {
    let (mut spy, dispatcher) = setup_dispatcher_with_event();
    spy.drop_all_events();
    (spy, dispatcher)
}

fn setup_receiver() -> ContractAddress {
    utils::declare_and_deploy("DualCaseERC721ReceiverMock", array![])
}

fn setup_account() -> ContractAddress {
    let mut calldata = array![PUBKEY];
    utils::declare_and_deploy("DualCaseAccountMock", calldata)
}

//
// mint_assets
//

#[test]
fn test_mint_assets() {
    let mut state = ERC721Upgradeable::contract_state_for_testing();
    let mut token_ids = array![TOKEN_1, TOKEN_2, TOKEN_3].span();

    state.mint_assets(OWNER, token_ids);
    assert_eq!(state.erc721.balance_of(OWNER), TOKENS_LEN);

    loop {
        if token_ids.len() == 0 {
            break;
        }
        let id = *token_ids.pop_front().unwrap();
        assert_eq!(state.erc721.owner_of(id), OWNER);
    };
}

//
// constructor
//

#[test]
fn test_constructor() {
    let (_, dispatcher) = setup_dispatcher_with_event();

    // Check interface registration
    let mut interface_ids = array![ISRC5_ID, IERC721_ID, IERC721_METADATA_ID];
    loop {
        let id = interface_ids.pop_front().unwrap();
        if interface_ids.len() == 0 {
            break;
        }
        let supports_interface = dispatcher.supports_interface(id);
        assert!(supports_interface);
    }

    // Check token balance and owner
    let mut tokens = array![TOKEN_1, TOKEN_2, TOKEN_3];
    assert_eq!(dispatcher.balance_of(OWNER), TOKENS_LEN);

    loop {
        let token = tokens.pop_front().unwrap();
        if tokens.len() == 0 {
            break;
        }
        let current_owner = dispatcher.owner_of(token);
        assert_eq!(current_owner, OWNER);
    };
}

#[test]
fn test_constructor_events() {
    let (mut spy, dispatcher) = setup_dispatcher_with_event();
    let mut tokens = array![TOKEN_1, TOKEN_2, TOKEN_3];

    spy.assert_event_ownership_transferred(dispatcher.contract_address, ZERO, OWNER);
    loop {
        let token = tokens.pop_front().unwrap();
        if tokens.len() == 0 {
            // Includes event queue check
            spy.assert_only_event_transfer(dispatcher.contract_address, ZERO, OWNER, token);
            break;
        }
        spy.assert_event_transfer(dispatcher.contract_address, ZERO, OWNER, token);
    };
}

//
// Getters
//

#[test]
fn test_balance_of() {
    let (_, dispatcher) = setup_dispatcher();
    assert_eq!(dispatcher.balance_of(OWNER), TOKENS_LEN);
}

#[test]
#[should_panic(expected: 'ERC721: invalid account')]
fn test_balance_of_zero() {
    let (_, dispatcher) = setup_dispatcher();
    dispatcher.balance_of(ZERO);
}

#[test]
fn test_owner_of() {
    let (_, dispatcher) = setup_dispatcher();
    assert_eq!(dispatcher.owner_of(TOKEN_1), OWNER);
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test_owner_of_non_minted() {
    let (_, dispatcher) = setup_dispatcher();
    dispatcher.owner_of(7);
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test_token_uri_non_minted() {
    let (_, dispatcher) = setup_dispatcher();
    dispatcher.token_uri(7);
}

#[test]
fn test_token_uri() {
    let (_, dispatcher) = setup_dispatcher();

    let uri = dispatcher.token_uri(TOKEN_1);
    let expected = format!("{}{}", BASE_URI(), TOKEN_1);
    assert_eq!(uri, expected);
}

#[test]
fn test_get_approved() {
    let (_, dispatcher) = setup_dispatcher();
    let spender = SPENDER;
    let token_id = TOKEN_1;

    let approved = dispatcher.get_approved(token_id);
    assert!(approved.is_zero());

    dispatcher.approve(spender, token_id);
    let approved = dispatcher.get_approved(token_id);
    assert_eq!(approved, spender);
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test_get_approved_nonexistent() {
    let (_, dispatcher) = setup_dispatcher();
    dispatcher.get_approved(NONEXISTENT);
}

//
// approve
//

#[test]
fn test_approve_from_owner() {
    let (mut spy, dispatcher) = setup_dispatcher();

    dispatcher.approve(SPENDER, TOKEN_1);
    spy.assert_event_approval(dispatcher.contract_address, OWNER, SPENDER, TOKEN_1);

    let approved = dispatcher.get_approved(TOKEN_1);
    assert_eq!(approved, SPENDER);
}

#[test]
fn test_approve_from_operator() {
    let (mut spy, dispatcher) = setup_dispatcher();

    dispatcher.set_approval_for_all(OPERATOR, true);
    spy.drop_event();

    start_cheat_caller_address(dispatcher.contract_address, OPERATOR);
    dispatcher.approve(SPENDER, TOKEN_1);
    spy.assert_event_approval(dispatcher.contract_address, OWNER, SPENDER, TOKEN_1);

    let approved = dispatcher.get_approved(TOKEN_1);
    assert_eq!(approved, SPENDER);
}

#[test]
#[should_panic(expected: 'ERC721: unauthorized caller')]
fn test_approve_from_unauthorized() {
    let (_, dispatcher) = setup_dispatcher();

    start_cheat_caller_address(dispatcher.contract_address, OTHER);
    dispatcher.approve(SPENDER, TOKEN_1);
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test_approve_nonexistent() {
    let (_, dispatcher) = setup_dispatcher();
    dispatcher.approve(SPENDER, NONEXISTENT);
}

//
// set_approval_for_all
//

#[test]
fn test_set_approval_for_all() {
    let (mut spy, dispatcher) = setup_dispatcher();

    let is_not_approved_for_all = !dispatcher.is_approved_for_all(OWNER, OPERATOR);
    assert!(is_not_approved_for_all);

    dispatcher.set_approval_for_all(OPERATOR, true);
    spy.assert_event_approval_for_all(dispatcher.contract_address, OWNER, OPERATOR, true);

    let is_approved_for_all = dispatcher.is_approved_for_all(OWNER, OPERATOR);
    assert!(is_approved_for_all);

    dispatcher.set_approval_for_all(OPERATOR, false);
    spy.assert_event_approval_for_all(dispatcher.contract_address, OWNER, OPERATOR, false);

    let is_not_approved_for_all = !dispatcher.is_approved_for_all(OWNER, OPERATOR);
    assert!(is_not_approved_for_all);
}

//
// transfer_from & transferFrom
//

#[test]
fn test_transfer_from_owner() {
    let (mut spy, dispatcher) = setup_dispatcher();
    let token_id = TOKEN_1;
    let owner = OWNER;
    let recipient = RECIPIENT;

    // set approval to check reset
    dispatcher.approve(OTHER, token_id);
    spy.drop_event();

    assert_state_before_transfer(dispatcher, owner, recipient, token_id);

    let approved = dispatcher.get_approved(token_id);
    assert_eq!(approved, OTHER);

    dispatcher.transfer_from(owner, recipient, token_id);
    spy.assert_only_event_transfer(dispatcher.contract_address, owner, recipient, token_id);

    assert_state_after_transfer(dispatcher, owner, recipient, token_id);
}

#[test]
fn test_transferFrom_owner() {
    let (mut spy, dispatcher) = setup_dispatcher();
    let token_id = TOKEN_1;
    let owner = OWNER;
    let recipient = RECIPIENT;

    // set approval to check reset
    dispatcher.approve(OTHER, token_id);
    spy.drop_event();

    assert_state_before_transfer(dispatcher, owner, recipient, token_id);

    let approved = dispatcher.get_approved(token_id);
    assert_eq!(approved, OTHER);

    dispatcher.transferFrom(owner, recipient, token_id);
    spy.assert_only_event_transfer(dispatcher.contract_address, owner, recipient, token_id);

    assert_state_after_transfer(dispatcher, owner, recipient, token_id);
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test_transfer_from_nonexistent() {
    let (_, dispatcher) = setup_dispatcher();
    dispatcher.transfer_from(OWNER, RECIPIENT, NONEXISTENT);
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test_transferFrom_nonexistent() {
    let (_, dispatcher) = setup_dispatcher();
    dispatcher.transferFrom(OWNER, RECIPIENT, NONEXISTENT);
}

#[test]
#[should_panic(expected: 'ERC721: invalid receiver')]
fn test_transfer_from_to_zero() {
    let (_, dispatcher) = setup_dispatcher();
    dispatcher.transfer_from(OWNER, ZERO, TOKEN_1);
}

#[test]
#[should_panic(expected: 'ERC721: invalid receiver')]
fn test_transferFrom_to_zero() {
    let (_, dispatcher) = setup_dispatcher();
    dispatcher.transferFrom(OWNER, ZERO, TOKEN_1);
}

#[test]
fn test_transfer_from_to_owner() {
    let (mut spy, dispatcher) = setup_dispatcher();

    assert_state_transfer_to_self(dispatcher, OWNER, TOKEN_1, TOKENS_LEN);
    dispatcher.transfer_from(OWNER, OWNER, TOKEN_1);
    spy.assert_only_event_transfer(dispatcher.contract_address, OWNER, OWNER, TOKEN_1);

    assert_state_transfer_to_self(dispatcher, OWNER, TOKEN_1, TOKENS_LEN);
}

#[test]
fn test_transferFrom_to_owner() {
    let (mut spy, dispatcher) = setup_dispatcher();

    assert_state_transfer_to_self(dispatcher, OWNER, TOKEN_1, TOKENS_LEN);
    dispatcher.transferFrom(OWNER, OWNER, TOKEN_1);
    spy.assert_only_event_transfer(dispatcher.contract_address, OWNER, OWNER, TOKEN_1);

    assert_state_transfer_to_self(dispatcher, OWNER, TOKEN_1, TOKENS_LEN);
}

#[test]
fn test_transfer_from_approved() {
    let (mut spy, dispatcher) = setup_dispatcher();
    let token_id = TOKEN_1;
    let owner = OWNER;
    let recipient = RECIPIENT;
    assert_state_before_transfer(dispatcher, owner, recipient, token_id);

    dispatcher.approve(OPERATOR, token_id);
    spy.drop_event();

    start_cheat_caller_address(dispatcher.contract_address, OPERATOR);
    dispatcher.transfer_from(owner, recipient, token_id);
    spy.assert_only_event_transfer(dispatcher.contract_address, owner, recipient, token_id);

    assert_state_after_transfer(dispatcher, owner, recipient, token_id);
}

#[test]
fn test_transferFrom_approved() {
    let (mut spy, dispatcher) = setup_dispatcher();
    let token_id = TOKEN_1;
    let owner = OWNER;
    let recipient = RECIPIENT;
    assert_state_before_transfer(dispatcher, owner, recipient, token_id);

    dispatcher.approve(OPERATOR, token_id);
    spy.drop_event();

    start_cheat_caller_address(dispatcher.contract_address, OPERATOR);
    dispatcher.transferFrom(owner, recipient, token_id);
    spy.assert_only_event_transfer(dispatcher.contract_address, owner, recipient, token_id);

    assert_state_after_transfer(dispatcher, owner, recipient, token_id);
}

#[test]
fn test_transfer_from_approved_for_all() {
    let (mut spy, dispatcher) = setup_dispatcher();
    let token_id = TOKEN_1;
    let owner = OWNER;
    let recipient = RECIPIENT;

    assert_state_before_transfer(dispatcher, owner, recipient, token_id);

    dispatcher.set_approval_for_all(OPERATOR, true);
    spy.drop_event();

    start_cheat_caller_address(dispatcher.contract_address, OPERATOR);
    dispatcher.transfer_from(owner, recipient, token_id);
    spy.assert_only_event_transfer(dispatcher.contract_address, owner, recipient, token_id);

    assert_state_after_transfer(dispatcher, owner, recipient, token_id);
}

#[test]
fn test_transferFrom_approved_for_all() {
    let (mut spy, dispatcher) = setup_dispatcher();
    let token_id = TOKEN_1;
    let owner = OWNER;
    let recipient = RECIPIENT;

    assert_state_before_transfer(dispatcher, owner, recipient, token_id);

    dispatcher.set_approval_for_all(OPERATOR, true);
    spy.drop_event();

    start_cheat_caller_address(dispatcher.contract_address, OPERATOR);
    dispatcher.transferFrom(owner, recipient, token_id);
    spy.assert_only_event_transfer(dispatcher.contract_address, owner, recipient, token_id);

    assert_state_after_transfer(dispatcher, owner, recipient, token_id);
}

#[test]
#[should_panic(expected: 'ERC721: unauthorized caller')]
fn test_transfer_from_unauthorized() {
    let (_, dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OTHER);
    dispatcher.transfer_from(OWNER, RECIPIENT, TOKEN_1);
}

#[test]
#[should_panic(expected: 'ERC721: unauthorized caller')]
fn test_transferFrom_unauthorized() {
    let (_, dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OTHER);
    dispatcher.transferFrom(OWNER, RECIPIENT, TOKEN_1);
}

//
// safe_transfer_from & safeTransferFrom
//

#[test]
fn test_safe_transfer_from_to_account() {
    let (mut spy, dispatcher) = setup_dispatcher();
    let account = setup_account();
    let token_id = TOKEN_1;
    let owner = OWNER;
    spy.drop_all_events();

    assert_state_before_transfer(dispatcher, owner, account, token_id);

    dispatcher.safe_transfer_from(owner, account, token_id, DATA(true));
    spy.assert_only_event_transfer(dispatcher.contract_address, owner, account, token_id);

    assert_state_after_transfer(dispatcher, owner, account, token_id);
}

#[test]
fn test_safeTransferFrom_to_account() {
    let (mut spy, dispatcher) = setup_dispatcher();
    let account = setup_account();
    let token_id = TOKEN_1;
    let owner = OWNER;
    spy.drop_all_events();

    assert_state_before_transfer(dispatcher, owner, account, token_id);

    dispatcher.safeTransferFrom(owner, account, token_id, DATA(true));
    spy.assert_only_event_transfer(dispatcher.contract_address, owner, account, token_id);

    assert_state_after_transfer(dispatcher, owner, account, token_id);
}

#[test]
fn test_safe_transfer_from_to_receiver() {
    let (mut spy, dispatcher) = setup_dispatcher();
    let receiver = setup_receiver();
    let token_id = TOKEN_1;
    let owner = OWNER;

    assert_state_before_transfer(dispatcher, owner, receiver, token_id);

    dispatcher.safe_transfer_from(owner, receiver, token_id, DATA(true));
    spy.assert_only_event_transfer(dispatcher.contract_address, owner, receiver, token_id);

    assert_state_after_transfer(dispatcher, owner, receiver, token_id);
}

#[test]
fn test_safeTransferFrom_to_receiver() {
    let (mut spy, dispatcher) = setup_dispatcher();
    let receiver = setup_receiver();
    let token_id = TOKEN_1;
    let owner = OWNER;

    assert_state_before_transfer(dispatcher, owner, receiver, token_id);

    dispatcher.safeTransferFrom(owner, receiver, token_id, DATA(true));
    spy.assert_only_event_transfer(dispatcher.contract_address, owner, receiver, token_id);

    assert_state_after_transfer(dispatcher, owner, receiver, token_id);
}

#[test]
#[should_panic(expected: 'ERC721: safe transfer failed')]
fn test_safe_transfer_from_to_receiver_failure() {
    let (_, dispatcher) = setup_dispatcher();
    let receiver = setup_receiver();
    let token_id = TOKEN_1;
    let owner = OWNER;

    dispatcher.safe_transfer_from(owner, receiver, token_id, DATA(false));
}

#[test]
#[should_panic(expected: 'ERC721: safe transfer failed')]
fn test_safeTransferFrom_to_receiver_failure() {
    let (_, dispatcher) = setup_dispatcher();
    let receiver = setup_receiver();
    let token_id = TOKEN_1;
    let owner = OWNER;

    dispatcher.safeTransferFrom(owner, receiver, token_id, DATA(false));
}

#[test]
#[should_panic(expected: 'ENTRYPOINT_NOT_FOUND')]
fn test_safe_transfer_from_to_non_receiver() {
    let (_, dispatcher) = setup_dispatcher();
    let recipient = utils::declare_and_deploy("NonImplementingMock", array![]);
    let token_id = TOKEN_1;
    let owner = OWNER;

    dispatcher.safe_transfer_from(owner, recipient, token_id, DATA(true));
}

#[test]
#[should_panic(expected: 'ENTRYPOINT_NOT_FOUND')]
fn test_safeTransferFrom_to_non_receiver() {
    let (_, dispatcher) = setup_dispatcher();
    let recipient = utils::declare_and_deploy("NonImplementingMock", array![]);
    let token_id = TOKEN_1;
    let owner = OWNER;

    dispatcher.safeTransferFrom(owner, recipient, token_id, DATA(true));
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test_safe_transfer_from_nonexistent() {
    let (_, dispatcher) = setup_dispatcher();
    dispatcher.safe_transfer_from(OWNER, RECIPIENT, NONEXISTENT, DATA(true));
}

#[test]
#[should_panic(expected: 'ERC721: invalid token ID')]
fn test_safeTransferFrom_nonexistent() {
    let (_, dispatcher) = setup_dispatcher();
    dispatcher.safeTransferFrom(OWNER, RECIPIENT, NONEXISTENT, DATA(true));
}

#[test]
#[should_panic(expected: 'ERC721: invalid receiver')]
fn test_safe_transfer_from_to_zero() {
    let (_, dispatcher) = setup_dispatcher();
    dispatcher.safe_transfer_from(OWNER, ZERO, TOKEN_1, DATA(true));
}

#[test]
#[should_panic(expected: 'ERC721: invalid receiver')]
fn test_safeTransferFrom_to_zero() {
    let (_, dispatcher) = setup_dispatcher();
    dispatcher.safeTransferFrom(OWNER, ZERO, TOKEN_1, DATA(true));
}

#[test]
fn test_safe_transfer_from_to_owner() {
    let (mut spy, dispatcher) = setup_dispatcher();
    let token_id = TOKEN_1;
    let receiver = setup_receiver();

    dispatcher.transfer_from(OWNER, receiver, token_id);
    spy.drop_event();

    assert_state_transfer_to_self(dispatcher, receiver, token_id, 1);

    start_cheat_caller_address(dispatcher.contract_address, receiver);
    dispatcher.safe_transfer_from(receiver, receiver, token_id, DATA(true));
    spy.assert_only_event_transfer(dispatcher.contract_address, receiver, receiver, token_id);

    assert_state_transfer_to_self(dispatcher, receiver, token_id, 1);
}

#[test]
fn test_safeTransferFrom_to_owner() {
    let (mut spy, dispatcher) = setup_dispatcher();
    let token_id = TOKEN_1;
    let receiver = setup_receiver();

    dispatcher.transfer_from(OWNER, receiver, token_id);
    spy.drop_event();

    assert_state_transfer_to_self(dispatcher, receiver, token_id, 1);

    start_cheat_caller_address(dispatcher.contract_address, receiver);
    dispatcher.safeTransferFrom(receiver, receiver, token_id, DATA(true));
    spy.assert_only_event_transfer(dispatcher.contract_address, receiver, receiver, token_id);

    assert_state_transfer_to_self(dispatcher, receiver, token_id, 1);
}

#[test]
fn test_safe_transfer_from_approved() {
    let (mut spy, dispatcher) = setup_dispatcher();
    let receiver = setup_receiver();
    let token_id = TOKEN_1;
    let owner = OWNER;

    assert_state_before_transfer(dispatcher, owner, receiver, token_id);

    dispatcher.approve(OPERATOR, token_id);
    spy.drop_event();

    start_cheat_caller_address(dispatcher.contract_address, OPERATOR);
    dispatcher.safe_transfer_from(owner, receiver, token_id, DATA(true));
    spy.assert_only_event_transfer(dispatcher.contract_address, owner, receiver, token_id);

    assert_state_after_transfer(dispatcher, owner, receiver, token_id);
}

#[test]
fn test_safeTransferFrom_approved() {
    let (mut spy, dispatcher) = setup_dispatcher();
    let receiver = setup_receiver();
    let token_id = TOKEN_1;
    let owner = OWNER;

    assert_state_before_transfer(dispatcher, owner, receiver, token_id);

    dispatcher.approve(OPERATOR, token_id);
    spy.drop_event();

    start_cheat_caller_address(dispatcher.contract_address, OPERATOR);
    dispatcher.safeTransferFrom(owner, receiver, token_id, DATA(true));
    spy.assert_only_event_transfer(dispatcher.contract_address, owner, receiver, token_id);

    assert_state_after_transfer(dispatcher, owner, receiver, token_id);
}

#[test]
fn test_safe_transfer_from_approved_for_all() {
    let (mut spy, dispatcher) = setup_dispatcher();
    let receiver = setup_receiver();
    let token_id = TOKEN_1;
    let owner = OWNER;

    assert_state_before_transfer(dispatcher, owner, receiver, token_id);

    dispatcher.set_approval_for_all(OPERATOR, true);
    spy.drop_event();

    start_cheat_caller_address(dispatcher.contract_address, OPERATOR);
    dispatcher.safe_transfer_from(owner, receiver, token_id, DATA(true));
    spy.assert_only_event_transfer(dispatcher.contract_address, owner, receiver, token_id);

    assert_state_after_transfer(dispatcher, owner, receiver, token_id);
}

#[test]
fn test_safeTransferFrom_approved_for_all() {
    let (mut spy, dispatcher) = setup_dispatcher();
    let receiver = setup_receiver();
    let token_id = TOKEN_1;
    let owner = OWNER;

    assert_state_before_transfer(dispatcher, owner, receiver, token_id);

    dispatcher.set_approval_for_all(OPERATOR, true);
    spy.drop_event();

    start_cheat_caller_address(dispatcher.contract_address, OPERATOR);
    dispatcher.safeTransferFrom(owner, receiver, token_id, DATA(true));
    spy.assert_only_event_transfer(dispatcher.contract_address, owner, receiver, token_id);

    assert_state_after_transfer(dispatcher, owner, receiver, token_id);
}

#[test]
#[should_panic(expected: 'ERC721: unauthorized caller')]
fn test_safe_transfer_from_unauthorized() {
    let (_, dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OTHER);
    dispatcher.safe_transfer_from(OWNER, RECIPIENT, TOKEN_1, DATA(true));
}

#[test]
#[should_panic(expected: 'ERC721: unauthorized caller')]
fn test_safeTransferFrom_unauthorized() {
    let (_, dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OTHER);
    dispatcher.safeTransferFrom(OWNER, RECIPIENT, TOKEN_1, DATA(true));
}

//
// transfer_ownership & transferOwnership
//

#[test]
fn test_transfer_ownership() {
    let (mut spy, mut dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.transfer_ownership(OTHER);

    spy.assert_event_ownership_transferred(dispatcher.contract_address, OWNER, OTHER);
    assert_eq!(dispatcher.owner(), OTHER);
}

#[test]
#[should_panic(expected: 'New owner is the zero address')]
fn test_transfer_ownership_to_zero() {
    let (_, mut dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.transfer_ownership(ZERO);
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_transfer_ownership_from_nonowner() {
    let (_, mut dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OTHER);
    dispatcher.transfer_ownership(OTHER);
}

#[test]
fn test_transferOwnership() {
    let (mut spy, mut dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.transferOwnership(OTHER);

    spy.assert_event_ownership_transferred(dispatcher.contract_address, OWNER, OTHER);
    assert_eq!(dispatcher.owner(), OTHER);
}

#[test]
#[should_panic(expected: 'New owner is the zero address')]
fn test_transferOwnership_to_zero() {
    let (_, mut dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.transferOwnership(ZERO);
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_transferOwnership_from_nonowner() {
    let (_, mut dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OTHER);
    dispatcher.transferOwnership(OTHER);
}

//
// renounce_ownership & renounceOwnership
//

#[test]
fn test_renounce_ownership() {
    let (mut spy, mut dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.renounce_ownership();

    spy.assert_event_ownership_transferred(dispatcher.contract_address, OWNER, ZERO);
    assert!(dispatcher.owner().is_zero());
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_renounce_ownership_from_nonowner() {
    let (_, mut dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OTHER);
    dispatcher.renounce_ownership();
}

#[test]
fn test_renounceOwnership() {
    let (mut spy, mut dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.renounceOwnership();

    spy.assert_event_ownership_transferred(dispatcher.contract_address, OWNER, ZERO);
    assert!(dispatcher.owner().is_zero());
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_renounceOwnership_from_nonowner() {
    let (_, mut dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OTHER);
    dispatcher.renounceOwnership();
}

//
// upgrade
//

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_upgrade_unauthorized() {
    let (_, mut v1) = setup_dispatcher();
    start_cheat_caller_address(v1.contract_address, OTHER);
    v1.upgrade(CLASS_HASH_ZERO);
}

#[test]
#[should_panic(expected: 'Class hash cannot be zero')]
fn test_upgrade_with_class_hash_zero() {
    let (_, mut v1) = setup_dispatcher();

    start_cheat_caller_address(v1.contract_address, OWNER);
    v1.upgrade(CLASS_HASH_ZERO);
}

#[test]
fn test_upgraded_event() {
    let (mut spy, mut v1) = setup_dispatcher();
    let v2_class_hash = V2_CLASS_HASH();

    start_cheat_caller_address(v1.contract_address, OWNER);
    v1.upgrade(v2_class_hash);

    spy.assert_only_event_upgraded(v1.contract_address, v2_class_hash);
}

#[test]
#[feature("safe_dispatcher")]
fn test_v2_missing_camel_selector() {
    let (_, mut v1) = setup_dispatcher();
    let v2_class_hash = V2_CLASS_HASH();

    start_cheat_caller_address(v1.contract_address, OWNER);
    v1.upgrade(v2_class_hash);

    let safe_dispatcher = IERC721CamelOnlySafeDispatcher { contract_address: v1.contract_address };
    let mut result = safe_dispatcher.ownerOf(TOKEN_1);
    let selector = selector!("ownerOf");

    utils::assert_entrypoint_not_found_error(result, selector, v1.contract_address);
}

#[test]
fn test_state_persists_after_upgrade() {
    let (_, mut v1) = setup_dispatcher();
    let v2_class_hash = V2_CLASS_HASH();

    start_cheat_caller_address(v1.contract_address, OWNER);
    v1.transferFrom(OWNER, RECIPIENT, TOKEN_1);

    // Check RECIPIENT balance v1
    let camel_balance = v1.balanceOf(RECIPIENT);
    assert_eq!(camel_balance, 1);

    v1.upgrade(v2_class_hash);

    // Check RECIPIENT balance v2
    let v2 = IERC721Dispatcher { contract_address: v1.contract_address };
    let snake_balance = v2.balance_of(RECIPIENT);
    assert_eq!(snake_balance, camel_balance);
}

//
// Helpers
//

fn assert_state_before_transfer(
    dispatcher: ERC721UpgradeableABIDispatcher,
    owner: ContractAddress,
    recipient: ContractAddress,
    token_id: u256,
) {
    assert_eq!(dispatcher.owner_of(token_id), owner);
    assert_eq!(dispatcher.balance_of(owner), TOKENS_LEN);
    assert!(dispatcher.balance_of(recipient).is_zero());
}

fn assert_state_after_transfer(
    dispatcher: ERC721UpgradeableABIDispatcher,
    owner: ContractAddress,
    recipient: ContractAddress,
    token_id: u256,
) {
    let current_owner = dispatcher.owner_of(token_id);
    assert_eq!(current_owner, recipient);
    assert_eq!(dispatcher.balance_of(owner), TOKENS_LEN - 1);
    assert_eq!(dispatcher.balance_of(recipient), 1);

    let approved = dispatcher.get_approved(token_id);
    assert!(approved.is_zero());
}

fn assert_state_transfer_to_self(
    dispatcher: ERC721UpgradeableABIDispatcher,
    target: ContractAddress,
    token_id: u256,
    token_balance: u256,
) {
    assert_eq!(dispatcher.owner_of(token_id), target);
    assert_eq!(dispatcher.balance_of(target), token_balance);
}
