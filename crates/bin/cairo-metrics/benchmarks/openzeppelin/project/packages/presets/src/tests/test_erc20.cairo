use core::num::traits::{Bounded, Zero};
use openzeppelin_test_common::erc20::ERC20SpyHelpers;
use openzeppelin_test_common::ownable::OwnableSpyHelpers;
use openzeppelin_test_common::upgrades::UpgradeableSpyHelpers;
use openzeppelin_testing as utils;
use openzeppelin_testing::common::IntoBase16String;
use openzeppelin_testing::constants::{
    CLASS_HASH_ZERO, DECIMALS, NAME, OTHER, OWNER, RECIPIENT, SPENDER, SUPPLY, SYMBOL, VALUE, ZERO,
};
use openzeppelin_testing::{EventSpyExt, EventSpyQueue as EventSpy, spy_events};
use openzeppelin_token::erc20::interface::{IERC20Dispatcher, IERC20DispatcherTrait};
use openzeppelin_utils::serde::SerializedAppend;
use snforge_std::start_cheat_caller_address;
use starknet::ClassHash;
use crate::interfaces::erc20::{
    ERC20UpgradeableABISafeDispatcher, ERC20UpgradeableABISafeDispatcherTrait,
};
use crate::interfaces::{ERC20UpgradeableABIDispatcher, ERC20UpgradeableABIDispatcherTrait};

fn V2_CLASS_HASH() -> ClassHash {
    utils::declare_class("SnakeERC20Mock").class_hash
}

//
// Setup
//

fn setup_dispatcher_with_event() -> (EventSpy, ERC20UpgradeableABIDispatcher) {
    let mut calldata = array![];

    calldata.append_serde(NAME());
    calldata.append_serde(SYMBOL());
    calldata.append_serde(SUPPLY);
    calldata.append_serde(OWNER);
    calldata.append_serde(OWNER);

    let spy = spy_events();
    let address = utils::declare_and_deploy("ERC20Upgradeable", calldata);
    (spy, ERC20UpgradeableABIDispatcher { contract_address: address })
}

fn setup_dispatcher() -> (EventSpy, ERC20UpgradeableABIDispatcher) {
    let (mut spy, dispatcher) = setup_dispatcher_with_event();
    spy.drop_all_events();
    (spy, dispatcher)
}

//
// constructor
//

#[test]
fn test_constructor() {
    let (mut spy, dispatcher) = setup_dispatcher_with_event();

    assert_eq!(dispatcher.owner(), OWNER);
    spy.assert_event_ownership_transferred(dispatcher.contract_address, ZERO, OWNER);

    assert_eq!(dispatcher.name(), NAME());
    assert_eq!(dispatcher.symbol(), SYMBOL());
    assert_eq!(dispatcher.decimals(), DECIMALS);
    assert_eq!(dispatcher.total_supply(), SUPPLY);
    assert_eq!(dispatcher.balance_of(OWNER), SUPPLY);
    spy.assert_only_event_transfer(dispatcher.contract_address, ZERO, OWNER, SUPPLY);
}

//
// Getters
//

#[test]
fn test_total_supply() {
    let (_, dispatcher) = setup_dispatcher();

    assert_eq!(dispatcher.total_supply(), SUPPLY);
    assert_eq!(dispatcher.totalSupply(), SUPPLY);
}

#[test]
fn test_balance_of() {
    let (_, dispatcher) = setup_dispatcher();

    assert_eq!(dispatcher.balance_of(OWNER), SUPPLY);
    assert_eq!(dispatcher.balanceOf(OWNER), SUPPLY);
}

#[test]
fn test_allowance() {
    let (_, mut dispatcher) = setup_dispatcher();

    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.approve(SPENDER, VALUE);

    let allowance = dispatcher.allowance(OWNER, SPENDER);
    assert_eq!(allowance, VALUE);
}

//
// approve
//

#[test]
fn test_approve() {
    let (mut spy, mut dispatcher) = setup_dispatcher();
    let allowance = dispatcher.allowance(OWNER, SPENDER);
    assert!(allowance.is_zero());

    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    assert!(dispatcher.approve(SPENDER, VALUE));

    let allowance = dispatcher.allowance(OWNER, SPENDER);
    assert_eq!(allowance, VALUE);

    spy.assert_only_event_approval(dispatcher.contract_address, OWNER, SPENDER, VALUE);
}

#[test]
#[should_panic(expected: 'ERC20: approve from 0')]
fn test_approve_from_zero() {
    let (_, mut dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, ZERO);
    dispatcher.approve(SPENDER, VALUE);
}

#[test]
#[should_panic(expected: 'ERC20: approve to 0')]
fn test_approve_to_zero() {
    let (_, mut dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.approve(Zero::zero(), VALUE);
}

//
// transfer
//

#[test]
fn test_transfer() {
    let (mut spy, mut dispatcher) = setup_dispatcher();

    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    assert!(dispatcher.transfer(RECIPIENT, VALUE));

    assert_eq!(dispatcher.balance_of(OWNER), SUPPLY - VALUE);
    assert_eq!(dispatcher.balance_of(RECIPIENT), VALUE);
    assert_eq!(dispatcher.total_supply(), SUPPLY);

    spy.assert_only_event_transfer(dispatcher.contract_address, OWNER, RECIPIENT, VALUE);
}

#[test]
#[should_panic(expected: 'ERC20: insufficient balance')]
fn test_transfer_not_enough_balance() {
    let (_, mut dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OWNER);

    let balance_plus_one = SUPPLY + 1;
    dispatcher.transfer(RECIPIENT, balance_plus_one);
}

#[test]
#[should_panic(expected: 'ERC20: transfer from 0')]
fn test_transfer_from_zero() {
    let (_, mut dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, ZERO);
    dispatcher.transfer(RECIPIENT, VALUE);
}

#[test]
#[should_panic(expected: 'ERC20: transfer to 0')]
fn test_transfer_to_zero() {
    let (_, mut dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.transfer(ZERO, VALUE);
}

//
// transfer_from & transferFrom
//

#[test]
fn test_transfer_from() {
    let (mut spy, mut dispatcher) = setup_dispatcher();

    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.approve(SPENDER, VALUE);
    spy.drop_event();

    start_cheat_caller_address(dispatcher.contract_address, SPENDER);
    assert!(dispatcher.transfer_from(OWNER, RECIPIENT, VALUE));

    spy.assert_event_approval(dispatcher.contract_address, OWNER, SPENDER, 0);
    spy.assert_only_event_transfer(dispatcher.contract_address, OWNER, RECIPIENT, VALUE);

    assert_eq!(dispatcher.balance_of(RECIPIENT), VALUE);
    assert_eq!(dispatcher.balance_of(OWNER), SUPPLY - VALUE);
    assert_eq!(dispatcher.allowance(OWNER, SPENDER), 0);
    assert_eq!(dispatcher.total_supply(), SUPPLY);
}

#[test]
fn test_transfer_from_doesnt_consume_infinite_allowance() {
    let (_, mut dispatcher) = setup_dispatcher();

    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.approve(SPENDER, Bounded::MAX);

    start_cheat_caller_address(dispatcher.contract_address, SPENDER);
    dispatcher.transfer_from(OWNER, RECIPIENT, VALUE);

    let allowance = dispatcher.allowance(OWNER, SPENDER);
    assert_eq!(allowance, Bounded::MAX, "Should not decrease");
}

#[test]
#[should_panic(expected: 'ERC20: insufficient allowance')]
fn test_transfer_from_greater_than_allowance() {
    let (_, mut dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.approve(SPENDER, VALUE);

    start_cheat_caller_address(dispatcher.contract_address, SPENDER);
    let allowance_plus_one = VALUE + 1;
    dispatcher.transfer_from(OWNER, RECIPIENT, allowance_plus_one);
}

#[test]
#[should_panic(expected: 'ERC20: transfer to 0')]
fn test_transfer_from_to_zero_address() {
    let (_, mut dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.approve(SPENDER, VALUE);

    start_cheat_caller_address(dispatcher.contract_address, SPENDER);
    dispatcher.transfer_from(OWNER, Zero::zero(), VALUE);
}

#[test]
#[should_panic(expected: 'ERC20: insufficient allowance')]
fn test_transfer_from_from_zero_address() {
    let (_, mut dispatcher) = setup_dispatcher();
    dispatcher.transfer_from(Zero::zero(), RECIPIENT, VALUE);
}

#[test]
fn test_transferFrom() {
    let (mut spy, mut dispatcher) = setup_dispatcher();

    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.approve(SPENDER, VALUE);
    spy.drop_event();

    start_cheat_caller_address(dispatcher.contract_address, SPENDER);
    assert!(dispatcher.transferFrom(OWNER, RECIPIENT, VALUE));

    spy.assert_event_approval(dispatcher.contract_address, OWNER, SPENDER, 0);
    spy.assert_only_event_transfer(dispatcher.contract_address, OWNER, RECIPIENT, VALUE);

    assert_eq!(dispatcher.balance_of(RECIPIENT), VALUE);
    assert_eq!(dispatcher.balance_of(OWNER), SUPPLY - VALUE);
    assert_eq!(dispatcher.allowance(OWNER, SPENDER), 0);
    assert_eq!(dispatcher.total_supply(), SUPPLY);
}

#[test]
fn test_transferFrom_doesnt_consume_infinite_allowance() {
    let (_, mut dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.approve(SPENDER, Bounded::MAX);

    start_cheat_caller_address(dispatcher.contract_address, SPENDER);
    dispatcher.transferFrom(OWNER, RECIPIENT, VALUE);

    let allowance = dispatcher.allowance(OWNER, SPENDER);
    assert_eq!(allowance, Bounded::MAX, "Should not decrease");
}

#[test]
#[should_panic(expected: 'ERC20: insufficient allowance')]
fn test_transferFrom_greater_than_allowance() {
    let (_, mut dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.approve(SPENDER, VALUE);

    start_cheat_caller_address(dispatcher.contract_address, SPENDER);
    let allowance_plus_one = VALUE + 1;
    dispatcher.transferFrom(OWNER, RECIPIENT, allowance_plus_one);
}

#[test]
#[should_panic(expected: 'ERC20: transfer to 0')]
fn test_transferFrom_to_zero_address() {
    let (_, mut dispatcher) = setup_dispatcher();
    start_cheat_caller_address(dispatcher.contract_address, OWNER);
    dispatcher.approve(SPENDER, VALUE);

    start_cheat_caller_address(dispatcher.contract_address, SPENDER);
    dispatcher.transferFrom(OWNER, Zero::zero(), VALUE);
}

#[test]
#[should_panic(expected: 'ERC20: insufficient allowance')]
fn test_transferFrom_from_zero_address() {
    let (_, mut dispatcher) = setup_dispatcher();
    dispatcher.transferFrom(Zero::zero(), RECIPIENT, VALUE);
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

    let safe_dispatcher = ERC20UpgradeableABISafeDispatcher {
        contract_address: v1.contract_address,
    };
    let result = safe_dispatcher.totalSupply();

    utils::assert_entrypoint_not_found_error(result, selector!("totalSupply"), v1.contract_address)
}

#[test]
fn test_state_persists_after_upgrade() {
    let (_, mut v1) = setup_dispatcher();
    let v2_class_hash = V2_CLASS_HASH();

    start_cheat_caller_address(v1.contract_address, OWNER);
    v1.transfer(RECIPIENT, VALUE);

    // Check RECIPIENT balance v1
    let camel_balance = v1.balanceOf(RECIPIENT);
    assert_eq!(camel_balance, VALUE);

    v1.upgrade(v2_class_hash);

    // Check RECIPIENT balance v2
    let v2 = IERC20Dispatcher { contract_address: v1.contract_address };
    let snake_balance = v2.balance_of(RECIPIENT);
    assert_eq!(snake_balance, camel_balance);
}
