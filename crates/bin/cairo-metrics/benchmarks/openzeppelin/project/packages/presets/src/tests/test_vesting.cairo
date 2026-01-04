use core::num::traits::Zero;
use openzeppelin_finance::vesting::VestingComponent::InternalImpl;
use openzeppelin_test_common::erc20::deploy_erc20;
use openzeppelin_test_common::ownable::OwnableSpyHelpers;
use openzeppelin_test_common::vesting::VestingSpyHelpers;
use openzeppelin_testing as utils;
use openzeppelin_testing::constants::{OTHER, OWNER, ZERO};
use openzeppelin_testing::spy_events;
use openzeppelin_token::erc20::interface::{IERC20Dispatcher, IERC20DispatcherTrait};
use openzeppelin_utils::serde::SerializedAppend;
use snforge_std::{start_cheat_block_timestamp_global, start_cheat_caller_address};
use starknet::ContractAddress;
use crate::interfaces::{VestingWalletABIDispatcher, VestingWalletABIDispatcherTrait};

//
// Setup
//

#[derive(Copy, Drop)]
struct TestData {
    total_allocation: u256,
    beneficiary: ContractAddress,
    start: u64,
    duration: u64,
    cliff_duration: u64,
}

fn TEST_DATA() -> TestData {
    TestData {
        total_allocation: 200, beneficiary: OWNER, start: 30, duration: 100, cliff_duration: 0,
    }
}

fn setup(data: TestData) -> (VestingWalletABIDispatcher, ContractAddress) {
    let mut calldata = array![];
    calldata.append_serde(data.beneficiary);
    calldata.append_serde(data.start);
    calldata.append_serde(data.duration);
    calldata.append_serde(data.cliff_duration);
    let contract_address = utils::declare_and_deploy("VestingWallet", calldata);
    let token = deploy_erc20(contract_address, data.total_allocation);
    let vesting = VestingWalletABIDispatcher { contract_address };
    (vesting, token.contract_address)
}

//
// IVesting
//

#[test]
fn test_state_after_init() {
    let data = TEST_DATA();
    let (vesting, _) = setup(data);

    assert_eq!(vesting.start(), data.start);
    assert_eq!(vesting.duration(), data.duration);
    assert_eq!(vesting.cliff(), data.start + data.cliff_duration);
    assert_eq!(vesting.end(), data.start + data.duration);
    assert_eq!(vesting.owner(), data.beneficiary);
}

#[test]
fn test_vesting_schedule_no_cliff() {
    let data = TEST_DATA();
    let (vesting, token) = setup(data);
    let tokens_per_sec = data.total_allocation / data.duration.into();

    let mut time_passed = 0;
    while time_passed <= data.duration {
        let expected_vested_amount = tokens_per_sec * time_passed.into();
        let actual_vested_amount = vesting.vested_amount(token, data.start + time_passed);
        assert_eq!(actual_vested_amount, expected_vested_amount);

        time_passed += 1;
    }

    let end_timestamp = data.start + data.duration;
    assert_eq!(vesting.vested_amount(token, end_timestamp), data.total_allocation);
}

#[test]
fn test_vesting_schedule_with_cliff() {
    let mut data = TEST_DATA();
    data.cliff_duration = 30;
    let (vesting, token) = setup(data);
    let tokens_per_sec = data.total_allocation / data.duration.into();

    let mut time_passed = 0;
    while time_passed < data.cliff_duration {
        let actual_vested_amount = vesting.vested_amount(token, data.start + time_passed);
        assert_eq!(actual_vested_amount, 0);

        time_passed += 1;
    }

    while time_passed <= data.duration {
        let expected_vested_amount = tokens_per_sec * time_passed.into();
        let actual_vested_amount = vesting.vested_amount(token, data.start + time_passed);
        assert_eq!(actual_vested_amount, expected_vested_amount);

        time_passed += 1;
    }

    let end_timestamp = data.start + data.duration;
    assert_eq!(vesting.vested_amount(token, end_timestamp), data.total_allocation);
}

#[test]
fn test_release_single_call_within_duration() {
    let data = TEST_DATA();
    let (vesting, token) = setup(data);

    let time_passed = 40;
    let expected_release_amount = time_passed.into()
        * (data.total_allocation / data.duration.into());
    start_cheat_block_timestamp_global(data.start + time_passed);
    let mut spy = spy_events();

    assert_eq!(vesting.released(token), 0);
    assert_eq!(vesting.releasable(token), expected_release_amount);

    let actual_release_amount = vesting.release(token);
    assert_eq!(actual_release_amount, expected_release_amount);

    assert_eq!(vesting.released(token), expected_release_amount);
    assert_eq!(vesting.releasable(token), 0);

    spy.assert_only_event_amount_released(vesting.contract_address, token, expected_release_amount);
}

#[test]
fn test_release_single_call_after_end() {
    let data = TEST_DATA();
    let (vesting, token) = setup(data);

    let time_passed = data.duration + 1;
    start_cheat_block_timestamp_global(data.start + time_passed);
    let mut spy = spy_events();

    assert_eq!(vesting.released(token), 0);
    assert_eq!(vesting.releasable(token), data.total_allocation);

    let actual_release_amount = vesting.release(token);
    assert_eq!(actual_release_amount, data.total_allocation);

    assert_eq!(vesting.released(token), data.total_allocation);
    assert_eq!(vesting.releasable(token), 0);

    spy.assert_only_event_amount_released(vesting.contract_address, token, data.total_allocation);
}

#[test]
fn test_release_multiple_calls() {
    let mut data = TEST_DATA();
    data.cliff_duration = 30;
    let (vesting, token) = setup(data);

    // 1. Before cliff ended
    start_cheat_block_timestamp_global(vesting.cliff() - 1);
    assert_eq!(vesting.released(token), 0);
    assert_eq!(vesting.releasable(token), 0);

    vesting.release(token);

    assert_eq!(vesting.released(token), 0);
    assert_eq!(vesting.releasable(token), 0);

    // 2. When the cliff ended
    start_cheat_block_timestamp_global(vesting.cliff());
    assert_eq!(vesting.released(token), 0);
    assert_eq!(vesting.releasable(token), 60);

    vesting.release(token);

    assert_eq!(vesting.released(token), 60);
    assert_eq!(vesting.releasable(token), 0);

    // 3. When 40/100 seconds passed
    start_cheat_block_timestamp_global(data.start + 40);
    assert_eq!(vesting.released(token), 60);
    assert_eq!(vesting.releasable(token), 20);

    vesting.release(token);

    assert_eq!(vesting.released(token), 80);
    assert_eq!(vesting.releasable(token), 0);

    // 4. After the vesting ended
    start_cheat_block_timestamp_global(data.start + data.duration + 1);
    assert_eq!(vesting.released(token), 80);
    assert_eq!(vesting.releasable(token), 120);

    vesting.release(token);

    assert_eq!(vesting.released(token), data.total_allocation);
    assert_eq!(vesting.releasable(token), 0);
}

#[test]
fn test_release_after_ownership_transferred() {
    let data = TEST_DATA();
    let (vesting, token) = setup(data);
    let token_dispatcher = IERC20Dispatcher { contract_address: token };

    // 1. Release to initial owner
    let time_passed = 40;
    let release_amount_1 = 80;
    start_cheat_block_timestamp_global(data.start + time_passed);
    vesting.release(token);
    assert_eq!(vesting.released(token), release_amount_1);
    assert_eq!(token_dispatcher.balance_of(data.beneficiary), release_amount_1);

    // 2. Transfer ownership
    let new_owner = OTHER;
    start_cheat_caller_address(vesting.contract_address, data.beneficiary);
    vesting.transfer_ownership(new_owner);

    // 3. Release to new owner
    let release_amount_2 = data.total_allocation - release_amount_1;
    start_cheat_block_timestamp_global(data.start + data.duration);
    vesting.release(token);
    assert_eq!(vesting.released(token), data.total_allocation);
    assert_eq!(token_dispatcher.balance_of(data.beneficiary), release_amount_1);
    assert_eq!(token_dispatcher.balance_of(new_owner), release_amount_2);
}

//
// transfer_ownership & transferOwnership
//

#[test]
fn test_transfer_ownership() {
    let (vesting, _) = setup(TEST_DATA());
    let mut spy = spy_events();
    start_cheat_caller_address(vesting.contract_address, OWNER);
    vesting.transfer_ownership(OTHER);

    spy.assert_event_ownership_transferred(vesting.contract_address, OWNER, OTHER);
    assert_eq!(vesting.owner(), OTHER);
}

#[test]
#[should_panic(expected: 'New owner is the zero address')]
fn test_transfer_ownership_to_zero() {
    let (vesting, _) = setup(TEST_DATA());
    start_cheat_caller_address(vesting.contract_address, OWNER);
    vesting.transfer_ownership(ZERO);
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_transfer_ownership_from_nonowner() {
    let (vesting, _) = setup(TEST_DATA());
    start_cheat_caller_address(vesting.contract_address, OTHER);
    vesting.transfer_ownership(OTHER);
}

#[test]
fn test_transferOwnership() {
    let (vesting, _) = setup(TEST_DATA());
    let mut spy = spy_events();
    start_cheat_caller_address(vesting.contract_address, OWNER);
    vesting.transferOwnership(OTHER);

    spy.assert_event_ownership_transferred(vesting.contract_address, OWNER, OTHER);
    assert_eq!(vesting.owner(), OTHER);
}

#[test]
#[should_panic(expected: 'New owner is the zero address')]
fn test_transferOwnership_to_zero() {
    let (vesting, _) = setup(TEST_DATA());
    start_cheat_caller_address(vesting.contract_address, OWNER);
    vesting.transferOwnership(ZERO);
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_transferOwnership_from_nonowner() {
    let (vesting, _) = setup(TEST_DATA());
    start_cheat_caller_address(vesting.contract_address, OTHER);
    vesting.transferOwnership(OTHER);
}

//
// renounce_ownership & renounceOwnership
//

#[test]
fn test_renounce_ownership() {
    let (vesting, _) = setup(TEST_DATA());
    let mut spy = spy_events();
    start_cheat_caller_address(vesting.contract_address, OWNER);
    vesting.renounce_ownership();

    spy.assert_event_ownership_transferred(vesting.contract_address, OWNER, ZERO);
    assert!(vesting.owner().is_zero());
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_renounce_ownership_from_nonowner() {
    let (vesting, _) = setup(TEST_DATA());
    start_cheat_caller_address(vesting.contract_address, OTHER);
    vesting.renounce_ownership();
}

#[test]
fn test_renounceOwnership() {
    let (vesting, _) = setup(TEST_DATA());
    let mut spy = spy_events();
    start_cheat_caller_address(vesting.contract_address, OWNER);
    vesting.renounceOwnership();

    spy.assert_event_ownership_transferred(vesting.contract_address, OWNER, ZERO);
    assert!(vesting.owner().is_zero());
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_renounceOwnership_from_nonowner() {
    let (vesting, _) = setup(TEST_DATA());
    start_cheat_caller_address(vesting.contract_address, OTHER);
    vesting.renounceOwnership();
}
