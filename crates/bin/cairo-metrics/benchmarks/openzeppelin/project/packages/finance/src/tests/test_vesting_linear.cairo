use openzeppelin_access::ownable::interface::{IOwnableDispatcher, IOwnableDispatcherTrait};
use openzeppelin_test_common::mocks::vesting::LinearVestingMock;
use openzeppelin_test_common::vesting::VestingSpyHelpers;
use openzeppelin_testing::constants::{OTHER, OWNER};
use openzeppelin_testing::{EventSpyExt, spy_events};
use openzeppelin_token::erc20::interface::{IERC20Dispatcher, IERC20DispatcherTrait};
use snforge_std::{start_cheat_block_timestamp_global, start_cheat_caller_address};
use crate::tests::common::{TestData, VestingStrategy, set_transfer_to_fail, setup};
use crate::vesting::VestingComponent;
use crate::vesting::VestingComponent::InternalImpl;
use crate::vesting::interface::IVestingDispatcherTrait;

//
// Setup
//

type ComponentState = VestingComponent::ComponentState<LinearVestingMock::ContractState>;

fn COMPONENT_STATE() -> ComponentState {
    VestingComponent::component_state_for_testing()
}

fn TEST_DATA() -> TestData {
    TestData {
        strategy: VestingStrategy::Linear,
        total_allocation: 200,
        beneficiary: OWNER,
        start: 30,
        duration: 100,
        cliff_duration: 0,
    }
}

//
// Tests
//

#[test]
fn test_state_after_init() {
    let data = TEST_DATA();
    let (vesting, _) = setup(data);

    assert_eq!(vesting.start(), data.start);
    assert_eq!(vesting.duration(), data.duration);
    assert_eq!(vesting.cliff(), data.start + data.cliff_duration);
    assert_eq!(vesting.end(), data.start + data.duration);
    let beneficiary = IOwnableDispatcher { contract_address: vesting.contract_address }.owner();
    assert_eq!(beneficiary, data.beneficiary);
}

#[test]
#[should_panic(expected: 'Vesting: Invalid cliff duration')]
fn test_init_invalid_cliff_value() {
    let mut component_state = COMPONENT_STATE();
    let mut data = TEST_DATA();
    data.cliff_duration = data.duration + 1;

    component_state.initializer(data.start, data.duration, data.cliff_duration);
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
fn test_release_zero_amount() {
    let data = TEST_DATA();
    let (vesting, token) = setup(data);

    start_cheat_block_timestamp_global(data.start);
    let mut spy = spy_events();

    assert_eq!(vesting.releasable(token), 0);

    let actual_release_amount = vesting.release(token);
    assert_eq!(actual_release_amount, 0);

    spy.assert_no_events_left();
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
    let ownable_vesting = IOwnableDispatcher { contract_address: vesting.contract_address };
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
    ownable_vesting.transfer_ownership(new_owner);

    // 3. Release to new owner
    let release_amount_2 = data.total_allocation - release_amount_1;
    start_cheat_block_timestamp_global(data.start + data.duration);
    vesting.release(token);
    assert_eq!(vesting.released(token), data.total_allocation);
    assert_eq!(token_dispatcher.balance_of(data.beneficiary), release_amount_1);
    assert_eq!(token_dispatcher.balance_of(new_owner), release_amount_2);
}

#[test]
#[should_panic(expected: 'Vesting: Token transfer failed')]
fn test_panics_when_transfer_fails() {
    let data = TEST_DATA();
    let (vesting, token) = setup(data);

    let time_passed = 40;
    start_cheat_block_timestamp_global(data.start + time_passed);
    set_transfer_to_fail(token, true);

    vesting.release(token);
}
