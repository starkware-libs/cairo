use core::num::traits::Bounded;
use openzeppelin_test_common::mocks::checkpoint::{IMockTrace, MockTrace};
use starknet::storage_access::StorePacking;
use crate::structs::checkpoint::Checkpoint;

const _2_POW_184: felt252 = 0x10000000000000000000000000000000000000000000000;
const KEY_MASK: u256 = 0xffffffffffffffff;
const LOW_MASK: u256 = 0xffffffffffffffffffffffffffffffff;

fn CONTRACT_STATE() -> MockTrace::ContractState {
    MockTrace::contract_state_for_testing()
}

#[test]
fn test_push_checkpoint() {
    let mut mock_trace = CONTRACT_STATE();

    let (prev, new) = mock_trace.push_checkpoint(100, 1000);
    assert_eq!(prev, 0);
    assert_eq!(new, 1000);

    let (prev, new) = mock_trace.push_checkpoint(200, 2000);
    assert_eq!(prev, 1000);
    assert_eq!(new, 2000);
}

#[test]
fn test_get_latest() {
    let mut mock_trace = CONTRACT_STATE();

    mock_trace.push_checkpoint(100, 1000);
    mock_trace.push_checkpoint(200, 2000);

    let latest = mock_trace.get_latest();
    assert_eq!(latest, 2000);
}

#[test]
fn test_get_at_key() {
    let mut mock_trace = CONTRACT_STATE();

    mock_trace.push_checkpoint(100, 1000);
    mock_trace.push_checkpoint(200, 2000);
    mock_trace.push_checkpoint(300, 3000);

    let value_at_150 = mock_trace.get_at_key(150);
    assert_eq!(value_at_150, 1000);

    let value_at_250 = mock_trace.get_at_key(250);
    assert_eq!(value_at_250, 2000);

    let value_at_350 = mock_trace.get_at_key(350);
    assert_eq!(value_at_350, 3000);
}

#[test]
fn test_get_length() {
    let mut mock_trace = CONTRACT_STATE();

    assert_eq!(mock_trace.get_length(), 0);

    mock_trace.push_checkpoint(100, 1000);
    assert_eq!(mock_trace.get_length(), 1);

    mock_trace.push_checkpoint(200, 2000);
    assert_eq!(mock_trace.get_length(), 2);
}

#[test]
#[should_panic(expected: 'Unordered insertion')]
fn test_unordered_insertion() {
    let mut mock_trace = CONTRACT_STATE();

    mock_trace.push_checkpoint(200, 2000);
    mock_trace.push_checkpoint(100, 1000); // This should panic
}

#[test]
fn test_pack_big_key_and_value() {
    let key = Bounded::MAX;
    let value = Bounded::MAX;
    let checkpoint = Checkpoint { key, value };

    let (key_and_low, high) = StorePacking::pack(checkpoint);

    let expected_key: u256 = (key_and_low.into() / _2_POW_184.into()) & KEY_MASK;
    let expected_low: u256 = key_and_low.into() & LOW_MASK;
    let expected_high: felt252 = Bounded::<u128>::MAX.into();

    assert_eq!(key.into(), expected_key);
    assert_eq!(value.low.into(), expected_low);
    assert_eq!(high, expected_high);
}

#[test]
fn test_unpack_big_key_and_value() {
    let key_and_low = Bounded::<u64>::MAX.into() * _2_POW_184 + Bounded::<u128>::MAX.into();
    let high = Bounded::<u128>::MAX.into();

    let checkpoint: Checkpoint = StorePacking::unpack((key_and_low, high));

    let expected_key: u64 = Bounded::MAX;
    let expected_value: u256 = Bounded::MAX;

    assert_eq!(checkpoint.key, expected_key);
    assert_eq!(checkpoint.value, expected_value);
}
