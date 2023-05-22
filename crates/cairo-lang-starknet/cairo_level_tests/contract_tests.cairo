use array::ArrayTrait;
use array::SpanTrait;
use box::BoxTrait;
use option::OptionTrait;
use traits::Into;
use zeroable::Zeroable;
use clone::Clone;
use starknet::Event;
use test::test_utils::{assert_eq, assert_ne};

use super::utils::serialized_element;
use super::utils::single_deserialize;

#[contract]
mod TestContract {
    use array::ArrayTrait;
    use option::OptionTrait;
    use traits::Into;
    use starknet::StorageAddress;

    struct Storage {
        value: felt252,
        mapping: LegacyMap::<u128, bool>,
        large_mapping: LegacyMap::<u256, u256>,
    }

    #[view]
    fn get_plus_2(a: felt252) -> felt252 {
        a + 2
    }

    #[view]
    fn spend_all_gas() {
        spend_all_gas();
    }

    #[view]
    fn get_appended_array(mut arr: Array<felt252>) -> Array<felt252> {
        let elem = arr.len().into();
        arr.append(elem);
        arr
    }

    #[external]
    fn set_value(a: felt252) {
        value::write(a);
    }

    #[view]
    fn get_value() -> felt252 {
        value::read()
    }

    #[external]
    fn insert(key: u128) {
        mapping::write(key, true)
    }

    #[external]
    fn remove(key: u128) {
        mapping::write(key, false)
    }

    #[view]
    fn contains(key: u128) -> bool {
        mapping::read(key)
    }

    #[external]
    fn set_large(key: u256, value: u256) {
        large_mapping::write(key, value)
    }

    #[view]
    fn get_large(key: u256) -> u256 {
        large_mapping::read(key)
    }

    #[view]
    fn test_storage_address(storage_address: StorageAddress) -> StorageAddress {
        storage_address
    }
}

#[test]
#[should_panic]
fn test_wrapper_not_enough_args() {
    TestContract::__external::get_plus_2(Default::default().span());
}

#[test]
#[should_panic]
fn test_wrapper_too_many_enough_args() {
    let mut calldata = Default::default();
    calldata.append(1);
    calldata.append(2);
    TestContract::__external::get_plus_2(calldata.span());
}

#[test]
#[available_gas(30000)]
fn test_wrapper_valid_args() {
    let mut retdata = TestContract::__external::get_plus_2(serialized_element(1));
    assert_eq(single_deserialize(ref retdata), 3, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(20000)]
#[should_panic]
fn test_wrapper_valid_args_out_of_gas() {
    TestContract::__external::spend_all_gas(Default::default().span());
}

#[test]
#[available_gas(200000)]
fn test_wrapper_array_arg_and_output() {
    let mut calldata = Default::default();
    calldata.append(1);
    calldata.append(2);
    let mut retdata = TestContract::__external::get_appended_array(calldata.span());
    assert_eq(single_deserialize(ref retdata), 2, 'Wrong length');
    assert_eq(single_deserialize(ref retdata), 2, 'Wrong original value');
    assert_eq(single_deserialize(ref retdata), 1, 'Wrong added value');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(200000)]
fn read_first_value() {
    let mut retdata = TestContract::__external::get_value(Default::default().span());
    assert_eq(single_deserialize(ref retdata), 0, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(300000)]
fn write_read_value() {
    assert(TestContract::__external::set_value(serialized_element(4)).is_empty(), 'Not empty');
    let mut retdata = TestContract::__external::get_value(Default::default().span());
    assert_eq(single_deserialize(ref retdata), 4, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(200000)]
fn empty_start() {
    let mut retdata = TestContract::__external::contains(serialized_element(4));
    assert_eq(single_deserialize(ref retdata), 0, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(300000)]
fn contains_added() {
    assert(TestContract::__external::insert(serialized_element(4)).is_empty(), 'Not empty');
    let mut retdata = TestContract::__external::contains(serialized_element(4));
    assert_eq(single_deserialize(ref retdata), 1, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
    let mut retdata = TestContract::__external::contains(serialized_element(5));
    assert_eq(single_deserialize(ref retdata), 0, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(300000)]
fn not_contains_removed() {
    assert(TestContract::__external::insert(serialized_element(4)).is_empty(), 'Not empty');
    assert(TestContract::__external::remove(serialized_element(4)).is_empty(), 'Not empty');
    let mut retdata = TestContract::__external::contains(serialized_element(4));
    assert_eq(single_deserialize(ref retdata), 0, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(300000)]
fn read_large_first_value() {
    let mut retdata = TestContract::__external::get_large(
        serialized_element(u256 { low: 1_u128, high: 2_u128 })
    );
    assert_eq(single_deserialize(ref retdata), u256 { low: 0_u128, high: 0_u128 }, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(300000)]
fn write_read_large_value() {
    let mut args = Default::default();
    serde::Serde::serialize(@u256 { low: 1_u128, high: 2_u128 }, ref args);
    serde::Serde::serialize(@u256 { low: 3_u128, high: 4_u128 }, ref args);
    let mut retdata = TestContract::__external::set_large(args.span());
    assert(retdata.is_empty(), 'Array not empty');
    let mut retdata = TestContract::__external::get_large(
        serialized_element(u256 { low: 1_u128, high: 2_u128 })
    );
    assert_eq(single_deserialize(ref retdata), u256 { low: 3_u128, high: 4_u128 }, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(300000)]
fn test_get_block_info() {
    let info = starknet::get_block_info().unbox();
    assert_eq(info.block_number, 0_u64, 'non default block_number');
    assert_eq(info.block_timestamp, 0_u64, 'non default block_timestamp');
    assert(info.sequencer_address.is_zero(), 'non default sequencer_address');
    starknet::testing::set_block_number(1_u64);
    starknet::testing::set_block_timestamp(2_u64);
    starknet::testing::set_sequencer_address(starknet::contract_address_const::<3>());
    let info = starknet::get_block_info().unbox();
    assert_eq(info.block_number, 1_u64, 'block_number not set');
    assert_eq(info.block_timestamp, 2_u64, 'block_timestamp not set');
    assert_eq(info.sequencer_address.into(), 3, 'sequencer_address not set');
}

#[test]
#[available_gas(300000)]
fn test_get_caller_address() {
    assert(starknet::get_caller_address().is_zero(), 'non default value');
    starknet::testing::set_caller_address(starknet::contract_address_const::<1>());
    assert_eq(starknet::get_caller_address().into(), 1, 'not set value');
}

#[test]
#[available_gas(300000)]
fn test_get_contract_address() {
    assert(starknet::get_contract_address().is_zero(), 'non default value');
    starknet::testing::set_contract_address(starknet::contract_address_const::<1>());
    assert_eq(starknet::get_contract_address().into(), 1, 'not set value');
}

#[test]
#[available_gas(300000)]
fn test_get_version() {
    assert(starknet::get_tx_info().unbox().version.is_zero(), 'non default value');
    starknet::testing::set_version(1);
    assert_eq(starknet::get_tx_info().unbox().version, 1_felt252, 'not set value');
}

#[test]
#[available_gas(300000)]
fn test_get_account_contract_address() {
    assert(starknet::get_tx_info().unbox().account_contract_address.is_zero(), 'non default value');
    starknet::testing::set_account_contract_address(starknet::contract_address_const::<1>());
    assert_eq(starknet::get_tx_info().unbox().account_contract_address.into(), 1, 'not set value');
}

#[test]
#[available_gas(300000)]
fn test_get_max_fee() {
    assert_eq(starknet::get_tx_info().unbox().max_fee, 0_u128, 'non default value');
    starknet::testing::set_max_fee(1);
    assert_eq(starknet::get_tx_info().unbox().max_fee, 1_u128, 'not set value');
}

#[test]
#[available_gas(300000)]
fn test_get_transaction_hash() {
    assert(starknet::get_tx_info().unbox().transaction_hash.is_zero(), 'non default value');
    starknet::testing::set_transaction_hash(1);
    assert_eq(starknet::get_tx_info().unbox().transaction_hash, 1_felt252, 'not set value');
}

#[test]
#[available_gas(300000)]
fn test_get_chain_id() {
    assert(starknet::get_tx_info().unbox().chain_id.is_zero(), 'non default value');
    starknet::testing::set_chain_id(1);
    assert_eq(starknet::get_tx_info().unbox().chain_id, 1_felt252, 'not set value');
}

#[test]
#[available_gas(300000)]
fn test_get_nonce() {
    assert(starknet::get_tx_info().unbox().nonce.is_zero(), 'non default value');
    starknet::testing::set_nonce(1);
    assert_eq(starknet::get_tx_info().unbox().nonce, 1_felt252, 'not set value');
}

#[test]
#[available_gas(300000)]
fn test_get_signature() {
    assert(starknet::get_tx_info().unbox().signature.is_empty(), 'non default value');
    let mut signature = Default::default();
    signature.append('some');
    signature.append('signature');
    starknet::testing::set_signature(signature.span());
    let read_signature = starknet::get_tx_info().unbox().signature;
    assert_eq(read_signature.len(), 2, 'unexpected read size');
    assert_eq(*read_signature.at(0), 'some', 'unexpected element 0');
    assert_eq(*read_signature.at(1), 'signature', 'unexpected element 1');
}

#[test]
#[should_panic]
fn test_out_of_range_storage_address_from_felt252() -> starknet::StorageAddress {
    starknet::storage_address_try_from_felt252(-1).unwrap()
}

#[test]
#[available_gas(300000)]
fn test_storage_address() {
    let mut args = Default::default();
    args.append(0x17);
    let storage_address = starknet::storage_address_try_from_felt252(0x17).unwrap();
    let ret_data = TestContract::__external::test_storage_address(args.span());

    assert_eq(*args[0_u32], *ret_data[0_u32], 'Unexpected ret_data.');
}

#[derive(starknet::Event, PartialEq, Drop, Clone, Serde)]
struct MyEventStruct {
    x: felt252,
    data: usize,
}

#[derive(starknet::Event, PartialEq, Drop, Clone)]
enum MyEventEnum {
    #[event]
    A: MyEventStruct,
    B: felt252,
}

fn event_serde_tester<
    T,
    impl TEvent: Event<T>,
    impl TClone: Clone<T>,
    impl TPartialEq: PartialEq<T>,
    impl TDrop: Drop<T>
>(
    event: T
) {
    let original_event = event.clone();
    let mut keys = Default::default();
    let mut values = Default::default();
    event.append_keys_and_values(ref keys, ref values);
    let mut keys = keys.span();
    let mut values = values.span();
    let mut event = Event::deserialize(ref keys, ref values).unwrap();
    assert_eq(event, original_event, 'Event deserialization failed');
}

#[test]
fn test_event_serde() {
    let event = MyEventStruct { x: 0x17, data: 2 };
    event_serde_tester(event.clone());
    let event = MyEventEnum::A(event);
    event_serde_tester(event.clone());
}
