use starknet::StorageAddress;
use test::test_utils::{assert_eq, assert_ne};

use super::utils::serialized_element;
use super::utils::single_deserialize;

#[starknet::interface]
trait ITestContract {}

#[starknet::contract]
mod test_contract {
    use starknet::StorageAddress;

    #[storage]
    struct Storage {
        value: felt252,
        mapping: LegacyMap::<u128, bool>,
        large_mapping: LegacyMap::<u256, u256>,
    }

    #[external(v0)]
    fn get_plus_2(self: @ContractState, a: felt252) -> felt252 {
        a + 2
    }

    #[external(v0)]
    fn spend_all_gas(self: @ContractState) {
        spend_all_gas(self);
    }

    #[external(v0)]
    fn get_appended_array(self: @ContractState, mut arr: Array<felt252>) -> Array<felt252> {
        let elem = arr.len().into();
        arr.append(elem);
        arr
    }

    #[external(v0)]
    fn set_value(ref self: ContractState, a: felt252) {
        self.value.write(a);
    }

    #[external(v0)]
    fn get_value(self: @ContractState,) -> felt252 {
        self.value.read()
    }

    #[external(v0)]
    fn insert(ref self: ContractState, key: u128) {
        self.mapping.write(key, true)
    }

    #[external(v0)]
    fn remove(ref self: ContractState, key: u128) {
        self.mapping.write(key, false)
    }

    #[external(v0)]
    fn contains(self: @ContractState, key: u128) -> bool {
        self.mapping.read(key)
    }

    #[external(v0)]
    fn set_large(ref self: ContractState, key: u256, value: u256) {
        self.large_mapping.write(key, value)
    }

    #[external(v0)]
    fn get_large(self: @ContractState, key: u256) -> u256 {
        self.large_mapping.read(key)
    }

    #[external(v0)]
    fn test_storage_address(
        self: @ContractState, storage_address: StorageAddress
    ) -> StorageAddress {
        storage_address
    }
}

#[test]
#[should_panic]
fn test_wrapper_not_enough_args() {
    test_contract::__external::get_plus_2(Default::default().span());
}

#[test]
#[should_panic]
fn test_wrapper_too_many_enough_args() {
    test_contract::__external::get_plus_2(array![1, 2].span());
}

#[test]
#[available_gas(30000)]
fn test_wrapper_valid_args() {
    let mut retdata = test_contract::__external::get_plus_2(serialized_element(1));
    assert_eq(@single_deserialize(ref retdata), @3, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(20000)]
#[should_panic]
fn test_wrapper_valid_args_out_of_gas() {
    test_contract::__external::spend_all_gas(Default::default().span());
}

#[test]
#[available_gas(200000)]
fn test_wrapper_array_arg_and_output() {
    let mut retdata = test_contract::__external::get_appended_array(array![1, 2].span());
    assert_eq(@single_deserialize(ref retdata), @2, 'Wrong length');
    assert_eq(@single_deserialize(ref retdata), @2, 'Wrong original value');
    assert_eq(@single_deserialize(ref retdata), @1, 'Wrong added value');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(200000)]
fn read_first_value() {
    let mut retdata = test_contract::__external::get_value(ArrayTrait::new().span());
    assert_eq(@single_deserialize(ref retdata), @0, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(300000)]
fn write_read_value() {
    assert(test_contract::__external::set_value(serialized_element(4)).is_empty(), 'Not empty');
    let mut retdata = test_contract::__external::get_value(ArrayTrait::new().span());
    assert_eq(@single_deserialize(ref retdata), @4, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(200000)]
fn empty_start() {
    let mut retdata = test_contract::__external::contains(serialized_element(4));
    assert_eq(@single_deserialize(ref retdata), @0, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(300000)]
fn contains_added() {
    assert(test_contract::__external::insert(serialized_element(4)).is_empty(), 'Not empty');
    let mut retdata = test_contract::__external::contains(serialized_element(4));
    assert_eq(@single_deserialize(ref retdata), @1, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
    let mut retdata = test_contract::__external::contains(serialized_element(5));
    assert_eq(@single_deserialize(ref retdata), @0, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(300000)]
fn not_contains_removed() {
    assert(test_contract::__external::insert(serialized_element(4)).is_empty(), 'Not empty');
    assert(test_contract::__external::remove(serialized_element(4)).is_empty(), 'Not empty');
    let mut retdata = test_contract::__external::contains(serialized_element(4));
    assert_eq(@single_deserialize(ref retdata), @0, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(300000)]
fn read_large_first_value() {
    let mut retdata = test_contract::__external::get_large(
        serialized_element(u256 { low: 1_u128, high: 2_u128 })
    );
    assert_eq(
        @single_deserialize(ref retdata), @u256 { low: 0_u128, high: 0_u128 }, 'Wrong result'
    );
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(300000)]
fn write_read_large_value() {
    let mut args = Default::default();
    serde::Serde::serialize(@u256 { low: 1_u128, high: 2_u128 }, ref args);
    serde::Serde::serialize(@u256 { low: 3_u128, high: 4_u128 }, ref args);
    let mut retdata = test_contract::__external::set_large(args.span());
    assert(retdata.is_empty(), 'Array not empty');
    let mut retdata = test_contract::__external::get_large(
        serialized_element(u256 { low: 1_u128, high: 2_u128 })
    );
    assert_eq(
        @single_deserialize(ref retdata), @u256 { low: 3_u128, high: 4_u128 }, 'Wrong result'
    );
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(300000)]
fn test_get_block_info() {
    let info = starknet::get_block_info().unbox();
    assert_eq(@info.block_number, @0_u64, 'non default block_number');
    assert_eq(@info.block_timestamp, @0_u64, 'non default block_timestamp');
    assert(info.sequencer_address.is_zero(), 'non default sequencer_address');
    starknet::testing::set_block_number(1_u64);
    starknet::testing::set_block_timestamp(2_u64);
    starknet::testing::set_sequencer_address(starknet::contract_address_const::<3>());
    let info = starknet::get_block_info().unbox();
    assert_eq(@info.block_number, @1_u64, 'block_number not set');
    assert_eq(@info.block_timestamp, @2_u64, 'block_timestamp not set');
    assert_eq(@info.sequencer_address.into(), @3, 'sequencer_address not set');
}

#[test]
#[available_gas(300000)]
fn test_get_caller_address() {
    assert(starknet::get_caller_address().is_zero(), 'non default value');
    starknet::testing::set_caller_address(starknet::contract_address_const::<1>());
    assert_eq(@starknet::get_caller_address().into(), @1, 'not set value');
}

#[test]
#[available_gas(300000)]
fn test_get_contract_address() {
    assert(starknet::get_contract_address().is_zero(), 'non default value');
    starknet::testing::set_contract_address(starknet::contract_address_const::<1>());
    assert_eq(@starknet::get_contract_address().into(), @1, 'not set value');
}

#[test]
#[available_gas(300000)]
fn test_get_version() {
    assert(starknet::get_tx_info().unbox().version.is_zero(), 'non default value');
    starknet::testing::set_version(1);
    assert_eq(@starknet::get_tx_info().unbox().version, @1_felt252, 'not set value');
}

#[test]
#[available_gas(300000)]
fn test_get_account_contract_address() {
    assert(starknet::get_tx_info().unbox().account_contract_address.is_zero(), 'non default value');
    starknet::testing::set_account_contract_address(starknet::contract_address_const::<1>());
    assert_eq(
        @starknet::get_tx_info().unbox().account_contract_address.into(), @1, 'not set value'
    );
}

#[test]
#[available_gas(300000)]
fn test_get_max_fee() {
    assert_eq(@starknet::get_tx_info().unbox().max_fee, @0_u128, 'non default value');
    starknet::testing::set_max_fee(1);
    assert_eq(@starknet::get_tx_info().unbox().max_fee, @1_u128, 'not set value');
}

#[test]
#[available_gas(300000)]
fn test_get_transaction_hash() {
    assert(starknet::get_tx_info().unbox().transaction_hash.is_zero(), 'non default value');
    starknet::testing::set_transaction_hash(1);
    assert_eq(@starknet::get_tx_info().unbox().transaction_hash, @1_felt252, 'not set value');
}

#[test]
#[available_gas(300000)]
fn test_get_chain_id() {
    assert(starknet::get_tx_info().unbox().chain_id.is_zero(), 'non default value');
    starknet::testing::set_chain_id(1);
    assert_eq(@starknet::get_tx_info().unbox().chain_id, @1_felt252, 'not set value');
}

#[test]
#[available_gas(300000)]
fn test_get_nonce() {
    assert(starknet::get_tx_info().unbox().nonce.is_zero(), 'non default value');
    starknet::testing::set_nonce(1);
    assert_eq(@starknet::get_tx_info().unbox().nonce, @1_felt252, 'not set value');
}

#[test]
#[available_gas(300000)]
fn test_get_signature() {
    assert(starknet::get_tx_info().unbox().signature.is_empty(), 'non default value');
    starknet::testing::set_signature(array!['some', 'signature'].span());
    let read_signature = starknet::get_tx_info().unbox().signature;
    assert_eq(@read_signature.len(), @2, 'unexpected read size');
    assert_eq(read_signature.at(0), @'some', 'unexpected element 0');
    assert_eq(read_signature.at(1), @'signature', 'unexpected element 1');
}

#[test]
#[available_gas(300000)]
#[should_panic]
fn test_pop_log_empty_logs() {
    let contract_address = starknet::contract_address_const::<0x1234>();
    starknet::testing::pop_log_raw(contract_address).unwrap();
}

#[test]
#[available_gas(300000)]
#[should_panic]
fn test_pop_l2_to_l1_message_empty_messages() {
    let contract_address = starknet::contract_address_const::<0x1234>();
    starknet::testing::pop_l2_to_l1_message(contract_address).unwrap();
}

#[test]
#[should_panic]
fn test_out_of_range_storage_address_from_felt252() -> starknet::StorageAddress {
    starknet::storage_address_try_from_felt252(-1).unwrap()
}

#[test]
#[available_gas(300000)]
fn test_storage_address() {
    let mut args = array![0x17];
    let storage_address = starknet::storage_address_try_from_felt252(0x17).unwrap();
    let ret_data = test_contract::__external::test_storage_address(args.span());

    assert_eq(args[0_u32], ret_data[0_u32], 'Unexpected ret_data.');
}

#[derive(starknet::Event, PartialEq, Drop, Clone, Serde)]
struct A {
    x: felt252,
    #[key]
    data: usize,
}

#[derive(starknet::Event, PartialEq, Drop, Clone, Serde)]
struct B {
    x: felt252,
}

#[derive(starknet::Event, PartialEq, Drop, Clone)]
enum MyEventEnum {
    A: A,
    B: B,
}

fn event_serde_tester<
    T,
    impl TEvent: starknet::Event<T>,
    impl TClone: Clone<T>,
    impl TPartialEq: PartialEq<T>,
    impl TDrop: Drop<T>
>(
    event: T
) {
    let original_event = event.clone();
    let mut keys = Default::default();
    let mut data = Default::default();
    event.append_keys_and_data(ref keys, ref data);
    let mut keys = keys.span();
    let mut data = data.span();
    let mut event = starknet::Event::deserialize(ref keys, ref data).unwrap();
    assert_eq(@event, @original_event, 'Event deserialization failed');
}

#[test]
fn test_event_serde() {
    let event = A { x: 0x17, data: 2 };
    event_serde_tester(event.clone());
    let event = MyEventEnum::A(event);
    event_serde_tester(event.clone());
}

#[test]
#[available_gas(30000000)]
fn test_dispatcher_serde() {
    // Contract Dispatcher
    let contract_address = starknet::contract_address_const::<123>();
    let contract0 = ITestContractDispatcher { contract_address };

    // Serialize
    let mut calldata = Default::default();
    serde::Serde::serialize(@contract0, ref calldata);
    let mut calldata_span = calldata.span();
    assert(
        calldata_span.len() == 1 || *calldata_span.pop_front().unwrap() == contract_address.into(),
        'Serialize to 0'
    );

    // Deserialize
    let mut serialized = calldata.span();
    let contract0: ITestContractDispatcher = serde::Serde::deserialize(ref serialized).unwrap();
    assert(contract0.contract_address == contract_address, 'Deserialize to Dispatcher');

    // Library Dispatcher
    let class_hash = test_contract::TEST_CLASS_HASH.try_into().unwrap();
    let contract1 = ITestContractLibraryDispatcher { class_hash };

    // Serialize
    let mut calldata = Default::default();
    serde::Serde::serialize(@contract1, ref calldata);
    let mut calldata_span = calldata.span();
    assert(
        calldata_span.len() == 1 || *calldata_span.pop_front().unwrap() == class_hash.into(),
        'Serialize to class_hash'
    );

    // Deserialize
    let mut serialized = calldata.span();
    let contract1: ITestContractLibraryDispatcher = serde::Serde::deserialize(ref serialized)
        .unwrap();
    assert(contract1.class_hash == class_hash, 'Deserialize to Dispatcher');
}
