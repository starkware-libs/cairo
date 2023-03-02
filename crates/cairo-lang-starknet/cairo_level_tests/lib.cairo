use array::ArrayTrait;
use array::SpanTrait;
use option::OptionTrait;
use starknet::ContractAddressZeroable;
use starknet::ContractAddressIntoFelt;
use traits::Into;
use zeroable::Zeroable;

#[contract]
mod TestContract {
    use array::ArrayTrait;
    use traits::Into;

    struct Storage {
        value: felt,
        mapping: LegacyMap::<u128, bool>,
        large_mapping: LegacyMap::<u256, u256>,
    }

    #[view]
    fn get_plus_2(a: felt) -> felt {
        a + 2
    }

    #[view]
    fn get_appended_array(mut arr: Array::<felt>) -> Array::<felt> {
        let elem = arr.len().into();
        arr.append(elem);
        arr
    }

    #[external]
    fn set_value(a: felt) {
        value::write(a);
    }

    #[view]
    fn get_value() -> felt {
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
}

#[test]
#[should_panic]
fn test_wrapper_not_enough_args() {
    TestContract::__external::get_plus_2(ArrayTrait::new().span());
}

#[test]
#[should_panic]
fn test_wrapper_too_many_enough_args() {
    let mut calldata = ArrayTrait::new();
    calldata.append(1);
    calldata.append(2);
    TestContract::__external::get_plus_2(calldata.span());
}

fn single_felt_input(value: felt) -> Span::<felt> {
    let mut arr = ArrayTrait::new();
    serde::Serde::serialize(ref arr, value);
    arr.span()
}

fn single_u256_input(value: u256) -> Span::<felt> {
    let mut arr = ArrayTrait::new();
    serde::Serde::serialize(ref arr, value);
    arr.span()
}

fn pop_felt(ref data: Span::<felt>) -> felt {
    serde::Serde::deserialize(ref data).expect('missing data')
}

fn pop_u256(ref data: Span::<felt>) -> u256 {
    serde::Serde::deserialize(ref data).expect('missing data')
}

#[test]
#[available_gas(20000)]
fn test_wrapper_valid_args() {
    let mut retdata = TestContract::__external::get_plus_2(single_felt_input(1)).span();
    assert(pop_felt(ref retdata) == 3, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(5000)]
#[should_panic]
fn test_wrapper_valid_args_out_of_gas() {
    TestContract::__external::get_plus_2(single_felt_input(1));
}

#[test]
#[available_gas(200000)]
fn test_wrapper_array_arg_and_output() {
    let mut calldata = ArrayTrait::new();
    calldata.append(1);
    calldata.append(2);
    let mut retdata = TestContract::__external::get_appended_array(calldata.span()).span();
    assert(pop_felt(ref retdata) == 2, 'Wrong length');
    assert(pop_felt(ref retdata) == 2, 'Wrong original value');
    assert(pop_felt(ref retdata) == 1, 'Wrong added value');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(200000)]
fn read_first_value() {
    let mut retdata = TestContract::__external::get_value(ArrayTrait::new().span()).span();
    assert(pop_felt(ref retdata) == 0, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(300000)]
fn write_read_value() {
    assert(TestContract::__external::set_value(single_felt_input(4)).is_empty(), 'Not empty');
    let mut retdata = TestContract::__external::get_value(ArrayTrait::new().span()).span();
    assert(pop_felt(ref retdata) == 4, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(200000)]
fn empty_start() {
    let mut retdata = TestContract::__external::contains(single_felt_input(4)).span();
    assert(pop_felt(ref retdata) == 0, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(300000)]
fn contains_added() {
    assert(TestContract::__external::insert(single_felt_input(4)).is_empty(), 'Not empty');
    let mut retdata = TestContract::__external::contains(single_felt_input(4)).span();
    assert(pop_felt(ref retdata) == 1, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
    let mut retdata = TestContract::__external::contains(single_felt_input(5)).span();
    assert(pop_felt(ref retdata) == 0, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(300000)]
fn not_contains_removed() {
    assert(TestContract::__external::insert(single_felt_input(4)).is_empty(), 'Not empty');
    assert(TestContract::__external::remove(single_felt_input(4)).is_empty(), 'Not empty');
    let mut retdata = TestContract::__external::contains(single_felt_input(4)).span();
    assert(pop_felt(ref retdata) == 0, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(300000)]
fn read_large_first_value() {
    let mut retdata = TestContract::__external::get_large(
        single_u256_input(u256 { low: 1_u128, high: 2_u128 })
    ).span();
    assert(pop_u256(ref retdata) == u256 { low: 0_u128, high: 0_u128 }, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(300000)]
fn write_read_large_value() {
    let mut args = ArrayTrait::new();
    serde::Serde::serialize(ref args, u256 { low: 1_u128, high: 2_u128 });
    serde::Serde::serialize(ref args, u256 { low: 3_u128, high: 4_u128 });
    let mut retdata = TestContract::__external::set_large(args.span());
    assert(retdata.is_empty(), 'Array not empty');
    let mut retdata = TestContract::__external::get_large(
        single_u256_input(u256 { low: 1_u128, high: 2_u128 })
    ).span();
    assert(pop_u256(ref retdata) == u256 { low: 3_u128, high: 4_u128 }, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(300000)]
fn test_get_block_info() {
    let info = unbox(starknet::get_block_info());
    assert(info.block_number == 0_u64, 'non default block_number');
    assert(info.block_timestamp == 0_u64, 'non default block_timestamp');
    assert(info.sequencer_address.is_zero(), 'non default sequencer_address');
    starknet_testing::set_block_number(1_u64);
    starknet_testing::set_block_timestamp(2_u64);
    starknet_testing::set_sequencer_address(starknet::contract_address_const::<3>());
    let info = unbox(starknet::get_block_info());
    assert(info.block_number == 1_u64, 'block_number not set');
    assert(info.block_timestamp == 2_u64, 'block_timestamp not set');
    assert(info.sequencer_address.into() == 3, 'sequencer_address not set');
}

#[test]
#[available_gas(300000)]
fn test_get_caller_address() {
    assert(starknet::get_caller_address().is_zero(), 'non default value');
    starknet_testing::set_caller_address(starknet::contract_address_const::<1>());
    assert(starknet::get_caller_address().into() == 1, 'not set value');
}

#[test]
#[available_gas(300000)]
fn test_get_contract_address() {
    assert(starknet::get_contract_address().is_zero(), 'non default value');
    starknet_testing::set_contract_address(starknet::contract_address_const::<1>());
    assert(starknet::get_contract_address().into() == 1, 'not set value');
}

#[test]
#[should_panic]
fn test_out_of_range_storage_address_from_felt() -> starknet::StorageAddress {
    starknet::storage_address_try_from_felt(-1).unwrap()
}
