use core::num::traits::Zero;
use super::utils::serialized;

#[starknet::interface]
trait ITestContract<T> {}

#[starknet::contract]
mod test_contract {
    use starknet::StorageAddress;
    use starknet::storage::{
        Map, StorageMapReadAccess, StorageMapWriteAccess, StoragePointerReadAccess,
        StoragePointerWriteAccess,
    };

    #[storage]
    struct Storage {
        value: felt252,
        mapping: Map<u128, bool>,
        large_mapping: Map<u256, u256>,
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
    fn get_value(self: @ContractState) -> felt252 {
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
        self: @ContractState, storage_address: StorageAddress,
    ) -> StorageAddress {
        storage_address
    }
}

#[test]
#[should_panic]
fn test_wrapper_not_enough_args() {
    test_contract::__external::get_plus_2(serialized(()));
}

#[test]
#[should_panic]
fn test_wrapper_too_many_args() {
    test_contract::__external::get_plus_2(serialized((1, 2)));
}

#[test]
fn test_wrapper_valid_args() {
    assert_eq!(test_contract::__external::get_plus_2(serialized(1)), serialized(3));
}

#[test]
#[available_gas(20000)]
#[should_panic]
fn test_wrapper_valid_args_out_of_gas() {
    test_contract::__external::spend_all_gas(serialized(()));
}

#[test]
fn test_wrapper_array_arg_and_output() {
    assert_eq!(
        test_contract::__external::get_appended_array(serialized(array![2])),
        serialized(array![2, 1]),
    );
}

#[test]
fn read_first_value() {
    assert_eq!(test_contract::__external::get_value(serialized(())), serialized(0));
}

#[test]
fn write_read_value() {
    assert(test_contract::__external::set_value(serialized(4)).is_empty(), 'Not empty');
    assert_eq!(test_contract::__external::get_value(serialized(())), serialized(4));
}

#[test]
fn empty_start() {
    assert_eq!(test_contract::__external::contains(serialized(4)), serialized(0));
}

#[test]
fn contains_added() {
    assert(test_contract::__external::insert(serialized(4)).is_empty(), 'Not empty');
    assert_eq!(test_contract::__external::contains(serialized(4)), serialized(1));
    assert_eq!(test_contract::__external::contains(serialized(5)), serialized(0));
}

#[test]
fn not_contains_removed() {
    assert(test_contract::__external::insert(serialized(4)).is_empty(), 'Not empty');
    assert(test_contract::__external::remove(serialized(4)).is_empty(), 'Not empty');
    assert_eq!(test_contract::__external::contains(serialized(4)), serialized(0));
}

#[test]
fn read_large_first_value() {
    assert_eq!(
        test_contract::__external::get_large(serialized(0x200000000000000000000000000000001_u256)),
        serialized(0_u256),
    );
}

#[test]
fn write_read_large_value() {
    assert(
        test_contract::__external::set_large(
            serialized(
                (
                    0x200000000000000000000000000000001_u256,
                    0x400000000000000000000000000000003_u256,
                ),
            ),
        )
            .is_empty(),
        'Array not empty',
    );
    assert_eq!(
        test_contract::__external::get_large(serialized(0x200000000000000000000000000000001_u256)),
        serialized(0x400000000000000000000000000000003_u256),
    );
}

#[test]
fn test_get_block_info() {
    let info = starknet::get_block_info().unbox();
    assert_eq!(info.block_number, 0_u64);
    assert_eq!(info.block_timestamp, 0_u64);
    assert(info.sequencer_address.is_zero(), 'non default sequencer_address');
    starknet::testing::set_block_number(1_u64);
    starknet::testing::set_block_timestamp(2_u64);
    starknet::testing::set_sequencer_address(3_felt252.try_into().unwrap());
    let info = starknet::get_block_info().unbox();
    assert_eq!(info.block_number, 1_u64);
    assert_eq!(info.block_timestamp, 2_u64);
    assert_eq!(info.sequencer_address.into(), 3);
}

#[test]
fn test_get_caller_address() {
    assert(starknet::get_caller_address().is_zero(), 'non default value');
    starknet::testing::set_caller_address(1_felt252.try_into().unwrap());
    assert_eq!(starknet::get_caller_address().into(), 1);
}

#[test]
fn test_get_contract_address() {
    assert(starknet::get_contract_address().is_zero(), 'non default value');
    starknet::testing::set_contract_address(1_felt252.try_into().unwrap());
    assert_eq!(starknet::get_contract_address().into(), 1);
}

#[test]
fn test_get_version() {
    assert(starknet::get_tx_info().unbox().version.is_zero(), 'non default value');
    starknet::testing::set_version(1);
    assert_eq!(starknet::get_tx_info().unbox().version, 1_felt252);
}

#[test]
fn test_get_account_contract_address() {
    assert(starknet::get_tx_info().unbox().account_contract_address.is_zero(), 'non default value');
    starknet::testing::set_account_contract_address(1_felt252.try_into().unwrap());
    assert_eq!(starknet::get_tx_info().unbox().account_contract_address.into(), 1);
}

#[test]
fn test_get_max_fee() {
    assert_eq!(starknet::get_tx_info().unbox().max_fee, 0_u128);
    starknet::testing::set_max_fee(1);
    assert_eq!(starknet::get_tx_info().unbox().max_fee, 1_u128);
}

#[test]
fn test_get_transaction_hash() {
    assert(starknet::get_tx_info().unbox().transaction_hash.is_zero(), 'non default value');
    starknet::testing::set_transaction_hash(1);
    assert_eq!(starknet::get_tx_info().unbox().transaction_hash, 1_felt252);
}

#[test]
fn test_get_chain_id() {
    assert(starknet::get_tx_info().unbox().chain_id.is_zero(), 'non default value');
    starknet::testing::set_chain_id(1);
    assert_eq!(starknet::get_tx_info().unbox().chain_id, 1_felt252);
}

#[test]
fn test_get_nonce() {
    assert(starknet::get_tx_info().unbox().nonce.is_zero(), 'non default value');
    starknet::testing::set_nonce(1);
    assert_eq!(starknet::get_tx_info().unbox().nonce, 1_felt252);
}

#[test]
fn test_get_signature() {
    assert(starknet::get_tx_info().unbox().signature.is_empty(), 'non default value');
    starknet::testing::set_signature(array!['some', 'signature'].span());
    let read_signature = starknet::get_tx_info().unbox().signature;
    assert_eq!(read_signature.len(), 2);
    assert_eq!(read_signature.at(0), @'some');
    assert_eq!(read_signature.at(1), @'signature');
}

#[test]
fn test_get_block_hash() {
    assert!(
        starknet::syscalls::get_block_hash_syscall(1337) == Err(array!['GET_BLOCK_HASH_NOT_SET']),
    );
    starknet::testing::set_block_hash(1337, 'some-value');
    assert!(starknet::syscalls::get_block_hash_syscall(1337) == Ok('some-value'));
    assert!(
        starknet::syscalls::get_block_hash_syscall(1338) == Err(array!['GET_BLOCK_HASH_NOT_SET']),
    );
}

#[test]
#[should_panic]
fn test_pop_log_empty_logs() {
    starknet::testing::pop_log_raw(0x1234_felt252.try_into().unwrap()).unwrap();
}

#[test]
#[should_panic]
fn test_pop_l2_to_l1_message_empty_messages() {
    starknet::testing::pop_l2_to_l1_message(0x1234_felt252.try_into().unwrap()).unwrap();
}

#[test]
#[should_panic]
fn test_out_of_range_storage_address_from_felt252() -> starknet::StorageAddress {
    (-1).try_into().unwrap()
}

#[test]
fn test_storage_address() {
    let mut args = array![0x17];
    let _storage_address: starknet::StorageAddress = 0x17.try_into().unwrap();
    let ret_data = test_contract::__external::test_storage_address(args.span());

    assert_eq!(args[0_u32], ret_data[0_u32]);
}

#[derive(starknet::Event, PartialEq, Drop, Clone, Serde, Debug)]
struct A {
    x: felt252,
    #[key]
    data: usize,
}

#[derive(starknet::Event, PartialEq, Drop, Clone, Serde, Debug)]
struct B {
    x: felt252,
}

#[derive(starknet::Event, PartialEq, Drop, Clone, Debug)]
enum MyEventEnum {
    A: A,
    B: B,
}

fn event_serde_tester<
    T, +starknet::Event<T>, +Clone<T>, +PartialEq<T>, +Drop<T>, +core::fmt::Debug<T>,
>(
    event: T,
) {
    let original_event = event.clone();
    let mut keys = Default::default();
    let mut data = Default::default();
    event.append_keys_and_data(ref keys, ref data);
    let mut keys = keys.span();
    let mut data = data.span();
    let mut event = starknet::Event::deserialize(ref keys, ref data).unwrap();
    assert_eq!(event, original_event);
}

#[test]
fn test_event_serde() {
    let event = A { x: 0x17, data: 2 };
    event_serde_tester(event.clone());
    let event = MyEventEnum::A(event);
    event_serde_tester(event.clone());
}

#[test]
fn test_dispatcher_serde() {
    // Contract Dispatcher
    const CONTRACT_ADDRESS: starknet::ContractAddress = 0x1234_felt252.try_into().unwrap();
    let contract0 = ITestContractDispatcher { contract_address: CONTRACT_ADDRESS };

    // Serialize
    let mut calldata = Default::default();
    Serde::serialize(@contract0, ref calldata);
    assert!(calldata == array![CONTRACT_ADDRESS.into()]);

    // Deserialize
    let mut serialized = calldata.span();
    let contract0: ITestContractDispatcher = Serde::deserialize(ref serialized).unwrap();
    assert(contract0.contract_address == CONTRACT_ADDRESS, 'Deserialize to Dispatcher');

    // Library Dispatcher
    let contract1 = ITestContractLibraryDispatcher { class_hash: test_contract::TEST_CLASS_HASH };

    // Serialize
    let mut calldata = Default::default();
    Serde::serialize(@contract1, ref calldata);
    assert!(calldata == array![test_contract::TEST_CLASS_HASH.into()]);

    // Deserialize
    let mut serialized = calldata.span();
    let contract1: ITestContractLibraryDispatcher = Serde::deserialize(ref serialized).unwrap();
    assert!(contract1.class_hash == test_contract::TEST_CLASS_HASH);
}
