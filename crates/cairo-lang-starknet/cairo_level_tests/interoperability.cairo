use core::traits::Into;
use core::result::ResultTrait;
use test::test_utils::{assert_eq, assert_ne};
use starknet::syscalls::{deploy_syscall, get_block_hash_syscall};
use array::ArrayTrait;
use traits::TryInto;
use option::OptionTrait;
use starknet::SyscallResultTrait;
use starknet::class_hash::Felt252TryIntoClassHash;

#[starknet::interface]
trait IContract<T> {
    fn foo(ref self: T, a: u128) -> u128;
}

#[starknet::contract]
mod contract_a {
    use traits::Into;
    use starknet::info::get_contract_address;
    #[storage]
    struct Storage {
        value: u128, 
    }

    #[constructor]
    fn constructor(ref self: ContractState, value: u128) {
        self.value.write(value);
    }

    #[external(v0)]
    fn foo(ref self: ContractState, a: u128) -> u128 {
        let value = self.value.read();
        self.value.write(a);
        value
    }

    #[generate_trait]
    impl MyImpl of MyTrait {
        fn internal_func(self: @ContractState) -> u128 {
            5
        }
    }
}

use contract_a::MyTrait;
#[test]
#[available_gas(30000000)]
fn test_internal_func() {
    let mut contract_state = contract_a::contract_state_for_testing();
    contract_state.internal_func();
}

#[test]
#[available_gas(30000000)]
fn test_flow() {
    // Set up.
    let (address0, _) = deploy_syscall(
        contract_a::TEST_CLASS_HASH.try_into().unwrap(), 0, array![100].span(), false
    )
        .unwrap();
    let mut contract0 = IContractDispatcher { contract_address: address0 };
    let (address1, _) = deploy_syscall(
        contract_a::TEST_CLASS_HASH.try_into().unwrap(), 0, array![200].span(), false
    )
        .unwrap();
    let mut contract1 = IContractDispatcher { contract_address: address1 };

    // Interact.
    assert_eq(@contract0.foo(300), @100, 'contract0.foo(300) == 100');
    assert_eq(@contract1.foo(300), @200, 'contract1.foo(300) == 200');
    assert_eq(@contract0.foo(300), @300, 'contract0.foo(300) == 300');
    assert_eq(@contract1.foo(300), @300, 'contract1.foo(300) == 300');

    // Library calls.
    let mut library = IContractLibraryDispatcher {
        class_hash: contract_a::TEST_CLASS_HASH.try_into().unwrap()
    };
    assert_eq(@library.foo(300), @0, 'library.foo(300) == 0');
}

#[test]
#[available_gas(300000)]
#[should_panic(expected: ('Out of gas', 'ENTRYPOINT_FAILED', ))]
fn test_flow_out_of_gas() {
    // Set up.
    let (address0, _) = deploy_syscall(
        contract_a::TEST_CLASS_HASH.try_into().unwrap(), 0, array![100].span(), false
    )
        .unwrap();
    let mut contract0 = IContractDispatcher { contract_address: address0 };
    let (address1, _) = deploy_syscall(
        contract_a::TEST_CLASS_HASH.try_into().unwrap(), 0, array![200].span(), false
    )
        .unwrap();
    let mut contract1 = IContractDispatcher { contract_address: address1 };

    // Interact.
    assert_eq(@contract0.foo(300), @100, 'contract0.foo(300) == 100');
    assert_eq(@contract1.foo(300), @200, 'contract1.foo(300) == 200');
    assert_eq(@contract0.foo(300), @300, 'contract0.foo(300) == 300');
    assert_eq(@contract1.foo(300), @300, 'contract1.foo(300) == 300');

    // Library calls.
    let mut library = IContractLibraryDispatcher {
        class_hash: contract_a::TEST_CLASS_HASH.try_into().unwrap()
    };
    assert_eq(@library.foo(300), @0, 'library.foo(300) == 0');
}

#[test]
#[available_gas(30000000)]
fn test_class_hash_not_found() {
    let mut err = deploy_syscall(5.try_into().unwrap(), 0, array![100].span(), false).unwrap_err();
    assert_eq(@err.pop_front().unwrap(), @'CLASS_HASH_NOT_FOUND', 'err == "CLASS_HASH_NOT_FOUND"');
}

#[test]
#[available_gas(30000000)]
#[should_panic(expected: ('CONTRACT_NOT_DEPLOYED', ))]
fn test_contract_not_deployed() {
    let mut contract = IContractDispatcher { contract_address: 5.try_into().unwrap() };
    contract.foo(10);
}

#[starknet::contract]
mod contract_failed_constructor {
    #[storage]
    struct Storage {}

    #[constructor]
    fn constructor(ref self: ContractState, value: u128) {
        panic_with_felt252('Failure');
    }
}

#[test]
#[available_gas(30000000)]
fn test_failed_constructor() {
    // Set up.
    let mut calldata = Default::default();
    calldata.append(100);
    let mut err = deploy_syscall(
        contract_failed_constructor::TEST_CLASS_HASH.try_into().unwrap(), 0, calldata.span(), false
    )
        .unwrap_err();
    assert_eq(@err.pop_front().unwrap(), @'Failure', 'err == "Failure"');
    assert_eq(@err.pop_front().unwrap(), @'CONSTRUCTOR_FAILED', 'err == "CONSTRUCTOR_FAILED"');
}

#[starknet::contract]
mod contract_failed_entrypoint {
    #[storage]
    struct Storage {}

    #[external(v0)]
    fn foo(ref self: ContractState, value: u128) {
        panic_with_felt252('Failure');
    }
}

#[test]
#[available_gas(30000000)]
#[should_panic(expected: ('Failure', 'ENTRYPOINT_FAILED', ))]
fn test_entrypoint_failed() {
    let (address0, _) = deploy_syscall(
        contract_failed_entrypoint::TEST_CLASS_HASH.try_into().unwrap(),
        0,
        array![100].span(),
        false
    )
        .unwrap();
    let mut contract = IContractDispatcher { contract_address: address0 };
    contract.foo(300);
}

#[test]
#[available_gas(30000000)]
#[should_panic(expected: ('GET_BLOCK_HASH_UNIMPLEMENTED', ))]
fn test_get_block_hash() {
    get_block_hash_syscall(0).unwrap_syscall();
}
