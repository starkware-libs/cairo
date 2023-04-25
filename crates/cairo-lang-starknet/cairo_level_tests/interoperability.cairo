use core::traits::Into;
use core::result::ResultTrait;
use starknet::syscalls::deploy_syscall;
use array::ArrayTrait;
use array::SpanTrait;
use traits::TryInto;
use option::OptionTrait;
use starknet::class_hash::Felt252TryIntoClassHash;
use serde::Serde;

#[abi]
trait IContract {
    fn foo(a: u128) -> u128;
    fn bar(a: Span<felt252>) -> Span<felt252>;
}

impl SpanSerde of Serde::<Span<felt252>> {
    fn serialize(ref output: Array<felt252>, mut input: Span<felt252>) {
        gas::withdraw_gas().expect('Out of gas');
        match input.pop_front() {
            Option::Some(v) => {
                output.append(*v);
                SpanSerde::serialize(ref output, input);
            },
            Option::None(_) => {
                return ();
            },
        };
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<Span<felt252>> {
        Option::Some(serialized)
    }
}

#[contract]
mod ContractA {
    use traits::Into;
    use starknet::info::get_contract_address;
    use super::SpanSerde;

    struct Storage {
        value: u128, 
    }

    #[constructor]
    fn constructor(value_: u128) {
        value::write(value_);
    }

    #[external]
    fn foo(a: u128) -> u128 {
        let value = value::read();
        value::write(a);
        value
    }

    #[external]
    fn bar(value: Span<felt252>) -> Span<felt252> {
        value
    }
}

#[test]
#[available_gas(30000000)]
fn test_flow() {
    // Set up.
    let mut calldata = ArrayTrait::new();
    calldata.append(100);
    let (address0, _) = deploy_syscall(
        ContractA::TEST_CLASS_HASH.try_into().unwrap(), 0, calldata.span(), false
    ).unwrap();
    let contract0 = IContractDispatcher { contract_address: address0 };
    let mut calldata = ArrayTrait::new();
    calldata.append(200);
    let (address1, _) = deploy_syscall(
        ContractA::TEST_CLASS_HASH.try_into().unwrap(), 0, calldata.span(), false
    ).unwrap();
    let contract1 = IContractDispatcher { contract_address: address1 };

    // Interact.
    assert(contract0.foo(300) == 100, 'contract0.foo(300) == 100');
    assert(contract1.foo(300) == 200, 'contract1.foo(300) == 200');
    assert(contract0.foo(300) == 300, 'contract0.foo(300) == 300');
    assert(contract1.foo(300) == 300, 'contract1.foo(300) == 300');

    let mut arr = ArrayTrait::new();
    arr.append(42);
    assert(*contract0.bar(arr.span())[0] == 42, 'contract0.bar([42]) == [42]');

    // Library calls.
    let library = IContractLibraryDispatcher {
        class_hash: ContractA::TEST_CLASS_HASH.try_into().unwrap()
    };
    assert(library.foo(300) == 0, 'library.foo(300) == 0');
}

#[test]
#[available_gas(30000000)]
fn test_class_hash_not_found() {
    let mut calldata = ArrayTrait::new();
    calldata.append(100);
    let mut err = deploy_syscall(5.try_into().unwrap(), 0, calldata.span(), false).unwrap_err();
    assert(err.pop_front().unwrap() == 'CLASS_HASH_NOT_FOUND', 'err == "CLASS_HASH_NOT_FOUND"');
}

#[test]
#[available_gas(30000000)]
#[should_panic(expected: ('CONTRACT_NOT_DEPLOYED', ))]
fn test_contract_not_deployed() {
    let contract = IContractDispatcher { contract_address: 5.try_into().unwrap() };
    contract.foo(10);
}

#[contract]
mod ContractFailedConstructor {
    struct Storage {}

    #[constructor]
    fn constructor(value_: u128) {
        panic_with_felt252('Failure');
    }
}

#[test]
#[available_gas(30000000)]
fn test_failed_constructor() {
    // Set up.
    let mut calldata = ArrayTrait::new();
    calldata.append(100);
    let mut err = deploy_syscall(
        ContractFailedConstructor::TEST_CLASS_HASH.try_into().unwrap(), 0, calldata.span(), false
    ).unwrap_err();
    assert(err.pop_front().unwrap() == 'CONSTRUCTOR_FAILED', 'err == "CONSTRUCTOR_FAILED"');
}

#[contract]
mod ContractFailedEntrypoint {
    struct Storage {}

    #[external]
    fn foo(value_: u128) {
        panic_with_felt252('Failure');
    }
}

#[test]
#[available_gas(30000000)]
#[should_panic(expected: ('ENTRYPOINT_FAILED', ))]
fn test_entrypoint_failed() {
    let mut calldata = ArrayTrait::new();
    calldata.append(100);
    let (address0, _) = deploy_syscall(
        ContractFailedEntrypoint::TEST_CLASS_HASH.try_into().unwrap(), 0, calldata.span(), false
    ).unwrap();
    let contract = IContractDispatcher { contract_address: address0 };
    contract.foo(300);
}
