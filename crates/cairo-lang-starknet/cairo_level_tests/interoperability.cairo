use core::result::ResultTrait;
use starknet::syscalls::deploy_syscall;
use array::ArrayTrait;
use traits::TryInto;
use option::OptionTrait;
use starknet::class_hash::Felt252TryIntoClassHash;

#[abi]
trait IContract {
    fn foo(a: u128) -> u128;
}

#[contract]
mod ContractA {
    use traits::Into;
    use starknet::info::get_contract_address;
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
}

#[test]
#[available_gas(30000000)]
fn test_deploy() {
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
}
