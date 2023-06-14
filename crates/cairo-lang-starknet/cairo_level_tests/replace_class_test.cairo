use core::debug::PrintTrait;
use core::result::ResultTrait;
use core::traits::TryInto;
use array::ArrayTrait;
use starknet::syscalls::{deploy_syscall, replace_class_syscall};
use option::OptionTrait;
use test::test_utils::assert_eq;

#[starknet::interface]
trait IWithReplace<TContractState> {
    fn replace(ref self: TContractState);
}

#[starknet::contract]
mod ContractA {
    use core::traits::TryInto;
    use option::OptionTrait;
    use core::result::ResultTrait;

    #[storage]
    struct Storage {
        value: u128, 
    }

    #[constructor]
    fn constructor(ref self: ContractState, value_: u128) {
        self.value.write(value_);
    }

    #[external]
    fn replace(ref self: ContractState) {
        starknet::replace_class_syscall(super::ContractB::TEST_CLASS_HASH.try_into().unwrap())
            .unwrap();
    }
}

#[starknet::interface]
trait IWithFoo<TContractState> {
    fn foo(ref self: TContractState) -> u128;
}

#[starknet::contract]
mod ContractB {
    #[storage]
    struct Storage {
        value: u128, 
    }

    #[external]
    fn foo(ref self: ContractState) -> u128 {
        self.value.read()
    }
}

#[test]
#[available_gas(30000000)]
fn test_replace_flow() {
    // Deploy ContractA with 100 in the storage.
    let mut calldata = Default::default();
    calldata.append(100);
    let (address0, _) = deploy_syscall(
        ContractA::TEST_CLASS_HASH.try_into().unwrap(), 0, calldata.span(), false
    )
        .unwrap();

    // Replace its class hash to Class B.
    let mut contract0 = IWithReplaceDispatcher { contract_address: address0 };
    contract0.replace();

    // Read the previously stored value.
    let mut contract1 = IWithFooDispatcher { contract_address: address0 };
    assert_eq(@contract1.foo(), @100, 'contract1.foo() == 100');
}
