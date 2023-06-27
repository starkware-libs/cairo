use core::debug::PrintTrait;
use core::result::ResultTrait;
use core::traits::TryInto;
use array::ArrayTrait;
use starknet::syscalls::{deploy_syscall, replace_class_syscall};
use option::OptionTrait;
use test::test_utils::assert_eq;
<<<<<<< HEAD
use starknet::class_hash::ClassHash;

#[starknet::interface]
trait IWithReplace<TContractState> {
    fn replace(ref self: TContractState, class_hash: ClassHash);
=======

#[starknet::interface]
trait IWithReplace<TContractState> {
    fn replace(ref self: TContractState);
>>>>>>> v2.0.0-rc5
}

#[starknet::contract]
mod ContractA {
    use core::traits::TryInto;
    use option::OptionTrait;
    use core::result::ResultTrait;
<<<<<<< HEAD
    use starknet::class_hash::ClassHash;
=======
>>>>>>> v2.0.0-rc5

    #[storage]
    struct Storage {
        value: u128, 
    }

    #[constructor]
<<<<<<< HEAD
    fn constructor(ref self: ContractState, value: u128) {
        self.value.write(value);
    }

    #[external(v0)]
    impl IWithReplaceImpl of super::IWithReplace<ContractState> {
        fn replace(ref self: ContractState, class_hash: ClassHash) {
            starknet::replace_class_syscall(class_hash).unwrap();
        }
=======
    fn constructor(ref self: ContractState, value_: u128) {
        self.value.write(value_);
    }

    #[external]
    fn replace(ref self: ContractState) {
        starknet::replace_class_syscall(super::ContractB::TEST_CLASS_HASH.try_into().unwrap())
            .unwrap();
>>>>>>> v2.0.0-rc5
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

<<<<<<< HEAD
    #[external(v0)]
=======
    #[external]
>>>>>>> v2.0.0-rc5
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
<<<<<<< HEAD
        class_hash: ContractA::TEST_CLASS_HASH.try_into().unwrap(),
        contract_address_salt: 0,
        calldata: calldata.span(),
        deploy_from_zero: false
=======
        ContractA::TEST_CLASS_HASH.try_into().unwrap(), 0, calldata.span(), false
>>>>>>> v2.0.0-rc5
    )
        .unwrap();

    // Replace its class hash to Class B.
    let mut contract0 = IWithReplaceDispatcher { contract_address: address0 };
<<<<<<< HEAD
    contract0.replace(ContractB::TEST_CLASS_HASH.try_into().unwrap());

    // Read the previously stored value.
    let mut contract1 = IWithFooDispatcher { contract_address: address0 };
    assert_eq(@contract1.foo(), @100, 'contract1.foo() != 100');
=======
    contract0.replace();

    // Read the previously stored value.
    let mut contract1 = IWithFooDispatcher { contract_address: address0 };
    assert_eq(@contract1.foo(), @100, 'contract1.foo() == 100');
>>>>>>> v2.0.0-rc5
}
