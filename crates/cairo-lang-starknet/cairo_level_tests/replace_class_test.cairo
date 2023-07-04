use core::debug::PrintTrait;
use core::result::ResultTrait;
use core::traits::TryInto;
use array::ArrayTrait;
use starknet::syscalls::{deploy_syscall, replace_class_syscall};
use option::OptionTrait;
use test::test_utils::assert_eq;
use starknet::class_hash::ClassHash;

#[starknet::interface]
trait IWithReplace<TContractState> {
    fn replace(ref self: TContractState, class_hash: ClassHash);
}

#[starknet::contract]
mod contract_a {
    use core::traits::TryInto;
    use option::OptionTrait;
    use core::result::ResultTrait;
    use starknet::class_hash::ClassHash;

    #[storage]
    struct Storage {
        value: u128, 
    }

    #[constructor]
    fn constructor(ref self: ContractState, value: u128) {
        self.value.write(value);
    }

    #[external(v0)]
    impl IWithReplaceImpl of super::IWithReplace<ContractState> {
        fn replace(ref self: ContractState, class_hash: ClassHash) {
            starknet::replace_class_syscall(class_hash).unwrap();
        }
    }
}

#[starknet::interface]
trait IWithFoo<TContractState> {
    fn foo(ref self: TContractState) -> u128;
}

#[starknet::contract]
mod contract_b {
    #[storage]
    struct Storage {
        value: u128, 
    }

    #[external(v0)]
    fn foo(ref self: ContractState) -> u128 {
        self.value.read()
    }
}

#[test]
#[available_gas(30000000)]
fn test_replace_flow() {
    // Deploy ContractA with 100 in the storage.
    let (address0, _) = deploy_syscall(
        class_hash: contract_a::TEST_CLASS_HASH.try_into().unwrap(),
        contract_address_salt: 0,
        calldata: array![100].span(),
        deploy_from_zero: false
    )
        .unwrap();

    // Replace its class hash to Class B.
    let mut contract0 = IWithReplaceDispatcher { contract_address: address0 };
    contract0.replace(contract_b::TEST_CLASS_HASH.try_into().unwrap());

    // Read the previously stored value.
    let mut contract1 = IWithFooDispatcher { contract_address: address0 };
    assert_eq(@contract1.foo(), @100, 'contract1.foo() != 100');
}
