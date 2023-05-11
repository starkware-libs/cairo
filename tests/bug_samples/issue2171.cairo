use traits::Into;
use starknet::ContractAddress;
use test::test_utils::{assert_eq, assert_ne};

fn foo(contract_address: ContractAddress) {
    assert_eq(
        Into::<ContractAddress, felt252>::into(contract_address),
        Into::<ContractAddress, felt252>::into(contract_address),
        'Some message'
    );
}
