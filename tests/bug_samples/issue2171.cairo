use core::test::test_utils::assert_eq;
use starknet::ContractAddress;

#[test]
fn main() {
    let contract_address = 0x1000.try_into().unwrap();
    foo(contract_address);
}

fn foo(contract_address: ContractAddress) {
    assert_eq(
        @Into::<ContractAddress, felt252>::into(contract_address),
        @Into::<ContractAddress, felt252>::into(contract_address),
        'Some message',
    );
}
