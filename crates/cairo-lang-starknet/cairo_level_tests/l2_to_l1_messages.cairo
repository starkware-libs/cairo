use array::{ArrayTrait, SpanTrait, SpanPartialEq};

use contract_with_messages_sent_to_l1::IContractWithMessagesSentToL1;
use option::OptionTrait;
use result::ResultTrait;
use starknet::class_hash::Felt252TryIntoClassHash;
use starknet::syscalls::{deploy_syscall, get_block_hash_syscall};
use starknet::{testing, SyscallResultTrait};
use test::test_utils::{assert_eq, assert_ne};
use traits::{Into, TryInto, PartialEq};

#[starknet::contract]
mod contract_with_messages_sent_to_l1 {
    use array::ArrayTrait;

    use super::generate_payload;
    use traits::Into;

    #[storage]
    struct Storage {
        value: u128,
    }

    #[constructor]
    fn constructor(ref self: ContractState) {
        self.value.write(0);
    }

    #[external(v0)]
    #[generate_trait]
    impl IContractWithMessagesSentToL1Impl of IContractWithMessagesSentToL1 {
        fn send_message_to_l1(ref self: ContractState) {
            let value_ = self.value.read();

            starknet::send_message_to_l1_syscall(
                to_address: value_.into(), payload: generate_payload(n: value_).span()
            );
            self.value.write(value_ + 1);
        }
    }
}


// Generates the array [0, 1, 2, ..., n]
fn generate_payload(n: u128) -> Array<felt252> {
    let mut payload = array![];

    let mut i: u128 = 0;
    loop {
        if (i > n) {
            break;
        }

        payload.append(i.into());
        i += 1;
    };

    payload
}

#[test]
#[available_gas(30000000)]
fn test_l2_to_l1_messages() {
    // Set up.
    let mut contract = contract_with_messages_sent_to_l1::unsafe_new_contract_state();
    let contract_address = starknet::contract_address_const::<0x42>();
    let other_contract_address = starknet::contract_address_const::<0xdead>();

    testing::set_contract_address(contract_address);

    // Send messages.
    contract.send_message_to_l1();
    contract.send_message_to_l1();
    contract.send_message_to_l1();

    // Assert other addresses did not sent messages.
    assert(testing::pop_l2_to_l1_message(other_contract_address).is_none(), 'no messages');

    // Pop messages.
    assert_eq(
        @testing::pop_l2_to_l1_message(contract_address).unwrap(),
        @(0, array![0].span()),
        'message == (0, [0])'
    );
    assert_eq(
        @testing::pop_l2_to_l1_message(contract_address).unwrap(),
        @(1, array![0, 1].span()),
        'message == (1, [0, 1])'
    );
    assert_eq(
        @testing::pop_l2_to_l1_message(contract_address).unwrap(),
        @(2, array![0, 1, 2].span()),
        'message == (2, [0, 1, 2])'
    );

    // Assert all messages have been popped.
    assert(testing::pop_l2_to_l1_message(contract_address).is_none(), 'no more messages');
}

#[test]
#[available_gas(300000)]
fn test_pop_l2_to_l1_message() {
    let contract_address = starknet::contract_address_const::<0x42>();
    testing::set_contract_address(contract_address);

    let mut to_address = 1234;
    let mut payload = array![2345];

    starknet::send_message_to_l1_syscall(to_address, payload.span());
    starknet::send_message_to_l1_syscall(to_address, payload.span());

    let (to_address, payload) = starknet::testing::pop_l2_to_l1_message(contract_address).unwrap();
    assert_eq(@payload.len(), @1, 'unexpected payload size');
    assert_eq(@to_address, @1234, 'unexpected to_address');
    assert_eq(payload.at(0), @2345, 'unexpected payload');

    let (to_address, payload) = starknet::testing::pop_l2_to_l1_message(contract_address).unwrap();
    assert_eq(@payload.len(), @1, 'unexpected payload size');
    assert_eq(@to_address, @1234, 'unexpected to_address');
    assert_eq(payload.at(0), @2345, 'unexpected payload');
}
