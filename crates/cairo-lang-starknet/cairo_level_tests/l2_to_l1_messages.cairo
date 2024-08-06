use starknet::{testing, SyscallResultTrait};

use contract_with_messages_sent_to_l1::IContractWithMessagesSentToL1;

#[starknet::contract]
mod contract_with_messages_sent_to_l1 {
    use starknet::SyscallResultTrait;
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use core::array::ArrayTrait;

    use super::generate_payload;

    #[storage]
    struct Storage {
        value: u128,
    }

    #[constructor]
    fn constructor(ref self: ContractState) {
        self.value.write(0);
    }

    #[abi(per_item)]
    #[generate_trait]
    pub impl IContractWithMessagesSentToL1Impl of IContractWithMessagesSentToL1 {
        #[external(v0)]
        fn send_message_to_l1(ref self: ContractState) {
            let value_ = self.value.read();

            starknet::syscalls::send_message_to_l1_syscall(
                to_address: value_.into(), payload: generate_payload(n: value_).span()
            )
                .unwrap_syscall();
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
    assert!(testing::pop_l2_to_l1_message(other_contract_address).is_none());

    // Pop messages.
    assert_eq!(testing::pop_l2_to_l1_message(contract_address), Option::Some((0, [0].span())));
    assert_eq!(testing::pop_l2_to_l1_message(contract_address), Option::Some((1, [0, 1].span())));
    assert_eq!(
        testing::pop_l2_to_l1_message(contract_address), Option::Some((2, [0, 1, 2].span()))
    );

    // Assert all messages have been popped.
    assert!(testing::pop_l2_to_l1_message(contract_address).is_none());
}

#[test]
fn test_pop_l2_to_l1_message() {
    let contract_address = starknet::contract_address_const::<0x42>();
    testing::set_contract_address(contract_address);

    let mut to_address = 1234;
    let mut payload = [2345].span();

    starknet::syscalls::send_message_to_l1_syscall(to_address, payload).unwrap_syscall();
    starknet::syscalls::send_message_to_l1_syscall(to_address, payload).unwrap_syscall();

    assert_eq!(
        testing::pop_l2_to_l1_message(contract_address), Option::Some((to_address, payload))
    );
    assert_eq!(
        testing::pop_l2_to_l1_message(contract_address), Option::Some((to_address, payload))
    );
}
