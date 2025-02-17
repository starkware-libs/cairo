use contract_with_messages_sent_to_l1::IContractWithMessagesSentToL1;
use starknet::{SyscallResultTrait, testing};

#[starknet::contract]
mod contract_with_messages_sent_to_l1 {
    use core::array::ArrayTrait;
    use starknet::SyscallResultTrait;
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
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
                to_address: value_.into(), payload: generate_payload(n: value_).span(),
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
    }

    payload
}

#[test]
fn test_l2_to_l1_messages() {
    // Set up.
    let mut contract = contract_with_messages_sent_to_l1::unsafe_new_contract_state();
    const CONTRACT_ADDRESS: starknet::ContractAddress = 0x42_felt252.try_into().unwrap();
    const OTHER_CONTRACT_ADDRESS: starknet::ContractAddress = 0xdead_felt252.try_into().unwrap();

    testing::set_contract_address(CONTRACT_ADDRESS);

    // Send messages.
    contract.send_message_to_l1();
    contract.send_message_to_l1();
    contract.send_message_to_l1();

    // Assert other addresses did not sent messages.
    assert!(testing::pop_l2_to_l1_message(OTHER_CONTRACT_ADDRESS).is_none());

    // Pop messages.
    assert_eq!(testing::pop_l2_to_l1_message(CONTRACT_ADDRESS), Some((0, [0].span())));
    assert_eq!(testing::pop_l2_to_l1_message(CONTRACT_ADDRESS), Some((1, [0, 1].span())));
    assert_eq!(testing::pop_l2_to_l1_message(CONTRACT_ADDRESS), Some((2, [0, 1, 2].span())));

    // Assert all messages have been popped.
    assert!(testing::pop_l2_to_l1_message(CONTRACT_ADDRESS).is_none());
}

#[test]
fn test_pop_l2_to_l1_message() {
    const CONTRACT_ADDRESS: starknet::ContractAddress = 0x42_felt252.try_into().unwrap();
    testing::set_contract_address(CONTRACT_ADDRESS);

    let mut to_address = 1234;
    let mut payload = [2345].span();

    starknet::syscalls::send_message_to_l1_syscall(to_address, payload).unwrap_syscall();
    starknet::syscalls::send_message_to_l1_syscall(to_address, payload).unwrap_syscall();

    assert_eq!(testing::pop_l2_to_l1_message(CONTRACT_ADDRESS), Some((to_address, payload)));
    assert_eq!(testing::pop_l2_to_l1_message(CONTRACT_ADDRESS), Some((to_address, payload)));
}
