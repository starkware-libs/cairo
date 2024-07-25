use core::starknet::storage::{StoragePointerWriteAccess, StoragePointerReadAccess};

#[starknet::storage_node]
struct Struct2 {
    pub member2: felt252,
}

#[starknet::storage_node]
struct Struct1 {
    pub member1: Struct2,
}

#[starknet::contract]
mod contract_v0 {
    use core::starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use super::Struct1;
    #[storage]
    pub struct Storage {
        pub member: Struct1,
    }
}

#[starknet::storage_node]
struct Struct0 {
    #[flat]
    pub member0: Struct1,
}

#[starknet::contract]
mod contract_new {
    use core::starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use super::Struct0;
    #[storage]
    pub struct Storage {
        pub member: Struct0,
    }
}

#[test]
fn contract_v0_write_new_read_test() {
    let mut contract_state_v0 = contract_v0::contract_state_for_testing();
    let mut contract_state_new = contract_new::contract_state_for_testing();
    contract_state_v0.member.member1.member2.write(1);
    assert_eq!(contract_state_new.member.member0.member1.member2.read(), 1);
}

#[test]
fn contract_new_write_v0_read_test() {
    let mut contract_state_v0 = contract_v0::contract_state_for_testing();
    let mut contract_state_new = contract_new::contract_state_for_testing();
    contract_state_new.member.member0.member1.member2.write(2);
    assert_eq!(contract_state_v0.member.member1.member2.read(), 2);
}
