use core::starknet::storage::{StoragePointerWriteAccess, StoragePointerReadAccess};

#[starknet::storage_node]
struct Struct1 {
    pub member1: felt252,
}

#[starknet::storage_node]
struct Struct0 {
    pub member0: Struct1,
}

#[starknet::contract]
mod contract_v0 {
    use core::starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use super::Struct0;
    #[storage]
    pub struct Storage {
        pub member: Struct0,
    }
}

#[starknet::storage_node]
struct IgnoredMemberStruct {
    #[flat]
    pub ignored_member: Struct0,
}

#[starknet::contract]
mod contract_new {
    use core::starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use super::IgnoredMemberStruct;
    #[storage]
    pub struct Storage {
        pub member: IgnoredMemberStruct,
    }
}

#[test]
fn contract_v0_write_new_read_test() {
    let mut contract_state_v0 = contract_v0::contract_state_for_testing();
    let mut contract_state_new = contract_new::contract_state_for_testing();
    contract_state_v0.member.member0.member1.write(1);
    assert_eq!(contract_state_new.member.ignored_member.member0.member1.read(), 1);
}

#[test]
fn contract_new_write_v0_read_test() {
    let mut contract_state_v0 = contract_v0::contract_state_for_testing();
    let mut contract_state_new = contract_new::contract_state_for_testing();
    contract_state_new.member.ignored_member.member0.member1.write(2);
    assert_eq!(contract_state_v0.member.member0.member1.read(), 2);
}
