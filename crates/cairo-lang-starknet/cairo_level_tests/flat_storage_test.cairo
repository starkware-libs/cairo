use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};

#[starknet::storage_node]
struct Struct1 {
    pub member1: felt252,
}

#[starknet::storage_node]
struct Struct0 {
    pub member0: Struct1,
}

#[starknet::contract]
mod with_ignored {
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
mod without_ignored {
    use super::IgnoredMemberStruct;
    #[storage]
    pub struct Storage {
        pub member: IgnoredMemberStruct,
    }
}

#[test]
fn with_ignored_write_without_ignored_read_test() {
    let mut contract_state_v0 = with_ignored::contract_state_for_testing();
    let mut contract_state_new = without_ignored::contract_state_for_testing();
    contract_state_v0.member.member0.member1.write(1);
    assert_eq!(contract_state_new.member.ignored_member.member0.member1.read(), 1);
}

#[test]
fn without_ignored_write_with_ignored_read_test() {
    let mut contract_state_v0 = with_ignored::contract_state_for_testing();
    let mut contract_state_new = without_ignored::contract_state_for_testing();
    contract_state_new.member.ignored_member.member0.member1.write(2);
    assert_eq!(contract_state_v0.member.member0.member1.read(), 2);
}
