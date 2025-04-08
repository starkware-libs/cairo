use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};

#[starknet::component]
mod comp_v0 {
    #[storage]
    pub struct Storage {
        pub member: felt252,
    }
}

#[starknet::contract]
mod contract_v0 {
    use super::comp_v0;
    component!(path: comp_v0, storage: comp, event: comp);
    #[storage]
    pub struct Storage {
        #[substorage(v0)]
        pub comp: comp_v0::Storage,
    }
    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        comp: comp_v0::Event,
    }
}

#[starknet::storage_node]
struct ComponentNewStorage {
    member: felt252,
}

#[starknet::contract]
mod contract_new {
    use super::ComponentNewStorage;
    #[storage]
    pub struct Storage {
        #[flat]
        pub comp: ComponentNewStorage,
    }
}

#[test]
fn contract_v0_write_new_read_test() {
    let mut contract_state_v0 = contract_v0::contract_state_for_testing();
    let mut contract_state_new = contract_new::contract_state_for_testing();
    contract_state_v0.comp.member.write(1);
    assert_eq!(contract_state_new.comp.member.read(), 1);
}

#[test]
fn contract_new_write_v0_read_test() {
    let mut contract_state_v0 = contract_v0::contract_state_for_testing();
    let mut contract_state_new = contract_new::contract_state_for_testing();
    contract_state_new.comp.member.write(2);
    assert_eq!(contract_state_v0.comp.member.read(), 2);
}
