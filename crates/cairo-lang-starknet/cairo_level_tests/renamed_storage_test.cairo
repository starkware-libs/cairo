use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};

#[starknet::storage_node]
struct A {
    pub a: felt252,
}

#[starknet::storage_node]
struct B {
    #[rename("a")]
    pub b: felt252,
}

#[starknet::storage_node]
struct AB {
    #[flat]
    pub a: A,
    #[allow(starknet::colliding_storage_paths)]
    #[flat]
    pub b: B,
}

#[starknet::contract]
mod contract {
    #[storage]
    pub struct Storage {
        pub a: super::A,
        #[allow(starknet::colliding_storage_paths)]
        #[rename("a")]
        pub b: super::B,
        pub ab: super::AB,
    }
}

#[test]
fn rename_test() {
    let mut state = contract::contract_state_for_testing();
    state.a.a.write(1);
    assert_eq!(state.b.b.read(), 1);
    state.b.b.write(2);
    assert_eq!(state.a.a.read(), 2);
    assert_eq!(state.ab.a.a.read(), 0);
    state.ab.a.a.write(3);
    assert_eq!(state.ab.b.b.read(), 3);
    state.ab.b.b.write(4);
    assert_eq!(state.ab.a.a.read(), 4);
}
