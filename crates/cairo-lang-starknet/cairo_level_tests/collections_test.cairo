use starknet::storage::{
    IntoIterRange, MutableVecTrait, StoragePathEntry, StoragePointerReadAccess,
    StoragePointerWriteAccess,
};

#[starknet::contract]
mod contract_with_map {
    use starknet::storage::Map;
    #[storage]
    pub struct Storage {
        pub simple: Map<u64, u32>,
        pub nested: Map<u64, Map<u64, u32>>,
    }
}

#[starknet::contract]
mod contract_with_vec {
    use starknet::storage::Vec;
    #[storage]
    pub struct Storage {
        pub simple: Vec<u32>,
        pub nested: Vec<Vec<u32>>,
    }
}

#[test]
fn test_simple_member_write_to_map() {
    let mut map_contract_state = contract_with_map::contract_state_for_testing();
    let mut vec_contract_state = contract_with_vec::contract_state_for_testing();
    let vec_entry = vec_contract_state.simple.allocate();
    map_contract_state.simple.entry(0).write(1);
    assert_eq!(vec_entry.read(), 1);
}

#[test]
fn test_vec_iter() {
    let mut mut_state = contract_with_vec::contract_state_for_testing();
    for i in 0..9_usize {
        mut_state.simple.push(i);
    }

    let state = @contract_with_vec::contract_state_for_testing();
    let mut i = 0;
    for entry in state.simple.into_iter_full_range() {
        assert_eq!(entry.read(), i);
        i += 1;
    }
    assert_eq!(i, 9);

    let mut i = 2;
    for entry in state.simple.into_iter_range(2..5) {
        assert_eq!(entry.read(), i);
        i += 1;
    }
    assert_eq!(i, 5);
}

#[test]
fn test_mut_vec_iter() {
    let mut mut_state = contract_with_vec::contract_state_for_testing();
    for i in 0..9_usize {
        mut_state.simple.push(i);
    }

    let mut i = 0;
    for entry in mut_state.simple.into_iter_full_range() {
        assert_eq!(entry.read(), i);
        i += 1;
    }
    assert_eq!(i, 9);

    let mut i = 2;
    for entry in mut_state.simple.into_iter_range(2..5) {
        assert_eq!(entry.read(), i);
        i += 1;
    }
    assert_eq!(i, 5);
}

#[test]
fn test_simple_member_write_to_vec() {
    let mut map_contract_state = contract_with_map::contract_state_for_testing();
    let mut vec_contract_state = contract_with_vec::contract_state_for_testing();
    vec_contract_state.simple.push(1);
    assert_eq!(map_contract_state.simple.entry(0).read(), 1);
}

#[test]
fn test_nested_member_write_to_map() {
    let mut map_contract_state = contract_with_map::contract_state_for_testing();
    let mut vec_contract_state = contract_with_vec::contract_state_for_testing();
    let vec_entry = vec_contract_state.nested.allocate().allocate();
    map_contract_state.nested.entry(0).entry(0).write(1);
    assert_eq!(vec_entry.read(), 1);
}

#[test]
fn test_nested_member_write_to_vec() {
    let mut map_contract_state = contract_with_map::contract_state_for_testing();
    let mut vec_contract_state = contract_with_vec::contract_state_for_testing();
    vec_contract_state.nested.allocate().push(1);
    assert_eq!(map_contract_state.nested.entry(0).entry(0).read(), 1);
}

#[test]
fn test_simple_member_push_to_vec() {
    let mut state = contract_with_vec::contract_state_for_testing();
    state.simple.push(10);
    state.simple.push(20);
    state.simple.push(30);
    assert_eq!(state.simple.len(), 3);
    assert_eq!(state.simple.at(0).read(), 10);
    assert_eq!(state.simple.at(1).read(), 20);
    assert_eq!(state.simple.at(2).read(), 30);
}

#[test]
fn test_simple_member_pop_from_vec() {
    let mut state = contract_with_vec::contract_state_for_testing();
    state.simple.push(10);
    state.simple.push(20);
    state.simple.push(30);
    assert_eq!(state.simple.pop(), Some(30));
    assert_eq!(state.simple.pop(), Some(20));
    assert_eq!(state.simple.pop(), Some(10));
    assert_eq!(state.simple.len(), 0);
    assert_eq!(state.simple.pop(), None);
}
