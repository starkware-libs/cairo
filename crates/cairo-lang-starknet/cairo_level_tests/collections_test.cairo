use starknet::storage::{
    StoragePointerReadAccess, StoragePointerWriteAccess, MutableVecTrait, StoragePathEntry
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
    let vec_entry = vec_contract_state.simple.append();
    map_contract_state.simple.entry(0).write(1);
    assert_eq!(vec_entry.read(), 1);
}

#[test]
fn test_simple_member_write_to_vec() {
    let mut map_contract_state = contract_with_map::contract_state_for_testing();
    let mut vec_contract_state = contract_with_vec::contract_state_for_testing();
    vec_contract_state.simple.append().write(1);
    assert_eq!(map_contract_state.simple.entry(0).read(), 1);
}

#[test]
fn test_nested_member_write_to_map() {
    let mut map_contract_state = contract_with_map::contract_state_for_testing();
    let mut vec_contract_state = contract_with_vec::contract_state_for_testing();
    let vec_entry = vec_contract_state.nested.append().append();
    map_contract_state.nested.entry(0).entry(0).write(1);
    assert_eq!(vec_entry.read(), 1);
}

#[test]
fn test_nested_member_write_to_vec() {
    let mut map_contract_state = contract_with_map::contract_state_for_testing();
    let mut vec_contract_state = contract_with_vec::contract_state_for_testing();
    vec_contract_state.nested.append().append().write(1);
    assert_eq!(map_contract_state.nested.entry(0).entry(0).read(), 1);
}
