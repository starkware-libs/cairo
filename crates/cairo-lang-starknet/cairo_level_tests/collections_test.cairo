use core::starknet::storage::StoragePointerReadAccess;
use core::starknet::storage::MutableStorageVecTrait;
use core::starknet::storage::StoragePathEntry;


#[starknet::contract]
mod contract_with_map {
    use starknet::storage::Map;
    #[storage]
    struct Storage {
        simple: Map::<u64, u32>,
        nested: Map::<u64, Map<u64, u32>>,
    }
}

#[starknet::contract]
mod contract_with_vec {
    use starknet::storage::StorageVec;
    #[storage]
    struct Storage {
        simple: StorageVec::<u32>,
        nested: StorageVec::<StorageVec<u32>>,
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
