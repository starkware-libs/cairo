use core::starknet::storage::StoragePointerReadAccess;
use core::starknet::storage::MutableStorageArrayTrait;
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
mod contract_with_array {
    use starknet::storage::StorageArray;
    #[storage]
    struct Storage {
        simple: StorageArray::<u32>,
        nested: StorageArray::<StorageArray<u32>>,
    }
}

#[test]
fn test_simple_member_write_to_map() {
    let mut map_contract_state = contract_with_map::contract_state_for_testing();
    let mut array_contract_state = contract_with_array::contract_state_for_testing();
    let array_entry = array_contract_state.simple.append();
    map_contract_state.simple.entry(0).write(1);
    assert_eq!(array_entry.read(), 1);
}

#[test]
fn test_simple_member_write_to_array() {
    let mut map_contract_state = contract_with_map::contract_state_for_testing();
    let mut array_contract_state = contract_with_array::contract_state_for_testing();
    array_contract_state.simple.append().write(1);
    assert_eq!(map_contract_state.simple.entry(0).read(), 1);
}

#[test]
fn test_nested_member_write_to_map() {
    let mut map_contract_state = contract_with_map::contract_state_for_testing();
    let mut array_contract_state = contract_with_array::contract_state_for_testing();
    let array_entry = array_contract_state.nested.append().append();
    map_contract_state.nested.entry(0).entry(0).write(1);
    assert_eq!(array_entry.read(), 1);
}

#[test]
fn test_nested_member_write_to_array() {
    let mut map_contract_state = contract_with_map::contract_state_for_testing();
    let mut array_contract_state = contract_with_array::contract_state_for_testing();
    array_contract_state.nested.append().append().write(1);
    assert_eq!(map_contract_state.nested.entry(0).entry(0).read(), 1);
}
