use array::ArrayTrait;
use array::SpanTrait;
use starknet::ContractAddress;
use starknet::StorageAccess;
use starknet::StorageBaseAddress;
use starknet::SyscallResult;

use super::utils::serialized_element;
use super::utils::single_deserialize;

use starknet::storage_access::update_base;

use integer::BoundedInt;

#[derive(Drop, Serde, PartialEq, Copy, storage_access::StorageAccess)]
struct AB {
    a: u32,
    b: u256
}

#[derive(Drop, Serde, PartialEq, Copy, storage_access::StorageAccess)]
struct ABC {
    ab: AB,
    c: u256
}

#[contract]
mod TestContract {
    use super::ABC;

    struct Storage {
        abc: ABC, 
    }

    #[external]
    fn set_abc(value: ABC) {
        abc::write(value);
    }

    #[view]
    fn get_abc() -> ABC {
        abc::read()
    }
}

#[test]
#[available_gas(300000)]
fn write_read_value() {
    let abc = ABC { ab: AB { a: 1_u32, b: 2_u256 }, c: BoundedInt::max() };

    assert(TestContract::__external::set_abc(serialized_element(*@abc)).is_empty(), 'Not empty');

    let mut retdata = TestContract::__external::get_abc(ArrayTrait::new().span());
    assert(single_deserialize(ref retdata) == abc, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}
