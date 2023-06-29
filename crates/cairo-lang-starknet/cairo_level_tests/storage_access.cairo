use core::option::OptionTrait;
use core::traits::{Into, TryInto};
use array::{ArrayTrait, SpanTrait};
use starknet::{
    ClassHash, ContractAddress, EthAddress, StorageAddress, SyscallResult,
    storage_address_to_felt252, storage_address_try_from_felt252
};
use starknet::eth_address::Felt252TryIntoEthAddress;
use super::utils::{serialized_element, single_deserialize};
use integer::BoundedInt;
use zeroable::Zeroable;

impl StorageAddressPartialEq of PartialEq<StorageAddress> {
    fn eq(lhs: @StorageAddress, rhs: @StorageAddress) -> bool {
        storage_address_to_felt252(*lhs) == storage_address_to_felt252(*rhs)
    }
    fn ne(lhs: @StorageAddress, rhs: @StorageAddress) -> bool {
        !(storage_address_to_felt252(*lhs) == storage_address_to_felt252(*rhs))
    }
}

#[derive(Drop, Serde, PartialEq, Copy, storage_access::StorageAccess)]
struct Abc {
    a: u8,
    b: u16,
    c: u32,
}

#[derive(Drop, Serde, PartialEq, Copy, storage_access::StorageAccess)]
struct AbcEtc {
    a: u8,
    b: u16,
    c: u32,
    d: u64,
    e: u128,
    f: u256,
    g: ContractAddress,
    h: ClassHash,
    i: StorageAddress,
    j: bool,
    k: EthAddress,
    abc: Abc,
}


#[starknet::contract]
mod test_contract {
    use super::AbcEtc;

    #[storage]
    struct Storage {
        data: AbcEtc, 
    }

    #[external(v0)]
    fn set_data(ref self: ContractState, value: AbcEtc) {
        self.data.write(value);
    }

    #[external(v0)]
    fn get_data(self: @ContractState) -> AbcEtc {
        self.data.read()
    }
}

#[test]
#[available_gas(900000)]
fn write_read_struct() {
    let x = AbcEtc {
        a: 1_u8,
        b: 2_u16,
        c: 3_u32,
        d: 4_u64,
        e: 5_u128,
        f: BoundedInt::max(),
        g: Zeroable::zero(),
        h: Zeroable::zero(),
        i: storage_address_try_from_felt252(123_felt252).unwrap(),
        j: true,
        k: 123_felt252.try_into().unwrap(),
        abc: Abc {
            a: 1_u8, b: 2_u16, c: 3_u32, 
        }
    };

    assert(test_contract::__external::set_data(serialized_element(*@x)).is_empty(), 'Not empty');

    let mut retdata = test_contract::__external::get_data(Default::default().span());
    assert(single_deserialize(ref retdata) == x, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}
