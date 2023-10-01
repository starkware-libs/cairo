use starknet::{
    ClassHash, ContractAddress, EthAddress, StorageAddress, SyscallResult,
    storage_address_to_felt252, storage_address_try_from_felt252
};
use test::test_utils::assert_eq;
use super::utils::serialized;
use integer::BoundedInt;

impl StorageAddressPartialEq of PartialEq<StorageAddress> {
    fn eq(lhs: @StorageAddress, rhs: @StorageAddress) -> bool {
        storage_address_to_felt252(*lhs) == storage_address_to_felt252(*rhs)
    }
    fn ne(lhs: @StorageAddress, rhs: @StorageAddress) -> bool {
        !(storage_address_to_felt252(*lhs) == storage_address_to_felt252(*rhs))
    }
}

#[derive(Drop, Serde, PartialEq, Copy, starknet::Store)]
struct Abc {
    a: u8,
    b: u16,
    c: u32,
}

#[derive(Drop, Serde, PartialEq, Copy)]
struct TupleStructure {
    v1: u256,
    v2: u256,
}
impl TupleStructureStorePacking of starknet::StorePacking<TupleStructure, (felt252, felt252)> {
    fn pack(value: TupleStructure) -> (felt252, felt252) {
        (value.v1.try_into().unwrap(), value.v2.try_into().unwrap())
    }
    fn unpack(value: (felt252, felt252)) -> TupleStructure {
        let (v1, v2) = value;
        TupleStructure { v1: v1.into(), v2: v2.into(), }
    }
}

#[derive(Drop, Serde, PartialEq, Copy, starknet::Store)]
enum Efg {
    E: (),
    F: (),
    G: u256
}

#[derive(Drop, Serde, PartialEq, Copy, starknet::Store)]
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
    ts: TupleStructure,
    efg1: Efg,
    efg2: Efg,
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
        abc: Abc { a: 1_u8, b: 2_u16, c: 3_u32, },
        ts: TupleStructure { v1: 1_u256, v2: 2_u256, },
        efg1: Efg::E,
        efg2: Efg::G(123_u256)
    };

    assert(test_contract::__external::set_data(serialized(*@x)).is_empty(), 'Not empty');
    assert_eq(@test_contract::__external::get_data(serialized(())), @serialized(x), 'Wrong result');
}
