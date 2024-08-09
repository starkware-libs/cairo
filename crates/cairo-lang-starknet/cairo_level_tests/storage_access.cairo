use starknet::{ClassHash, ContractAddress, EthAddress, StorageAddress};
use super::utils::{deserialized, serialized};

#[feature("deprecated-bounded-int-trait")]
use core::integer::BoundedInt;
use core::num::traits::Zero;
use starknet::storage::Vec;

impl StorageAddressPartialEq of PartialEq<StorageAddress> {
    fn eq(lhs: @StorageAddress, rhs: @StorageAddress) -> bool {
        let lhs: felt252 = (*lhs).into();
        let rhs: felt252 = (*rhs).into();
        lhs == rhs
    }
}

#[derive(Copy, Drop, Debug, Serde, PartialEq, starknet::Store)]
struct Abc {
    a: u8,
    b: u16,
    c: u32,
}

#[derive(Copy, Drop, Debug, Serde, PartialEq)]
struct TupleStructure {
    v1: u256,
    v2: u256,
}
impl TupleStructureStorePacking of starknet::storage_access::StorePacking<
    TupleStructure, (felt252, felt252)
> {
    fn pack(value: TupleStructure) -> (felt252, felt252) {
        (value.v1.try_into().unwrap(), value.v2.try_into().unwrap())
    }
    fn unpack(value: (felt252, felt252)) -> TupleStructure {
        let (v1, v2) = value;
        TupleStructure { v1: v1.into(), v2: v2.into(), }
    }
}

#[derive(Copy, Drop, Debug, Serde, PartialEq, starknet::Store)]
enum Efg {
    E: (),
    F: (),
    G: u256
}

#[derive(Copy, Drop, Debug, Serde, PartialEq, starknet::Store)]
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

#[derive(Clone, Drop, Debug, Serde, PartialEq, starknet::Store)]
struct ByteArrays {
    empty: ByteArray,
    single_word: ByteArray,
    multi_word: ByteArray,
    multi_chunk: ByteArray,
}

#[derive(Copy, Drop, Debug, Serde, PartialEq, starknet::Store)]
struct NonZeros {
    value_u8: NonZero<u8>,
    value_u256: NonZero<u256>,
    value_felt252: NonZero<felt252>,
}

#[starknet::storage_node]
struct Vecs {
    vec: Vec<u32>,
    vec_of_vecs: Vec<Vec<u32>>,
}

#[derive(Copy, Drop, Debug, Serde, PartialEq, starknet::Store)]
#[starknet::sub_pointers(QueryableEnumVariants)]
enum QueryableEnum {
    A: (),
    B: u128,
    C: u256,
}

#[starknet::contract]
mod test_contract {
    use core::starknet::storage::{SubPointersForward, SubPointersMutForward};
    use super::{
        AbcEtc, ByteArrays, NonZeros, Vecs, QueryableEnum, QueryableEnumVariants,
        QueryableEnumVariantsMut
    };
    use starknet::storage::{
        VecTrait, MutableVecTrait, StoragePointerWriteAccess, StoragePointerReadAccess
    };

    #[storage]
    struct Storage {
        data: AbcEtc,
        byte_arrays: ByteArrays,
        non_zeros: NonZeros,
        vecs: Vecs,
        queryable_enum: QueryableEnum,
    }

    #[external(v0)]
    pub fn set_data(ref self: ContractState, value: AbcEtc) {
        self.data.write(value);
    }

    #[external(v0)]
    pub fn get_data(self: @ContractState) -> AbcEtc {
        self.data.read()
    }

    #[external(v0)]
    pub fn get_data_f_high(self: @ContractState) -> u128 {
        self.data.f.high.read()
    }

    #[external(v0)]
    pub fn get_data_f_low(self: @ContractState) -> u128 {
        self.data.f.low.read()
    }

    #[external(v0)]
    pub fn set_byte_arrays(ref self: ContractState, value: ByteArrays) {
        self.byte_arrays.write(value);
    }

    #[external(v0)]
    pub fn get_byte_arrays(self: @ContractState) -> ByteArrays {
        self.byte_arrays.read()
    }

    #[external(v0)]
    pub fn set_non_zeros(ref self: ContractState, value: NonZeros) {
        self.non_zeros.write(value);
    }

    #[external(v0)]
    pub fn get_non_zeros(self: @ContractState) -> NonZeros {
        self.non_zeros.read()
    }

    #[external(v0)]
    pub fn append_to_vec(ref self: ContractState, value: u32) {
        self.vecs.vec.append().write(value);
    }

    #[external(v0)]
    pub fn get_vec_length(self: @ContractState) -> u64 {
        self.vecs.vec.len()
    }

    #[external(v0)]
    pub fn get_vec_element(self: @ContractState, index: u64) -> u32 {
        self.vecs.vec[index].read()
    }

    #[external(v0)]
    pub fn append_an_vec(ref self: ContractState) {
        self.vecs.vec_of_vecs.append();
    }

    #[external(v0)]
    pub fn append_to_nested_vec(ref self: ContractState, index: u64, value: u32) {
        self.vecs.vec_of_vecs[index].append().write(value);
    }

    #[external(v0)]
    pub fn get_vec_of_vecs_length(self: @ContractState) -> u64 {
        self.vecs.vec_of_vecs.len()
    }

    #[external(v0)]
    pub fn get_nested_vec_length(self: @ContractState, index: u64) -> u64 {
        self.vecs.vec_of_vecs[index].len()
    }

    #[external(v0)]
    pub fn get_nested_vec_element(self: @ContractState, index: u64, nested_index: u64) -> u32 {
        self.vecs.vec_of_vecs[index][nested_index].read()
    }

    #[external(v0)]
    pub fn set_queryable_enum(ref self: ContractState, value: QueryableEnum) {
        self.queryable_enum.write(value);
    }

    #[external(v0)]
    pub fn is_queryable_enum_a(self: @ContractState) -> bool {
        if let QueryableEnumVariants::A(_) = self.queryable_enum.sub_pointers() {
            true
        } else {
            false
        }
    }
    #[external(v0)]
    pub fn get_queryable_enum_low(self: @ContractState) -> u128 {
        match self.queryable_enum.sub_pointers() {
            QueryableEnumVariants::B(ptr) => ptr.read(),
            QueryableEnumVariants::C(ptr) => ptr.low.read(),
            _ => 0
        }
    }

    #[external(v0)]
    pub fn set_queryable_enum_low(ref self: ContractState, value: u128) {
        match self.queryable_enum.sub_pointers_mut() {
            QueryableEnumVariantsMut::B(ptr) => ptr.write(value),
            QueryableEnumVariantsMut::C(ptr) => ptr.low.write(value),
            _ => {}
        }
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
        f: BoundedInt::max() - 1,
        g: Zero::zero(),
        h: Zero::zero(),
        i: 123.try_into().unwrap(),
        j: true,
        k: 123_felt252.try_into().unwrap(),
        abc: Abc { a: 1_u8, b: 2_u16, c: 3_u32, },
        ts: TupleStructure { v1: 1_u256, v2: 2_u256, },
        efg1: Efg::E,
        efg2: Efg::G(123_u256)
    };

    assert!(test_contract::__external::set_data(serialized(x)).is_empty());
    assert_eq!(deserialized(test_contract::__external::get_data(serialized(()))), x);
    assert_eq!(
        deserialized(test_contract::__external::get_data_f_low(serialized(()))),
        BoundedInt::max() - 1_u128
    );
    assert_eq!(
        deserialized(test_contract::__external::get_data_f_high(serialized(()))),
        BoundedInt::<u128>::max()
    );
}

#[test]
fn write_read_byte_arrays() {
    let mut multi_chunk: ByteArray =
        "0123456789abcdef0123456789abcdef"; // 32 bytes, 2 felt252s, 1 chunk.
    multi_chunk.append(@multi_chunk); // 64 bytes, 3 felt252s, 1 chunk.
    multi_chunk.append(@multi_chunk); // 128 bytes, 5 felt252s, 1 chunk.
    multi_chunk.append(@multi_chunk); // 256 bytes, 9 felt252s, 1 chunk.
    multi_chunk.append(@multi_chunk); // 512 bytes, 17 felt252s, 1 chunk.
    multi_chunk.append(@multi_chunk); // 1024 bytes, 34 felt252s, 1 chunk.
    multi_chunk.append(@multi_chunk); // 2048 bytes, 67 felt252s, 1 chunk.
    multi_chunk.append(@multi_chunk); // 4096 bytes, 133 felt252s, 1 chunk.
    multi_chunk.append(@multi_chunk); // 8192 bytes, 265 felt252s, 2 chunks.
    multi_chunk.append(@multi_chunk); // 16384 bytes, 529 felt252s, 3 chunks.
    let x = ByteArrays {
        empty: "",
        single_word: "shorter than 31",
        multi_word: "a byte array with more than 31 bytes",
        multi_chunk,
    };

    assert!(test_contract::__external::set_byte_arrays(serialized(x.clone())).is_empty());
    assert_eq!(deserialized(test_contract::__external::get_byte_arrays(serialized(()))), x);
    // Make sure the lengths were saved correctly.
    let base_address = starknet::storage_access::storage_base_address_from_felt252(
        selector!("byte_arrays")
    );
    assert!(starknet::Store::read_at_offset(0, base_address, 0).unwrap() == 0_usize);
    assert!(starknet::Store::read_at_offset(0, base_address, 1).unwrap() == 15_usize);
    assert!(starknet::Store::read_at_offset(0, base_address, 2).unwrap() == 36_usize);
    assert!(starknet::Store::read_at_offset(0, base_address, 3).unwrap() == 16384_usize);
    // Make sure the internal data was saved correctly.
    let (r, _, _) = core::poseidon::hades_permutation(
        starknet::storage_access::storage_address_from_base_and_offset(base_address, 3).into(),
        2,
        'ByteArray'_felt252
    );
    let internal_data_address = starknet::storage_access::storage_base_address_from_felt252(r);
    assert_eq!(
        starknet::Store::read_at_offset(0, internal_data_address, 0).unwrap(),
        '0123456789abcdef0123456789abcde'_felt252
    );
    assert_eq!(
        starknet::Store::read_at_offset(0, internal_data_address, 1).unwrap(),
        'f0123456789abcdef0123456789abcd'_felt252
    );
    assert_eq!(
        starknet::Store::read_at_offset(0, internal_data_address, 2).unwrap(),
        'ef0123456789abcdef0123456789abc'_felt252
    );
}

#[test]
fn test_read_write_non_zero() {
    let x = NonZeros {
        value_u8: 1_u8.try_into().unwrap(),
        value_u256: 3_u256.try_into().unwrap(),
        value_felt252: 5_felt252.try_into().unwrap(),
    };

    assert!(test_contract::__external::set_non_zeros(serialized(x.clone())).is_empty());
    assert_eq!(deserialized(test_contract::__external::get_non_zeros(serialized(()))), x);
}

#[test]
fn test_storage_array() {
    assert!(test_contract::__external::append_to_vec(serialized(1_u32)).is_empty());
    assert!(test_contract::__external::append_to_vec(serialized(2_u32)).is_empty());
    assert!(test_contract::__external::append_to_vec(serialized(3_u32)).is_empty());
    assert_eq!(deserialized(test_contract::__external::get_vec_length(serialized(()))), 3);
    assert_eq!(deserialized(test_contract::__external::get_vec_element(serialized(0_u64))), 1);
    assert_eq!(deserialized(test_contract::__external::get_vec_element(serialized(1_u64))), 2);
    assert_eq!(deserialized(test_contract::__external::get_vec_element(serialized(2_u64))), 3);
}

#[test]
fn test_storage_vec_of_vecs() {
    assert!(test_contract::__external::append_an_vec(serialized(())).is_empty());
    assert!(test_contract::__external::append_to_nested_vec(serialized((0_u64, 1_u32))).is_empty());
    assert!(test_contract::__external::append_to_nested_vec(serialized((0_u64, 2_u32))).is_empty());
    assert!(test_contract::__external::append_to_nested_vec(serialized((0_u64, 3_u32))).is_empty());
    assert!(test_contract::__external::append_an_vec(serialized(())).is_empty());
    assert!(test_contract::__external::append_to_nested_vec(serialized((1_u64, 4_u32))).is_empty());
    assert!(test_contract::__external::append_to_nested_vec(serialized((1_u64, 5_u32))).is_empty());
    assert_eq!(deserialized(test_contract::__external::get_vec_of_vecs_length(serialized(()))), 2);
    assert_eq!(
        deserialized(test_contract::__external::get_nested_vec_length(serialized(0_u64))), 3
    );
    assert_eq!(
        deserialized(test_contract::__external::get_nested_vec_element(serialized((0_u64, 0_u64)))),
        1
    );
    assert_eq!(
        deserialized(test_contract::__external::get_nested_vec_element(serialized((0_u64, 1_u64)))),
        2
    );
    assert_eq!(
        deserialized(test_contract::__external::get_nested_vec_element(serialized((0_u64, 2_u64)))),
        3
    );
    assert_eq!(
        deserialized(test_contract::__external::get_nested_vec_length(serialized(1_u64))), 2
    );
    assert_eq!(
        deserialized(test_contract::__external::get_nested_vec_element(serialized((1_u64, 0_u64)))),
        4
    );
    assert_eq!(
        deserialized(test_contract::__external::get_nested_vec_element(serialized((1_u64, 1_u64)))),
        5
    );
}

#[test]
fn test_enum_sub_pointers() {
    assert!(
        test_contract::__external::set_queryable_enum(serialized(QueryableEnum::A(()))).is_empty()
    );
    assert!(deserialized(test_contract::__external::is_queryable_enum_a(serialized(()))));
    assert_eq!(deserialized(test_contract::__external::get_queryable_enum_low(serialized(()))), 0);
    assert!(
        test_contract::__external::set_queryable_enum(serialized(QueryableEnum::B(123_u128)))
            .is_empty()
    );
    assert!(!deserialized(test_contract::__external::is_queryable_enum_a(serialized(()))));
    assert_eq!(
        deserialized(test_contract::__external::get_queryable_enum_low(serialized(()))), 123
    );
    assert!(
        test_contract::__external::set_queryable_enum(serialized(QueryableEnum::C(456_u256)))
            .is_empty()
    );
    assert!(!deserialized(test_contract::__external::is_queryable_enum_a(serialized(()))));
    assert_eq!(
        deserialized(test_contract::__external::get_queryable_enum_low(serialized(()))), 456
    );
    assert!(
        test_contract::__external::set_queryable_enum(serialized(QueryableEnum::C(789_u256)))
            .is_empty()
    );
    assert!(!deserialized(test_contract::__external::is_queryable_enum_a(serialized(()))));
    assert_eq!(
        deserialized(test_contract::__external::get_queryable_enum_low(serialized(()))), 789
    );
}
