#[feature("deprecated-bounded-int-trait")]
use core::integer::BoundedInt;
use core::num::traits::Zero;
use starknet::storage::{
    Map, MutableVecTrait, StorageMapReadAccess, StoragePointerReadAccess, StoragePointerWriteAccess,
    SubPointersForward, Vec,
};
use starknet::{ClassHash, ContractAddress, EthAddress, StorageAddress};

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
    TupleStructure, (felt252, felt252),
> {
    fn pack(value: TupleStructure) -> (felt252, felt252) {
        (value.v1.try_into().unwrap(), value.v2.try_into().unwrap())
    }
    fn unpack(value: (felt252, felt252)) -> TupleStructure {
        let (v1, v2) = value;
        TupleStructure { v1: v1.into(), v2: v2.into() }
    }
}

#[derive(Copy, Drop, Debug, Serde, PartialEq, starknet::Store)]
enum Efg {
    #[default]
    E: (),
    F: (),
    G: u256,
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
    #[allow(starknet::colliding_storage_paths)]
    #[rename("vec")]
    equiv_map: Map<usize, u32>,
}

#[derive(Copy, Drop, Debug, Serde, PartialEq, starknet::Store)]
#[starknet::sub_pointers(QueryableEnumVariants)]
enum QueryableEnum {
    #[default]
    A: (),
    B: u128,
    C: u256,
}

#[starknet::contract]
mod test_contract {
    #[storage]
    pub struct Storage {
        pub data: super::AbcEtc,
        pub byte_arrays: super::ByteArrays,
        pub non_zeros: super::NonZeros,
        pub vecs: super::Vecs,
        pub queryable_enum: super::QueryableEnum,
    }
}

#[test]
fn write_read_struct() {
    let mut state = test_contract::contract_state_for_testing();
    let value = AbcEtc {
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
        abc: Abc { a: 1_u8, b: 2_u16, c: 3_u32 },
        ts: TupleStructure { v1: 1_u256, v2: 2_u256 },
        efg1: Efg::E,
        efg2: Efg::G(123_u256),
    };
    state.data.write(value);
    assert_eq!(state.data.read(), value);
    assert_eq!(state.data.f.low.read(), value.f.low);
    assert_eq!(state.data.f.high.read(), value.f.high);
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
    let value = ByteArrays {
        empty: "",
        single_word: "shorter than 31",
        multi_word: "a byte array with more than 31 bytes",
        multi_chunk,
    };
    let mut state = test_contract::contract_state_for_testing();
    state.byte_arrays.write(value.clone());
    assert_eq!(state.byte_arrays.read(), value);
    // Make sure the lengths were saved correctly.
    let base_address = starknet::storage_access::storage_base_address_from_felt252(
        selector!("byte_arrays"),
    );
    assert!(starknet::Store::read_at_offset(0, base_address, 0) == Ok(0_usize));
    assert!(starknet::Store::read_at_offset(0, base_address, 1) == Ok(15_usize));
    assert!(starknet::Store::read_at_offset(0, base_address, 2) == Ok(36_usize));
    assert!(starknet::Store::read_at_offset(0, base_address, 3) == Ok(16384_usize));
    // Make sure the internal data was saved correctly.
    let (r, _, _) = core::poseidon::hades_permutation(
        starknet::storage_access::storage_address_from_base_and_offset(base_address, 3).into(),
        2,
        'ByteArray'_felt252,
    );
    let internal_data_address = starknet::storage_access::storage_base_address_from_felt252(r);
    assert_eq!(
        starknet::Store::read_at_offset(0, internal_data_address, 0),
        Ok('0123456789abcdef0123456789abcde'_felt252),
    );
    assert_eq!(
        starknet::Store::read_at_offset(0, internal_data_address, 1),
        Ok('f0123456789abcdef0123456789abcd'_felt252),
    );
    assert_eq!(
        starknet::Store::read_at_offset(0, internal_data_address, 2),
        Ok('ef0123456789abcdef0123456789abc'_felt252),
    );
}

#[test]
fn test_read_write_non_zero() {
    let value = NonZeros {
        value_u8: 1_u8.try_into().unwrap(),
        value_u256: 3_u256.try_into().unwrap(),
        value_felt252: 5_felt252.try_into().unwrap(),
    };
    let mut state = test_contract::contract_state_for_testing();
    state.non_zeros.write(value);
    assert_eq!(state.non_zeros.read(), value);
}

#[test]
fn test_storage_array() {
    let mut state = test_contract::contract_state_for_testing();
    state.vecs.vec.push(1_u32);
    state.vecs.vec.push(2_u32);
    state.vecs.vec.push(3_u32);
    assert_eq!(state.vecs.vec.len(), 3);
    assert_eq!(state.vecs.vec[0].read(), 1);
    assert_eq!(state.vecs.vec[1].read(), 2);
    assert_eq!(state.vecs.vec[2].read(), 3);
    assert_eq!(state.vecs.equiv_map.read(0), 1);
    assert_eq!(state.vecs.equiv_map.read(1), 2);
    assert_eq!(state.vecs.equiv_map.read(2), 3);
    assert_eq!(state.vecs.equiv_map.read(3), 0);
}

#[test]
fn test_storage_vec_of_vecs() {
    let mut state = test_contract::contract_state_for_testing();
    state.vecs.vec_of_vecs.allocate();
    state.vecs.vec_of_vecs[0].push(1);
    state.vecs.vec_of_vecs[0].push(2);
    state.vecs.vec_of_vecs[0].push(3);
    state.vecs.vec_of_vecs.allocate();
    state.vecs.vec_of_vecs[1].push(4);
    state.vecs.vec_of_vecs[1].push(5);
    assert_eq!(state.vecs.vec_of_vecs.len(), 2);
    assert_eq!(state.vecs.vec_of_vecs[0].len(), 3);
    assert_eq!(state.vecs.vec_of_vecs[0][0].read(), 1);
    assert_eq!(state.vecs.vec_of_vecs[0][1].read(), 2);
    assert_eq!(state.vecs.vec_of_vecs[0][2].read(), 3);
    assert_eq!(state.vecs.vec_of_vecs[1].len(), 2);
    assert_eq!(state.vecs.vec_of_vecs[1][0].read(), 4);
    assert_eq!(state.vecs.vec_of_vecs[1][1].read(), 5);
}

#[test]
fn test_enum_sub_pointers() {
    let mut state = test_contract::contract_state_for_testing();
    state.queryable_enum.write(QueryableEnum::A);
    if let QueryableEnumVariants::A(_) = (@state)
        .queryable_enum
        .sub_pointers() {} else {
            panic!("expected QueryableEnumVariants::A(_)");
        }
    state.queryable_enum.write(QueryableEnum::B(123_u128));
    if let QueryableEnumVariants::B(ptr) = (@state).queryable_enum.sub_pointers() {
        assert_eq!(ptr.read(), 123);
    } else {
        panic!("expected QueryableEnumVariants::B(_)");
    }
    state.queryable_enum.write(QueryableEnum::C(456_u256));
    if let QueryableEnumVariants::C(ptr) = (@state).queryable_enum.sub_pointers() {
        assert_eq!(ptr.low.read(), 456);
        assert_eq!(ptr.high.read(), 0);
    } else {
        panic!("expected QueryableEnumVariants::C(_)");
    }
    state.queryable_enum.write(QueryableEnum::C(789_u256));
    if let QueryableEnumVariants::C(ptr) = (@state).queryable_enum.sub_pointers() {
        assert_eq!(ptr.low.read(), 789);
        assert_eq!(ptr.high.read(), 0);
    } else {
        panic!("expected QueryableEnumVariants::C(_)");
    }
}

#[test]
fn test_scrub_clears_memory() {
    let base_address = starknet::storage_access::storage_base_address_from_felt252(
        selector!("data"),
    );
    for i in 0..=255_u8 {
        starknet::Store::<u8>::write_at_offset(0, base_address, i, 1).unwrap();
    }
    starknet::Store::<[felt252; 11]>::scrub(0, base_address, 7).unwrap();
    for i in 0..7_u8 {
        assert_eq!(starknet::Store::<u8>::read_at_offset(0, base_address, i), Ok(1));
    }
    for i in 7..18_u8 {
        assert_eq!(starknet::Store::<u8>::read_at_offset(0, base_address, i), Ok(0));
    }
    for i in 18..=255_u8 {
        assert_eq!(starknet::Store::<u8>::read_at_offset(0, base_address, i), Ok(1));
    }
}
