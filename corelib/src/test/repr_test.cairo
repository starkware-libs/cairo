use test::test_utils::assert_eq;

use repr::{
    U8Representation, U16Representation, U32Representation, U64Representation, U128Representation,
    U256Representation, I8Representation, I16Representation, I32Representation, I64Representation,
    I128Representation, Felt252Representation
};

#[test]
fn test_bits_representation() {
    assert_eq(@(U8Representation::bits()), @8, 'U8 bits != 8');
    assert_eq(@(U16Representation::bits()), @16, 'U16 bits != 16');
    assert_eq(@(U32Representation::bits()), @32, 'U32 bits != 32');
    assert_eq(@(U64Representation::bits()), @64, 'U64 bits != 64');
    assert_eq(@(U128Representation::bits()), @128, 'U128 bits != 128');
    assert_eq(@(U256Representation::bits()), @256, 'U256 bits != 256');
    assert_eq(@(I8Representation::bits()), @8, 'I8 bits != 8');
    assert_eq(@(I16Representation::bits()), @16, 'I16 bits != 16');
    assert_eq(@(I32Representation::bits()), @32, 'I32 bits != 32');
    assert_eq(@(I64Representation::bits()), @64, 'I64 bits != 64');
    assert_eq(@(I128Representation::bits()), @128, 'I128 bits != 128');
    assert_eq(@(Felt252Representation::bits()), @252, 'Felt252 bits != 252');
}

#[test]
fn test_bits_representation_variables() {
    assert_eq(@(1_u8.bits_repr()), @8, 'u8 bits repr != 8');
    assert_eq(@(1_u16.bits_repr()), @16, 'u16 bits repr != 16');
    assert_eq(@(1_u32.bits_repr()), @32, 'u32 bits repr != 32');
    assert_eq(@(1_u64.bits_repr()), @64, 'u64 bits repr != 64');
    assert_eq(@(1_u128.bits_repr()), @128, 'u128 bits repr != 128');
    assert_eq(@(1_u256.bits_repr()), @256, 'u256 bits repr != 256');
    assert_eq(@(1_i8.bits_repr()), @8, 'i8 bits repr != 8');
    assert_eq(@(1_i16.bits_repr()), @16, 'i16 bits repr != 16');
    assert_eq(@(1_i32.bits_repr()), @32, 'i32 bits repr != 32');
    assert_eq(@(1_i64.bits_repr()), @64, 'i64 bits repr != 64');
    assert_eq(@(1_i128.bits_repr()), @128, 'i128 bits repr != 128');
    assert_eq(@(1_felt252.bits_repr()), @252, 'felt252 bits repr != 252');
}
