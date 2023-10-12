use test::test_utils::assert_eq;

use repr::{
    U8Representation, U16Representation, U32Representation, U64Representation, U128Representation,
    U256Representation, I8Representation, I16Representation, I32Representation, I64Representation,
    I128Representation
};

#[test]
fn test_bits_representation() {
    assert_eq(@(U8Representation::bits()), @8, 'u8 bits repr != 8');
    assert_eq(@(U16Representation::bits()), @16, 'u16 bits repr != 16');
    assert_eq(@(U32Representation::bits()), @32, 'u32 bits repr != 32');
    assert_eq(@(U64Representation::bits()), @64, 'u64 bits repr != 64');
    assert_eq(@(U128Representation::bits()), @128, 'u128 bits repr != 128');
    assert_eq(@(U256Representation::bits()), @256, 'u256 bits repr != 256');
    assert_eq(@(I8Representation::bits()), @8, 'i8 bits repr != 8');
    assert_eq(@(I16Representation::bits()), @16, 'i16 bits repr != 16');
    assert_eq(@(I32Representation::bits()), @32, 'i32 bits repr != 32');
    assert_eq(@(I64Representation::bits()), @64, 'i64 bits repr != 64');
    assert_eq(@(I128Representation::bits()), @128, 'i128 bits repr != 128');
}

#[test]
fn test_bits_representation_variables() {
    assert_eq(@(1_u8.in_bits()), @8, 'u8 bits repr != 8');
    assert_eq(@(1_u16.in_bits()), @16, 'u16 bits repr != 16');
    assert_eq(@(1_u32.in_bits()), @32, 'u32 bits repr != 32');
    assert_eq(@(1_u64.in_bits()), @64, 'u64 bits repr != 64');
    assert_eq(@(1_u128.in_bits()), @128, 'u128 bits repr != 128');
    assert_eq(@(1_u256.in_bits()), @256, 'u256 bits repr != 256');
    assert_eq(@(1_i8.in_bits()), @8, 'i8 bits repr != 8');
    assert_eq(@(1_i16.in_bits()), @16, 'i16 bits repr != 16');
    assert_eq(@(1_i32.in_bits()), @32, 'i32 bits repr != 32');
    assert_eq(@(1_i64.in_bits()), @64, 'i64 bits repr != 64');
    assert_eq(@(1_i128.in_bits()), @128, 'i128 bits repr != 128');
}
