use test::test_utils::assert_eq;

use num::traits::{
    U8Bits, U16Bits, U32Bits, U64Bits, U128Bits, U256Bits, I8Bits, I16Bits, I32Bits, I64Bits,
    I128Bits, Bytes31Bits
};

#[test]
fn test_bits() {
    assert_eq(@(U8Bits::len()), @8, 'U8 bits != 8');
    assert_eq(@(U16Bits::len()), @16, 'U16 bits != 16');
    assert_eq(@(U32Bits::len()), @32, 'U32 bits != 32');
    assert_eq(@(U64Bits::len()), @64, 'U64 bits != 64');
    assert_eq(@(U128Bits::len()), @128, 'U128 bits != 128');
    assert_eq(@(U256Bits::len()), @256, 'U256 bits != 256');
    assert_eq(@(I8Bits::len()), @8, 'I8 bits != 8');
    assert_eq(@(I16Bits::len()), @16, 'I16 bits != 16');
    assert_eq(@(I32Bits::len()), @32, 'I32 bits != 32');
    assert_eq(@(I64Bits::len()), @64, 'I64 bits != 64');
    assert_eq(@(I128Bits::len()), @128, 'I128 bits != 128');
    assert_eq(@(Bytes31Bits::len()), @31, 'Bytes31 bits != 31');
}

#[test]
fn test_bits_self() {
    assert_eq(@(1_u8.bits()), @8, 'u8 bits != 8');
    assert_eq(@(1_u16.bits()), @16, 'u16 bits != 16');
    assert_eq(@(1_u32.bits()), @32, 'u32 bits != 32');
    assert_eq(@(1_u64.bits()), @64, 'u64 bits != 64');
    assert_eq(@(1_u128.bits()), @128, 'u128 bits != 128');
    assert_eq(@(1_u256.bits()), @256, 'u256 bits != 256');
    assert_eq(@(1_i8.bits()), @8, 'i8 bits != 8');
    assert_eq(@(1_i16.bits()), @16, 'i16 bits != 16');
    assert_eq(@(1_i32.bits()), @32, 'i32 bits != 32');
    assert_eq(@(1_i64.bits()), @64, 'i64 bits != 64');
    assert_eq(@(1_i128.bits()), @128, 'i128 bits != 128');
    assert_eq(@(Into::<u8, bytes31>::into(1_u8).bits()), @31, 'bytes31 bits != 31');
}
