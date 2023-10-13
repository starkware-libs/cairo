use test::test_utils::assert_eq;

use bits_length::{
    U8BitsLength, U16BitsLength, U32BitsLength, U64BitsLength, U128BitsLength, U256BitsLength,
    I8BitsLength, I16BitsLength, I32BitsLength, I64BitsLength, I128BitsLength, Felt252BitsLength,
    Bytes31BitsLength
};

#[test]
fn test_bits_length() {
    assert_eq(@(U8BitsLength::bits()), @8, 'U8 bits != 8');
    assert_eq(@(U16BitsLength::bits()), @16, 'U16 bits != 16');
    assert_eq(@(U32BitsLength::bits()), @32, 'U32 bits != 32');
    assert_eq(@(U64BitsLength::bits()), @64, 'U64 bits != 64');
    assert_eq(@(U128BitsLength::bits()), @128, 'U128 bits != 128');
    assert_eq(@(U256BitsLength::bits()), @256, 'U256 bits != 256');
    assert_eq(@(I8BitsLength::bits()), @8, 'I8 bits != 8');
    assert_eq(@(I16BitsLength::bits()), @16, 'I16 bits != 16');
    assert_eq(@(I32BitsLength::bits()), @32, 'I32 bits != 32');
    assert_eq(@(I64BitsLength::bits()), @64, 'I64 bits != 64');
    assert_eq(@(I128BitsLength::bits()), @128, 'I128 bits != 128');
    assert_eq(@(Felt252BitsLength::bits()), @252, 'Felt252 bits != 252');
    assert_eq(@(Bytes31BitsLength::bits()), @31_u8.into(), 'Bytes31 bits != 31');
}

#[test]
fn test_bits_length_self() {
    assert_eq(@(1_u8.bits_len()), @8, 'u8 bits len != 8');
    assert_eq(@(1_u16.bits_len()), @16, 'u16 bits len != 16');
    assert_eq(@(1_u32.bits_len()), @32, 'u32 bits len != 32');
    assert_eq(@(1_u64.bits_len()), @64, 'u64 bits len != 64');
    assert_eq(@(1_u128.bits_len()), @128, 'u128 bits len != 128');
    assert_eq(@(1_u256.bits_len()), @256, 'u256 bits len != 256');
    assert_eq(@(1_i8.bits_len()), @8, 'i8 bits len != 8');
    assert_eq(@(1_i16.bits_len()), @16, 'i16 bits len != 16');
    assert_eq(@(1_i32.bits_len()), @32, 'i32 bits len != 32');
    assert_eq(@(1_i64.bits_len()), @64, 'i64 bits len != 64');
    assert_eq(@(1_i128.bits_len()), @128, 'i128 bits len != 128');
    assert_eq(@(1_felt252.bits_len()), @252, 'felt252 bits len != 252');
    assert_eq(
        @(bytes31_const::<1>().bits_len()), @(bytes31_const::<31>()), 'bytes31 bits len != 31'
    );
}
