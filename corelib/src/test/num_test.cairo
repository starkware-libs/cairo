use test::test_utils::assert_eq;

use num::traits::{
    U8Bits, U16Bits, U32Bits, U64Bits, U128Bits, U256Bits, I8Bits, I16Bits, I32Bits, I64Bits,
    I128Bits, Bytes31Bits
};

#[test]
fn test_traits_bits() {
    assert_eq(@(U8Bits::bits()), @8, 'U8 bits != 8');
    assert_eq(@(U16Bits::bits()), @16, 'U16 bits != 16');
    assert_eq(@(U32Bits::bits()), @32, 'U32 bits != 32');
    assert_eq(@(U64Bits::bits()), @64, 'U64 bits != 64');
    assert_eq(@(U128Bits::bits()), @128, 'U128 bits != 128');
    assert_eq(@(U256Bits::bits()), @256, 'U256 bits != 256');
    assert_eq(@(I8Bits::bits()), @8, 'I8 bits != 8');
    assert_eq(@(I16Bits::bits()), @16, 'I16 bits != 16');
    assert_eq(@(I32Bits::bits()), @32, 'I32 bits != 32');
    assert_eq(@(I64Bits::bits()), @64, 'I64 bits != 64');
    assert_eq(@(I128Bits::bits()), @128, 'I128 bits != 128');
    assert_eq(@(Bytes31Bits::bits()), @248, 'Bytes31 bits != 248');
}
