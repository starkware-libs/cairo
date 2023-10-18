use bytes_31::Bytes31BitSize;
use integer::{
    U8BitSize, U16BitSize, U32BitSize, U64BitSize, U128BitSize, U256BitSize, I8BitSize, I16BitSize,
    I32BitSize, I64BitSize, I128BitSize,
};

use test::test_utils::assert_eq;


#[test]
fn test_bits_length() {
    assert_eq(@U8BitSize::bits(), @8, 'U8 bit size != 8');
    assert_eq(@U16BitSize::bits(), @16, 'U16 bit size != 16');
    assert_eq(@U32BitSize::bits(), @32, 'U32 bit size != 32');
    assert_eq(@U64BitSize::bits(), @64, 'U64 bit size != 64');
    assert_eq(@U128BitSize::bits(), @128, 'U128 bit size != 128');
    assert_eq(@U256BitSize::bits(), @256, 'U256 bit size != 256');
    assert_eq(@I8BitSize::bits(), @8, 'I8 bit size != 8');
    assert_eq(@I16BitSize::bits(), @16, 'I16 bit size != 16');
    assert_eq(@I32BitSize::bits(), @32, 'I32 bit size != 32');
    assert_eq(@I64BitSize::bits(), @64, 'I64 bit size != 64');
    assert_eq(@I128BitSize::bits(), @128, 'I128 bit size != 128');
    assert_eq(@Bytes31BitSize::bits(), @248, 'Bytes31 bit size != 248');
}
