use test::test_utils::assert_eq;

#[test]
#[available_gas(10000000)]
fn test_to_string_hex_u8() {
    let hex_base: NonZero<u8> = 16_u8.try_into().unwrap();

    let expected_string = "0x0";
    let serialized: ByteArray = 0_u8.format_as_byte_array(hex_base);
    assert_eq(@serialized, @expected_string, 'Bad hex representation of 0');

    let expected_string = "0xA";
    let serialized: ByteArray = 0xA_u8.format_as_byte_array(hex_base);
    assert_eq(@serialized, @expected_string, 'Bad hex representation of 10');

    let expected_string = "0x6F";
    let serialized: ByteArray = 0x6F_u8.format_as_byte_array(hex_base);
    assert_eq(@serialized, @expected_string, 'Bad hex representation of 111');

    let expected_string = "0xFF";
    let serialized: ByteArray = 0xFF_u8.format_as_byte_array(hex_base);
    assert_eq(@serialized, @expected_string, 'Bad hex representation of 255');

    // Other uint types:
    let expected_string = "0x6F";
    let serialized: ByteArray = 0x6F_u16.format_as_byte_array(base: 16_u16.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u16 hex representation');
    let serialized: ByteArray = 0x6F_u32.format_as_byte_array(base: 16_u32.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u32 hex representation');
    let serialized: ByteArray = 0x6F_u64.format_as_byte_array(base: 16_u64.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u64 hex representation');
    let serialized: ByteArray = 0x6F_u128.format_as_byte_array(base: 16_u128.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u128 hex representation');
    let serialized: ByteArray = 0x6F_u256.format_as_byte_array(base: 16_u256.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u256 hex representation');
    let serialized: ByteArray = 0x6F_felt252.format_as_byte_array(base: 16.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad felt252 hex representation');
}
#[test]
#[available_gas(10000000)]
fn test_to_string_dec() {
    let dec_base: NonZero<u8> = 10_u8.try_into().unwrap();

    // let expected_string = "0";
    // let serialized: ByteArray = 0_u8.format_as_byte_array(dec_base);
    // assert_eq(@serialized, @expected_string, 'Bad dec representation of 0');

    // let expected_string = "10";
    // let serialized: ByteArray = 10_u8.format_as_byte_array(dec_base);
    // assert_eq(@serialized, @expected_string, 'Bad dec representation of 10');

    let expected_string = "111";
    let serialized: ByteArray = 111_u8.format_as_byte_array(dec_base);
    assert_eq(@serialized, @expected_string, 'Bad dec representation of 111');
// let expected_string = "255";
// let serialized: ByteArray = 255_u8.format_as_byte_array(dec_base);
// assert_eq(@serialized, @expected_string, 'Bad dec representation of 255');

// // Other uint types:
// let expected_string = "111";
// let serialized: ByteArray = 111_u16.format_as_byte_array(base: 10_u16.try_into().unwrap());
// assert_eq(@serialized, @expected_string, 'Bad u16 dec representation');
// let serialized: ByteArray = 111_u32.format_as_byte_array(base: 10_u32.try_into().unwrap());
// assert_eq(@serialized, @expected_string, 'Bad u32 dec representation');
// let serialized: ByteArray = 111_u64.format_as_byte_array(base: 10_u64.try_into().unwrap());
// assert_eq(@serialized, @expected_string, 'Bad u64 dec representation');
// let serialized: ByteArray = 111_u128.format_as_byte_array(base: 10_u128.try_into().unwrap());
// assert_eq(@serialized, @expected_string, 'Bad u128 dec representation');
// let serialized: ByteArray = 111_u256.format_as_byte_array(base: 10_u256.try_into().unwrap());
// assert_eq(@serialized, @expected_string, 'Bad u256 dec representation');
// let serialized: ByteArray = 111_felt252.format_as_byte_array(base: 10.try_into().unwrap());
// assert_eq(@serialized, @expected_string, 'Bad felt252 dec representation');
}

#[test]
#[available_gas(10000000)]
fn test_to_string_oct() {
    let oct_base: NonZero<u8> = 8_u8.try_into().unwrap();

    let expected_string = "0o0";
    let serialized: ByteArray = 0_u8.format_as_byte_array(oct_base);
    assert_eq(@serialized, @expected_string, 'Bad oct representation of 0');

    let expected_string = "0o12";
    let serialized: ByteArray = 10_u8.format_as_byte_array(oct_base);
    assert_eq(@serialized, @expected_string, 'Bad oct representation of 10');

    let expected_string = "0o157";
    let serialized: ByteArray = 111_u8.format_as_byte_array(oct_base);
    assert_eq(@serialized, @expected_string, 'Bad oct representation of 111');

    let expected_string = "0o377";
    let serialized: ByteArray = 255_u8.format_as_byte_array(oct_base);
    assert_eq(@serialized, @expected_string, 'Bad oct representation of 255');

    // Other uint types:
    let expected_string = "0o157";
    let serialized: ByteArray = 111_u16.format_as_byte_array(base: 8_u16.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u16 oct representation');
    let serialized: ByteArray = 111_u32.format_as_byte_array(base: 8_u32.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u32 oct representation');
    let serialized: ByteArray = 111_u64.format_as_byte_array(base: 8_u64.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u64 oct representation');
    let serialized: ByteArray = 111_u128.format_as_byte_array(base: 8_u128.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u128 oct representation');
    let serialized: ByteArray = 111_u256.format_as_byte_array(base: 8_u256.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u256 oct representation');
    let serialized: ByteArray = 111_felt252.format_as_byte_array(base: 8.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad felt252 oct representation');
}

#[test]
#[available_gas(10000000)]
fn test_to_string_bin() {
    let bin_base: NonZero<u8> = 2_u8.try_into().unwrap();

    let expected_string = "0b0";
    let serialized: ByteArray = 0_u8.format_as_byte_array(bin_base);
    assert_eq(@serialized, @expected_string, 'Bad bin representation of 0');

    let expected_string = "0b1010";
    let serialized: ByteArray = 10_u8.format_as_byte_array(bin_base);
    assert_eq(@serialized, @expected_string, 'Bad bin representation of 10');

    let expected_string = "0b1101111";
    let serialized: ByteArray = 111_u8.format_as_byte_array(bin_base);
    assert_eq(@serialized, @expected_string, 'Bad bin representation of 111');

    let expected_string = "0b11111111";
    let serialized: ByteArray = 255_u8.format_as_byte_array(bin_base);
    assert_eq(@serialized, @expected_string, 'Bad bin representation of 255');

    // Other uint types:
    let expected_string = "0b1101111";
    let serialized: ByteArray = 111_u16.format_as_byte_array(base: 2_u16.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u16 bin representation');
    let serialized: ByteArray = 111_u32.format_as_byte_array(base: 2_u32.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u32 bin representation');
    let serialized: ByteArray = 111_u64.format_as_byte_array(base: 2_u64.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u64 bin representation');
    let serialized: ByteArray = 111_u128.format_as_byte_array(base: 2_u128.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u128 bin representation');
    let serialized: ByteArray = 111_u256.format_as_byte_array(base: 2_u256.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u256 bin representation');
    let serialized: ByteArray = 111_felt252.format_as_byte_array(base: 2.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad felt252 bin representation');
}

#[test]
#[available_gas(10000000)]
fn test_to_string_base7() {
    let base7: NonZero<u8> = 7_u8.try_into().unwrap();

    let expected_string = "0";
    let serialized: ByteArray = 0_u8.format_as_byte_array(base7);
    assert_eq(@serialized, @expected_string, 'Bad b7 representation of 0');

    let expected_string = "13";
    let serialized: ByteArray = 10_u8.format_as_byte_array(base7);
    assert_eq(@serialized, @expected_string, 'Bad b7 representation of 10');

    let expected_string = "216";
    let serialized: ByteArray = 111_u8.format_as_byte_array(base7);
    assert_eq(@serialized, @expected_string, 'Bad b7 representation of 111');

    let expected_string = "513";
    let serialized: ByteArray = 255_u8.format_as_byte_array(base7);
    assert_eq(@serialized, @expected_string, 'Bad b7 representation of 255');

    // Other uint types:
    let expected_string = "216";
    let serialized: ByteArray = 111_u16.format_as_byte_array(base: 7_u16.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u16 b7 representation');
    let serialized: ByteArray = 111_u32.format_as_byte_array(base: 7_u32.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u32 b7 representation');
    let serialized: ByteArray = 111_u64.format_as_byte_array(base: 7_u64.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u64 b7 representation');
    let serialized: ByteArray = 111_u128.format_as_byte_array(base: 7_u128.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u128 b7 representation');
    let serialized: ByteArray = 111_u256.format_as_byte_array(base: 7_u256.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u256 b7 representation');
    let serialized: ByteArray = 111_felt252.format_as_byte_array(base: 7.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad felt252 b7 representation');
}

#[test]
#[available_gas(10000000)]
fn test_to_string_base36() {
    let base36: NonZero<u8> = 36_u8.try_into().unwrap();

    let expected_string = "0";
    let serialized: ByteArray = 0_u8.format_as_byte_array(base36);
    assert_eq(@serialized, @expected_string, 'Bad b36 representation of 0');

    let expected_string = "A";
    let serialized: ByteArray = 10_u8.format_as_byte_array(base36);
    assert_eq(@serialized, @expected_string, 'Bad b36 representation of 10');

    let expected_string = "3Z";
    let serialized: ByteArray = 143_u8.format_as_byte_array(base36);
    assert_eq(@serialized, @expected_string, 'Bad b36 representation of 143');

    let expected_string = "73";
    let serialized: ByteArray = 255_u8.format_as_byte_array(base36);
    assert_eq(@serialized, @expected_string, 'Bad b36 representation of 255');

    // Other uint types:
    let expected_string = "3Z";
    let serialized: ByteArray = 143_u16.format_as_byte_array(base: 36_u16.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u16 b36 representation');
    let serialized: ByteArray = 143_u32.format_as_byte_array(base: 36_u32.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u32 b36 representation');
    let serialized: ByteArray = 143_u64.format_as_byte_array(base: 36_u64.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u64 b36 representation');
    let serialized: ByteArray = 143_u128.format_as_byte_array(base: 36_u128.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u128 b36 representation');
    let serialized: ByteArray = 143_u256.format_as_byte_array(base: 36_u256.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u256 b36 representation');
    let serialized: ByteArray = 143_felt252.format_as_byte_array(base: 36.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad felt252 b36 representation');
}
