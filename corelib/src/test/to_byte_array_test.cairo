use test::test_utils::assert_eq;
use to_byte_array::{FormatAsByteArray, AppendFormattedToByteArray};

#[test]
#[available_gas(10000000)]
fn test_to_string_hex() {
    let hex_base: NonZero<u8> = 16_u8.try_into().unwrap();

    let expected_string = "0";
    let serialized = 0_u8.format_as_byte_array(hex_base);
    assert_eq(@serialized, @expected_string, 'Bad hex representation of 0');

    let expected_string = "a";
    let serialized = 0xa_u8.format_as_byte_array(hex_base);
    assert_eq(@serialized, @expected_string, 'Bad hex representation of 10');

    let expected_string = "6f";
    let serialized = 0x6f_u8.format_as_byte_array(hex_base);
    assert_eq(@serialized, @expected_string, 'Bad hex representation of 111');

    let expected_string = "ff";
    let serialized = 0xff_u8.format_as_byte_array(hex_base);
    assert_eq(@serialized, @expected_string, 'Bad hex representation of 255');

    // Other uint types:
    let expected_string = "6f";
    let serialized = 0x6f_u16.format_as_byte_array(base: 16_u16.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u16 hex representation');
    let serialized = 0x6f_u32.format_as_byte_array(base: 16_u32.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u32 hex representation');
    let serialized = 0x6f_u64.format_as_byte_array(base: 16_u64.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u64 hex representation');
    let serialized = 0x6f_u128.format_as_byte_array(base: 16_u128.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u128 hex representation');
    let serialized = 0x6f_u256.format_as_byte_array(base: 16_u256.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u256 hex representation');
    let serialized = 0x6f_felt252.format_as_byte_array(base: 16.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad felt252 hex representation');
}
#[test]
#[available_gas(10000000)]
fn test_to_string_dec() {
    let dec_base: NonZero<u8> = 10_u8.try_into().unwrap();

    let expected_string = "0";
    let serialized = 0_u8.format_as_byte_array(dec_base);
    assert_eq(@serialized, @expected_string, 'Bad dec representation of 0');

    let expected_string = "10";
    let serialized = 10_u8.format_as_byte_array(dec_base);
    assert_eq(@serialized, @expected_string, 'Bad dec representation of 10');

    let expected_string = "111";
    let serialized = 111_u8.format_as_byte_array(dec_base);
    assert_eq(@serialized, @expected_string, 'Bad dec representation of 111');

    let expected_string = "255";
    let serialized = 255_u8.format_as_byte_array(dec_base);
    assert_eq(@serialized, @expected_string, 'Bad dec representation of 255');

    // Other uint types:
    let expected_string = "111";
    let serialized = 111_u16.format_as_byte_array(base: 10_u16.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u16 dec representation');
    let serialized = 111_u32.format_as_byte_array(base: 10_u32.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u32 dec representation');
    let serialized = 111_u64.format_as_byte_array(base: 10_u64.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u64 dec representation');
    let serialized = 111_u128.format_as_byte_array(base: 10_u128.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u128 dec representation');
    let serialized = 111_u256.format_as_byte_array(base: 10_u256.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u256 dec representation');
    let serialized = 111_felt252.format_as_byte_array(base: 10.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad felt252 dec representation');
}

#[test]
#[available_gas(10000000)]
fn test_to_string_oct() {
    let oct_base: NonZero<u8> = 8_u8.try_into().unwrap();

    let expected_string = "0";
    let serialized = 0_u8.format_as_byte_array(oct_base);
    assert_eq(@serialized, @expected_string, 'Bad oct representation of 0');

    let expected_string = "12";
    let serialized = 10_u8.format_as_byte_array(oct_base);
    assert_eq(@serialized, @expected_string, 'Bad oct representation of 10');

    let expected_string = "157";
    let serialized = 111_u8.format_as_byte_array(oct_base);
    assert_eq(@serialized, @expected_string, 'Bad oct representation of 111');

    let expected_string = "377";
    let serialized = 255_u8.format_as_byte_array(oct_base);
    assert_eq(@serialized, @expected_string, 'Bad oct representation of 255');

    // Other uint types:
    let expected_string = "157";
    let serialized = 111_u16.format_as_byte_array(base: 8_u16.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u16 oct representation');
    let serialized = 111_u32.format_as_byte_array(base: 8_u32.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u32 oct representation');
    let serialized = 111_u64.format_as_byte_array(base: 8_u64.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u64 oct representation');
    let serialized = 111_u128.format_as_byte_array(base: 8_u128.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u128 oct representation');
    let serialized = 111_u256.format_as_byte_array(base: 8_u256.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u256 oct representation');
    let serialized = 111_felt252.format_as_byte_array(base: 8.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad felt252 oct representation');
}

#[test]
#[available_gas(10000000)]
fn test_to_string_bin() {
    let bin_base: NonZero<u8> = 2_u8.try_into().unwrap();

    let expected_string = "0";
    let serialized = 0_u8.format_as_byte_array(bin_base);
    assert_eq(@serialized, @expected_string, 'Bad bin representation of 0');

    let expected_string = "1010";
    let serialized = 10_u8.format_as_byte_array(bin_base);
    assert_eq(@serialized, @expected_string, 'Bad bin representation of 10');

    let expected_string = "1101111";
    let serialized = 111_u8.format_as_byte_array(bin_base);
    assert_eq(@serialized, @expected_string, 'Bad bin representation of 111');

    let expected_string = "11111111";
    let serialized = 255_u8.format_as_byte_array(bin_base);
    assert_eq(@serialized, @expected_string, 'Bad bin representation of 255');

    // Other uint types:
    let expected_string = "1101111";
    let serialized = 111_u16.format_as_byte_array(base: 2_u16.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u16 bin representation');
    let serialized = 111_u32.format_as_byte_array(base: 2_u32.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u32 bin representation');
    let serialized = 111_u64.format_as_byte_array(base: 2_u64.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u64 bin representation');
    let serialized = 111_u128.format_as_byte_array(base: 2_u128.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u128 bin representation');
    let serialized = 111_u256.format_as_byte_array(base: 2_u256.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u256 bin representation');
    let serialized = 111_felt252.format_as_byte_array(base: 2.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad felt252 bin representation');
}

#[test]
#[available_gas(10000000)]
fn test_to_string_base7() {
    let base7: NonZero<u8> = 7_u8.try_into().unwrap();

    let expected_string = "0";
    let serialized = 0_u8.format_as_byte_array(base7);
    assert_eq(@serialized, @expected_string, 'Bad b7 representation of 0');

    let expected_string = "13";
    let serialized = 10_u8.format_as_byte_array(base7);
    assert_eq(@serialized, @expected_string, 'Bad b7 representation of 10');

    let expected_string = "216";
    let serialized = 111_u8.format_as_byte_array(base7);
    assert_eq(@serialized, @expected_string, 'Bad b7 representation of 111');

    let expected_string = "513";
    let serialized = 255_u8.format_as_byte_array(base7);
    assert_eq(@serialized, @expected_string, 'Bad b7 representation of 255');

    // Other uint types:
    let expected_string = "216";
    let serialized = 111_u16.format_as_byte_array(base: 7_u16.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u16 b7 representation');
    let serialized = 111_u32.format_as_byte_array(base: 7_u32.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u32 b7 representation');
    let serialized = 111_u64.format_as_byte_array(base: 7_u64.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u64 b7 representation');
    let serialized = 111_u128.format_as_byte_array(base: 7_u128.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u128 b7 representation');
    let serialized = 111_u256.format_as_byte_array(base: 7_u256.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u256 b7 representation');
    let serialized = 111_felt252.format_as_byte_array(base: 7.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad felt252 b7 representation');
}

#[test]
#[available_gas(10000000)]
fn test_to_string_base36() {
    let base36: NonZero<u8> = 36_u8.try_into().unwrap();

    let expected_string = "0";
    let serialized = 0_u8.format_as_byte_array(base36);
    assert_eq(@serialized, @expected_string, 'Bad b36 representation of 0');

    let expected_string = "a";
    let serialized = 10_u8.format_as_byte_array(base36);
    assert_eq(@serialized, @expected_string, 'Bad b36 representation of 10');

    let expected_string = "3z";
    let serialized = 143_u8.format_as_byte_array(base36);
    assert_eq(@serialized, @expected_string, 'Bad b36 representation of 143');

    let expected_string = "73";
    let serialized = 255_u8.format_as_byte_array(base36);
    assert_eq(@serialized, @expected_string, 'Bad b36 representation of 255');

    // Other uint types:
    let expected_string = "3z";
    let serialized = 143_u16.format_as_byte_array(base: 36_u16.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u16 b36 representation');
    let serialized = 143_u32.format_as_byte_array(base: 36_u32.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u32 b36 representation');
    let serialized = 143_u64.format_as_byte_array(base: 36_u64.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u64 b36 representation');
    let serialized = 143_u128.format_as_byte_array(base: 36_u128.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u128 b36 representation');
    let serialized = 143_u256.format_as_byte_array(base: 36_u256.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad u256 b36 representation');
    let serialized = 143_felt252.format_as_byte_array(base: 36.try_into().unwrap());
    assert_eq(@serialized, @expected_string, 'Bad felt252 b36 representation');
}

#[test]
#[available_gas(10000000)]
fn test_append() {
    let expected = "prefix_6f";
    let mut byte_array = "prefix_";
    111_u8.append_formatted_to_byte_array(ref byte_array, base: 16_u8.try_into().unwrap());
    assert_eq(@byte_array, @expected, 'Bad append u8');

    let mut byte_array = "prefix_";
    111_u16.append_formatted_to_byte_array(ref byte_array, base: 16_u16.try_into().unwrap());
    assert_eq(@byte_array, @expected, 'Bad append u16');

    let mut byte_array = "prefix_";
    111_u32.append_formatted_to_byte_array(ref byte_array, base: 16_u32.try_into().unwrap());
    assert_eq(@byte_array, @expected, 'Bad append u32');

    let mut byte_array = "prefix_";
    111_u64.append_formatted_to_byte_array(ref byte_array, base: 16_u64.try_into().unwrap());
    assert_eq(@byte_array, @expected, 'Bad append u64');

    let mut byte_array = "prefix_";
    111_u128.append_formatted_to_byte_array(ref byte_array, base: 16_u128.try_into().unwrap());
    assert_eq(@byte_array, @expected, 'Bad append u128');

    let mut byte_array = "prefix_";
    111_u256.append_formatted_to_byte_array(ref byte_array, base: 16_u256.try_into().unwrap());
    assert_eq(@byte_array, @expected, 'Bad append u256');

    let mut byte_array = "prefix_";
    111.append_formatted_to_byte_array(ref byte_array, base: 16.try_into().unwrap());
    assert_eq(@byte_array, @expected, 'Bad append felt252');
}
