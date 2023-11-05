use core::to_byte_array::{FormatAsByteArray, AppendFormattedToByteArray};

#[test]
fn test_to_string_hex() {
    let hex_base: NonZero<u8> = 16_u8.try_into().unwrap();
    assert_eq!(0_u8.format_as_byte_array(hex_base), "0");
    assert_eq!(0xa_u8.format_as_byte_array(hex_base), "a");
    assert_eq!(0x6f_u8.format_as_byte_array(hex_base), "6f");
    assert_eq!(0xff_u8.format_as_byte_array(hex_base), "ff");

    // Other uint types:
    assert_eq!(0x6f_u16.format_as_byte_array(16_u16.try_into().unwrap()), "6f");
    assert_eq!(0x6f_u32.format_as_byte_array(16_u32.try_into().unwrap()), "6f");
    assert_eq!(0x6f_u64.format_as_byte_array(16_u64.try_into().unwrap()), "6f");
    assert_eq!(0x6f_u128.format_as_byte_array(16_u128.try_into().unwrap()), "6f");
    assert_eq!(0x6f_u256.format_as_byte_array(16_u256.try_into().unwrap()), "6f");
    assert_eq!(0x6f_felt252.format_as_byte_array(16.try_into().unwrap()), "6f");
}
#[test]
fn test_to_string_dec() {
    let dec_base: NonZero<u8> = 10_u8.try_into().unwrap();
    assert_eq!(0_u8.format_as_byte_array(dec_base), "0");
    assert_eq!(10_u8.format_as_byte_array(dec_base), "10");
    assert_eq!(111_u8.format_as_byte_array(dec_base), "111");
    assert_eq!(255_u8.format_as_byte_array(dec_base), "255");

    // Other uint types:
    assert_eq!(111_u16.format_as_byte_array(10_u16.try_into().unwrap()), "111");
    assert_eq!(111_u32.format_as_byte_array(10_u32.try_into().unwrap()), "111");
    assert_eq!(111_u64.format_as_byte_array(10_u64.try_into().unwrap()), "111");
    assert_eq!(111_u128.format_as_byte_array(10_u128.try_into().unwrap()), "111");
    assert_eq!(111_u256.format_as_byte_array(10_u256.try_into().unwrap()), "111");
    assert_eq!(111_felt252.format_as_byte_array(10.try_into().unwrap()), "111");
}

#[test]
fn test_to_string_oct() {
    let oct_base: NonZero<u8> = 8_u8.try_into().unwrap();
    assert_eq!(0_u8.format_as_byte_array(oct_base), "0");
    assert_eq!(10_u8.format_as_byte_array(oct_base), "12");
    assert_eq!(111_u8.format_as_byte_array(oct_base), "157");
    assert_eq!(255_u8.format_as_byte_array(oct_base), "377");

    // Other uint types:
    assert_eq!(111_u16.format_as_byte_array(8_u16.try_into().unwrap()), "157");
    assert_eq!(111_u32.format_as_byte_array(8_u32.try_into().unwrap()), "157");
    assert_eq!(111_u64.format_as_byte_array(8_u64.try_into().unwrap()), "157");
    assert_eq!(111_u128.format_as_byte_array(8_u128.try_into().unwrap()), "157");
    assert_eq!(111_u256.format_as_byte_array(8_u256.try_into().unwrap()), "157");
    assert_eq!(111_felt252.format_as_byte_array(8.try_into().unwrap()), "157");
}

#[test]
fn test_to_string_bin() {
    let bin_base: NonZero<u8> = 2_u8.try_into().unwrap();
    assert_eq!(0_u8.format_as_byte_array(bin_base), "0");
    assert_eq!(10_u8.format_as_byte_array(bin_base), "1010");
    assert_eq!(111_u8.format_as_byte_array(bin_base), "1101111");
    assert_eq!(255_u8.format_as_byte_array(bin_base), "11111111");

    // Other uint types:
    assert_eq!(111_u16.format_as_byte_array(2_u16.try_into().unwrap()), "1101111");
    assert_eq!(111_u32.format_as_byte_array(2_u32.try_into().unwrap()), "1101111");
    assert_eq!(111_u64.format_as_byte_array(2_u64.try_into().unwrap()), "1101111");
    assert_eq!(111_u128.format_as_byte_array(2_u128.try_into().unwrap()), "1101111");
    assert_eq!(111_u256.format_as_byte_array(2_u256.try_into().unwrap()), "1101111");
    assert_eq!(111_felt252.format_as_byte_array(2.try_into().unwrap()), "1101111");
}

#[test]
fn test_to_string_base7() {
    let base7: NonZero<u8> = 7_u8.try_into().unwrap();
    assert_eq!(0_u8.format_as_byte_array(base7), "0");
    assert_eq!(10_u8.format_as_byte_array(base7), "13");
    assert_eq!(111_u8.format_as_byte_array(base7), "216");
    assert_eq!(255_u8.format_as_byte_array(base7), "513");

    // Other uint types:
    assert_eq!(111_u16.format_as_byte_array(7_u16.try_into().unwrap()), "216");
    assert_eq!(111_u32.format_as_byte_array(7_u32.try_into().unwrap()), "216");
    assert_eq!(111_u64.format_as_byte_array(7_u64.try_into().unwrap()), "216");
    assert_eq!(111_u128.format_as_byte_array(7_u128.try_into().unwrap()), "216");
    assert_eq!(111_u256.format_as_byte_array(7_u256.try_into().unwrap()), "216");
    assert_eq!(111_felt252.format_as_byte_array(7.try_into().unwrap()), "216");
}

#[test]
fn test_to_string_base36() {
    let base36: NonZero<u8> = 36_u8.try_into().unwrap();
    assert_eq!(0_u8.format_as_byte_array(base36), "0");
    assert_eq!(10_u8.format_as_byte_array(base36), "a");
    assert_eq!(143_u8.format_as_byte_array(base36), "3z");
    assert_eq!(255_u8.format_as_byte_array(base36), "73");

    // Other uint types:
    assert_eq!(143_u16.format_as_byte_array(36_u16.try_into().unwrap()), "3z");
    assert_eq!(143_u32.format_as_byte_array(36_u32.try_into().unwrap()), "3z");
    assert_eq!(143_u64.format_as_byte_array(36_u64.try_into().unwrap()), "3z");
    assert_eq!(143_u128.format_as_byte_array(36_u128.try_into().unwrap()), "3z");
    assert_eq!(143_u256.format_as_byte_array(36_u256.try_into().unwrap()), "3z");
    assert_eq!(143_felt252.format_as_byte_array(36.try_into().unwrap()), "3z");
}

#[test]
fn test_append() {
    let expected = "prefix_6f";
    let mut byte_array = "prefix_";
    111_u8.append_formatted_to_byte_array(ref byte_array, base: 16_u8.try_into().unwrap());
    assert_eq!(byte_array, expected);

    let mut byte_array = "prefix_";
    111_u16.append_formatted_to_byte_array(ref byte_array, base: 16_u16.try_into().unwrap());
    assert_eq!(byte_array, expected);

    let mut byte_array = "prefix_";
    111_u32.append_formatted_to_byte_array(ref byte_array, base: 16_u32.try_into().unwrap());
    assert_eq!(byte_array, expected);

    let mut byte_array = "prefix_";
    111_u64.append_formatted_to_byte_array(ref byte_array, base: 16_u64.try_into().unwrap());
    assert_eq!(byte_array, expected);

    let mut byte_array = "prefix_";
    111_u128.append_formatted_to_byte_array(ref byte_array, base: 16_u128.try_into().unwrap());
    assert_eq!(byte_array, expected);

    let mut byte_array = "prefix_";
    111_u256.append_formatted_to_byte_array(ref byte_array, base: 16_u256.try_into().unwrap());
    assert_eq!(byte_array, expected);

    let mut byte_array = "prefix_";
    111.append_formatted_to_byte_array(ref byte_array, base: 16.try_into().unwrap());
    assert_eq!(byte_array, expected);
}
