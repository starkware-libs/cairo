use test::test_utils::assert_eq;

#[test]
#[available_gas(10000000)]
fn test_to_string_hex_u8() {
    let expected_string = "0x0";
    let serialized: ByteArray = 0_u8.format_as_byte_array_hex();
    assert_eq(@serialized, @expected_string, 'Bad hex representation of 0');

    let expected_string = "0xA";
    let serialized: ByteArray = 0xA_u8.format_as_byte_array_hex();
    assert_eq(@serialized, @expected_string, 'Bad hex representation of 10');

    let expected_string = "0x6F";
    let serialized: ByteArray = 0x6F_u8.format_as_byte_array_hex();
    assert_eq(@serialized, @expected_string, 'Bad hex representation of 111');

    let expected_string = "0xFF";
    let serialized: ByteArray = 0xFF_u8.format_as_byte_array_hex();
    assert_eq(@serialized, @expected_string, 'Bad hex representation of 255');

    // Other uint types:
    let expected_string = "0x6F";
    let serialized: ByteArray = 0x6F_u16.format_as_byte_array_hex();
    assert_eq(@serialized, @expected_string, 'Bad u16 hex representation');
    let serialized: ByteArray = 0x6F_u32.format_as_byte_array_hex();
    assert_eq(@serialized, @expected_string, 'Bad u32 hex representation');
    let serialized: ByteArray = 0x6F_u64.format_as_byte_array_hex();
    assert_eq(@serialized, @expected_string, 'Bad u64 hex representation');
    let serialized: ByteArray = 0x6F_u128.format_as_byte_array_hex();
    assert_eq(@serialized, @expected_string, 'Bad u128 hex representation');
    let serialized: ByteArray = 0x6F_u256.format_as_byte_array_hex();
    assert_eq(@serialized, @expected_string, 'Bad u256 hex representation');
    let serialized: ByteArray = 0x6F_felt252.format_as_byte_array_hex();
    assert_eq(@serialized, @expected_string, 'Bad felt252 hex representation');
}

#[test]
#[available_gas(10000000)]
fn test_to_string_dec() {
    let expected_string = "0";
    let serialized: ByteArray = 0_u8.format_as_byte_array_dec();
    assert_eq(@serialized, @expected_string, 'Bad dec representation of 0');

    let expected_string = "10";
    let serialized: ByteArray = 10_u8.format_as_byte_array_dec();
    assert_eq(@serialized, @expected_string, 'Bad dec representation of 10');

    let expected_string = "111";
    let serialized: ByteArray = 111_u8.format_as_byte_array_dec();
    assert_eq(@serialized, @expected_string, 'Bad dec representation of 111');

    let expected_string = "255";
    let serialized: ByteArray = 255_u8.format_as_byte_array_dec();
    assert_eq(@serialized, @expected_string, 'Bad dec representation of 255');

    // Other uint types:
    let expected_string = "111";
    let serialized: ByteArray = 111_u16.format_as_byte_array_dec();
    assert_eq(@serialized, @expected_string, 'Bad u16 dec representation');
    let serialized: ByteArray = 111_u32.format_as_byte_array_dec();
    assert_eq(@serialized, @expected_string, 'Bad u32 dec representation');
    let serialized: ByteArray = 111_u64.format_as_byte_array_dec();
    assert_eq(@serialized, @expected_string, 'Bad u64 dec representation');
    let serialized: ByteArray = 111_u128.format_as_byte_array_dec();
    assert_eq(@serialized, @expected_string, 'Bad u128 dec representation');
    let serialized: ByteArray = 111_u256.format_as_byte_array_dec();
    assert_eq(@serialized, @expected_string, 'Bad u256 dec representation');
    let serialized: ByteArray = 111_felt252.format_as_byte_array_dec();
    assert_eq(@serialized, @expected_string, 'Bad felt252 dec representation');
}

#[test]
#[available_gas(10000000)]
fn test_to_string_oct() {
    let expected_string = "0o0";
    let serialized: ByteArray = 0_u8.format_as_byte_array_oct();
    assert_eq(@serialized, @expected_string, 'Bad oct representation of 0');

    let expected_string = "0o12";
    let serialized: ByteArray = 10_u8.format_as_byte_array_oct();
    assert_eq(@serialized, @expected_string, 'Bad oct representation of 10');

    let expected_string = "0o157";
    let serialized: ByteArray = 111_u8.format_as_byte_array_oct();
    assert_eq(@serialized, @expected_string, 'Bad oct representation of 111');

    let expected_string = "0o377";
    let serialized: ByteArray = 255_u8.format_as_byte_array_oct();
    assert_eq(@serialized, @expected_string, 'Bad oct representation of 255');

    // Other uint types:
    let expected_string = "0o157";
    let serialized: ByteArray = 111_u16.format_as_byte_array_oct();
    assert_eq(@serialized, @expected_string, 'Bad u16 oct representation');
    let serialized: ByteArray = 111_u32.format_as_byte_array_oct();
    assert_eq(@serialized, @expected_string, 'Bad u32 oct representation');
    let serialized: ByteArray = 111_u64.format_as_byte_array_oct();
    assert_eq(@serialized, @expected_string, 'Bad u64 oct representation');
    let serialized: ByteArray = 111_u128.format_as_byte_array_oct();
    assert_eq(@serialized, @expected_string, 'Bad u128 oct representation');
    let serialized: ByteArray = 111_u256.format_as_byte_array_oct();
    assert_eq(@serialized, @expected_string, 'Bad u256 oct representation');
    let serialized: ByteArray = 111_felt252.format_as_byte_array_oct();
    assert_eq(@serialized, @expected_string, 'Bad felt252 oct representation');
}

#[test]
#[available_gas(10000000)]
fn test_to_string_bin() {
    let expected_string = "0b0";
    let serialized: ByteArray = 0_u8.format_as_byte_array_bin();
    assert_eq(@serialized, @expected_string, 'Bad bin representation of 0');

    let expected_string = "0b1010";
    let serialized: ByteArray = 10_u8.format_as_byte_array_bin();
    assert_eq(@serialized, @expected_string, 'Bad bin representation of 10');

    let expected_string = "0b1101111";
    let serialized: ByteArray = 111_u8.format_as_byte_array_bin();
    assert_eq(@serialized, @expected_string, 'Bad bin representation of 111');

    let expected_string = "0b11111111";
    let serialized: ByteArray = 255_u8.format_as_byte_array_bin();
    assert_eq(@serialized, @expected_string, 'Bad bin representation of 255');

    // Other uint types:
    let expected_string = "0b1101111";
    let serialized: ByteArray = 111_u16.format_as_byte_array_bin();
    assert_eq(@serialized, @expected_string, 'Bad u16 bin representation');
    let serialized: ByteArray = 111_u32.format_as_byte_array_bin();
    assert_eq(@serialized, @expected_string, 'Bad u32 bin representation');
    let serialized: ByteArray = 111_u64.format_as_byte_array_bin();
    assert_eq(@serialized, @expected_string, 'Bad u64 bin representation');
    let serialized: ByteArray = 111_u128.format_as_byte_array_bin();
    assert_eq(@serialized, @expected_string, 'Bad u128 bin representation');
    let serialized: ByteArray = 111_u256.format_as_byte_array_bin();
    assert_eq(@serialized, @expected_string, 'Bad u256 bin representation');
    let serialized: ByteArray = 111_felt252.format_as_byte_array_bin();
    assert_eq(@serialized, @expected_string, 'Bad felt252 bin representation');
}
