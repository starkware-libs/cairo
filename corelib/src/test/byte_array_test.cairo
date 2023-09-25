use test::test_utils::{assert_eq, assert_ne};

#[test]
#[available_gas(1000000)]
fn test_append_byte() {
    let mut ba = Default::default();
    let mut c = 1_u8;
    loop {
        if c == 34 {
            break;
        }
        ba.append_byte(c);
        c += 1;
    };

    let expected_data = array![0x0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f];
    compare_byte_array(@ba, expected_data.span(), 2, 0x2021);
}

#[test]
#[available_gas(10000000)]
fn test_append_word() {
    let mut ba = Default::default();

    ba.append_word(0x0102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e, 30);
    compare_byte_array(
        @ba, array![].span(), 30, 0x0102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e
    );

    ba.append_word(0x1f2021, 3);
    let expected_data = array![0x0102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e1f];
    compare_byte_array(@ba, expected_data.span(), 2, 0x2021);

    ba.append_word(0x2223, 2);
    compare_byte_array(@ba, expected_data.span(), 4, 0x20212223);

    // Length is 0, so nothing is actually appended.
    ba.append_word(0xffee, 0);
    compare_byte_array(@ba, expected_data.span(), 4, 0x20212223);

    ba.append_word(0x2425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e, 27);
    let expected_data = array![
        0x0102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e1f,
        0x202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e
    ];
    compare_byte_array(@ba, expected_data.span(), 0, 0);

    ba.append_word(0x3f, 1);
    compare_byte_array(@ba, expected_data.span(), 1, 0x3f);
}

#[test]
#[available_gas(1000000)]
fn test_append() {
    let mut ba1 = test_byte_array_32();
    let ba2 = test_byte_array_32();

    ba1.append(@ba2);

    let expected_data = array![
        0x0102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e1f,
        0x200102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e
    ];
    compare_byte_array(@ba1, expected_data.span(), 2, 0x1f20);
}

// Same as test_append, but with `+=` instead of `append`.
#[test]
#[available_gas(1000000)]
fn test_add_eq() {
    let mut ba1 = test_byte_array_32();
    let ba2 = test_byte_array_32();

    ba1 += ba2;

    let expected_data = array![
        0x0102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e1f,
        0x200102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e
    ];
    compare_byte_array(@ba1, expected_data.span(), 2, 0x1f20);
}

#[test]
#[available_gas(1000000)]
fn test_concat() {
    let ba1 = test_byte_array_32();
    let ba2 = test_byte_array_32();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let expected_data = array![
        0x0102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e1f,
        0x200102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e
    ];
    compare_byte_array(@ba3, expected_data.span(), 2, 0x1f20);
}

// Same as test_concat, but with `+` instead of `concat`.
#[test]
#[available_gas(1000000)]
fn test_add() {
    let ba1 = test_byte_array_32();
    let ba2 = test_byte_array_32();

    let ba3 = ba1 + ba2;

    let expected_data = array![
        0x0102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e1f,
        0x200102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e
    ];
    compare_byte_array(@ba3, expected_data.span(), 2, 0x1f20);
}

// Test concat/append, first byte array empty.
#[test]
#[available_gas(1000000)]
fn test_concat_first_empty() {
    let ba1 = Default::default();
    let ba2 = test_byte_array_32();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let expected_data = array![0x0102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e1f];
    compare_byte_array(@ba3, expected_data.span(), 1, 0x20);
}

// Test concat/append, second byte array empty.
#[test]
#[available_gas(1000000)]
fn test_concat_second_empty() {
    let ba1 = test_byte_array_32();
    let ba2 = Default::default();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let expected_data = array![0x0102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e1f];
    compare_byte_array(@ba3, expected_data.span(), 1, 0x20);
}

// Test concat/append, first byte array pending word is empty.
#[test]
#[available_gas(1000000)]
fn test_concat_first_pending_0() {
    let ba1 = test_byte_array_31();
    let ba2 = test_byte_array_32();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let expected_data = array![
        0x0102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e1f,
        0x0102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e1f
    ];
    compare_byte_array(@ba3, expected_data.span(), 1, 0x20);
}

// Test concat/append, second byte array pending word is empty.
#[test]
#[available_gas(1000000)]
fn test_concat_second_pending_0() {
    let ba1 = test_byte_array_32();
    let ba2 = test_byte_array_31();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let expected_data = array![
        0x0102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e1f,
        0x200102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e
    ];
    compare_byte_array(@ba3, expected_data.span(), 1, 0x1f);
}

// Test concat/append, split index of the words of the second byte array is 16.
#[test]
#[available_gas(1000000)]
fn test_concat_split_index_16() {
    let ba1 = test_byte_array_16();
    let ba2 = test_byte_array_32();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let expected_data = array![0x0102030405060708091a0b0c0d0e0f100102030405060708091a0b0c0d0e0f];
    compare_byte_array(@ba3, expected_data.span(), 17, 0x101112131415161718191a1b1c1d1e1f20);
}

// Test concat/append, split index of the words of the second byte array is < 16, specifically 1.
#[test]
#[available_gas(1000000)]
fn test_concat_split_index_lt_16() {
    let ba1 = test_byte_array_1();
    let ba2 = test_byte_array_32();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let expected_data = array![0x010102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e];
    compare_byte_array(@ba3, expected_data.span(), 2, 0x1f20);
}

// Test concat/append, split index of the words of the second byte array is > 16, specifically 30.
#[test]
#[available_gas(1000000)]
fn test_concat_split_index_gt_16() {
    let ba1 = test_byte_array_30();
    let ba2 = test_byte_array_33();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let expected_data = array![
        0x0102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e01,
        0x02030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20
    ];
    compare_byte_array(@ba3, expected_data.span(), 1, 0x21);
}

// Sum of the lengths of the pending words of both byte arrays is 31 (a full word).
#[test]
#[available_gas(1000000)]
fn test_concat_pending_sum_up_to_full() {
    let ba1 = test_byte_array_32();
    let ba2 = test_byte_array_30();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let expected_data = array![
        0x0102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e1f,
        0x200102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e
    ];
    compare_byte_array(@ba3, expected_data.span(), 0, 0);
}

// Sum of the lengths of the pending words of both byte arrays is 31+16.
// That is, the pending words aggregate to a full word, and the last split index is 16.
#[test]
#[available_gas(1000000)]
fn test_concat_pending_sum_up_to_more_than_word_16() {
    let ba1 = test_byte_array_17();
    let ba2 = test_byte_array_30();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let expected_data = array![0x0102030405060708091a0b0c0d0e0f10110102030405060708091a0b0c0d0e];
    compare_byte_array(@ba3, expected_data.span(), 16, 0x0f101112131415161718191a1b1c1d1e);
}

// Sum of the lengths of the pending words of both byte arrays is in [32, 31+15].
// That is, the pending words aggregate to a full word, and the last split index is <16.
#[test]
#[available_gas(1000000)]
fn test_concat_pending_sum_up_to_more_than_word_lt16() {
    let ba1 = test_byte_array_2();
    let ba2 = test_byte_array_30();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let expected_data = array![0x01020102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d];
    compare_byte_array(@ba3, expected_data.span(), 1, 0x1e);
}

// Sum of the lengths of the pending words of both byte arrays is >31+15
// That is, the pending words aggregate to a full word, and the last split index is >16.
#[test]
#[available_gas(1000000)]
fn test_concat_pending_sum_up_to_more_than_word_gt16() {
    let ba1 = test_byte_array_30();
    let ba2 = test_byte_array_30();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let expected_data = array![0x0102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e01];
    compare_byte_array(
        @ba3, expected_data.span(), 29, 0x02030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e
    );
}

#[test]
#[available_gas(1000000)]
fn test_len() {
    let ba: ByteArray = Default::default();
    assert(ba.len() == 0, 'wrong ByteArray len');

    let mut ba = test_byte_array_33();
    assert(ba.len() == 33, 'wrong ByteArray len');

    ba.append(@test_byte_array_30());
    assert(ba.len() == 63, 'wrong ByteArray len');
}

#[test]
#[available_gas(100000000)]
fn test_at_empty() {
    let ba: ByteArray = Default::default();

    assert(ba.at(0) == Option::None, 'index 0 is not out of bounds');
    assert(ba.at(1) == Option::None, 'index 1 is not out of bounds');
    assert(ba.at(30) == Option::None, 'index 30 is not out of bounds');
    assert(ba.at(31) == Option::None, 'index 31 is not out of bounds');
}

#[test]
#[available_gas(100000000)]
fn test_at() {
    let mut ba = test_byte_array_31();
    ba.append(@test_byte_array_31());
    ba.append(@test_byte_array_17());

    assert(ba.at(0) == Option::Some(0x01), 'wrong byte at index 0');
    assert(ba.at(1) == Option::Some(0x02), 'wrong byte at index 1');
    assert(ba.at(2) == Option::Some(0x03), 'wrong byte at index 2');
    assert(ba.at(14) == Option::Some(0x0f), 'wrong byte at index 14');
    assert(ba.at(15) == Option::Some(0x10), 'wrong byte at index 15');
    assert(ba.at(16) == Option::Some(0x11), 'wrong byte at index 16');
    assert(ba.at(17) == Option::Some(0x12), 'wrong byte at index 17');
    assert(ba.at(29) == Option::Some(0x1e), 'wrong byte at index 29');
    assert(ba.at(30) == Option::Some(0x1f), 'wrong byte at index 30');
    assert(ba.at(31) == Option::Some(0x01), 'wrong byte at index 31');
    assert(ba.at(32) == Option::Some(0x02), 'wrong byte at index 32');
    assert(ba.at(61) == Option::Some(0x1f), 'wrong byte at index 61');
    assert(ba.at(62) == Option::Some(0x01), 'wrong byte at index 62');
    assert(ba.at(63) == Option::Some(0x02), 'wrong byte at index 63');
    assert(ba.at(76) == Option::Some(0x0f), 'wrong byte at index 76');
    assert(ba.at(77) == Option::Some(0x10), 'wrong byte at index 77');
    assert(ba.at(78) == Option::Some(0x11), 'wrong byte at index 78');
    assert(ba.at(79) == Option::None, 'index 79 is not out of bounds');
}

// Same as the previous test, but with [] instead of .at() (and without the out-of-bounds case).
#[test]
#[available_gas(100000000)]
fn test_index_view() {
    let mut ba = test_byte_array_31();
    ba.append(@test_byte_array_31());
    ba.append(@test_byte_array_17());

    assert(ba[0] == 0x01, 'wrong byte at index 0');
    assert(ba[1] == 0x02, 'wrong byte at index 1');
    assert(ba[2] == 0x03, 'wrong byte at index 2');
    assert(ba[14] == 0x0f, 'wrong byte at index 14');
    assert(ba[15] == 0x10, 'wrong byte at index 15');
    assert(ba[16] == 0x11, 'wrong byte at index 16');
    assert(ba[17] == 0x12, 'wrong byte at index 17');
    assert(ba[29] == 0x1e, 'wrong byte at index 29');
    assert(ba[30] == 0x1f, 'wrong byte at index 30');
    assert(ba[31] == 0x01, 'wrong byte at index 31');
    assert(ba[32] == 0x02, 'wrong byte at index 32');
    assert(ba[61] == 0x1f, 'wrong byte at index 61');
    assert(ba[62] == 0x01, 'wrong byte at index 62');
    assert(ba[63] == 0x02, 'wrong byte at index 63');
    assert(ba[76] == 0x0f, 'wrong byte at index 76');
    assert(ba[77] == 0x10, 'wrong byte at index 77');
    assert(ba[78] == 0x11, 'wrong byte at index 78');
}

// Test panic with [] in case of out-of-bounds
#[test]
#[should_panic(expected: ('Index out of bounds',))]
#[available_gas(100000000)]
fn test_index_view_out_of_bounds() {
    let mut ba = test_byte_array_31();
    ba.append(@test_byte_array_31());
    ba.append(@test_byte_array_17());

    let x = ba[79];
}

#[test]
fn test_string_literals() {
    let ba: ByteArray = "12345"; // len < 16
    let ba: ByteArray = "1234567890123456"; // len == 16
    let ba: ByteArray = "123456789012345678"; // 16 < len < 31
    let ba: ByteArray = "1234567890123456789012345678901"; // len == 31
    let ba: ByteArray = "123456789012345678901234567890123"; // 31 < len < 47
    let ba: ByteArray = "12345678901234567890123456789012345678901234567"; // len == 47
    let ba: ByteArray = "123456789012345678901234567890123456789012345678"; // len > 47
}

#[test]
#[available_gas(100000000)]
fn test_equality() {
    let byte_array: ByteArray = "a";
    assert(@byte_array == @"a", 'Same strings are not equal');
    assert(@byte_array != @"b", 'Different strings are equal');

    let mut ba1 = test_byte_array_2();
    ba1.append(@test_byte_array_31());
    let ba2 = test_byte_array_33();
    let ba3 = test_byte_array_32();
    let mut ba4 = test_byte_array_32();
    ba4.append(@test_byte_array_1());

    assert(@ba1 == @ba1, 'Same ByteArrays are not equal');
    assert(@ba2 == @ba2, 'Same ByteArrays are not equal');
    assert(@ba3 == @ba3, 'Same ByteArrays are not equal');
    assert(@ba4 == @ba4, 'Same ByteArrays are not equal');

    // Different data
    assert(@ba1 != @ba2, 'Different ByteArrays are equal');

    // Different pending word length
    assert(@ba2 != @ba3, 'Different ByteArrays are equal');

    // Different pending word
    assert(@ba2 != @ba4, 'Different ByteArrays are equal');
}

#[test]
#[available_gas(100000000)]
fn test_reverse() {
    // Arrays of length < 16
    let ba: ByteArray = "abc";
    let ba_rev: ByteArray = "cba";
    let palindrome: ByteArray = "rotator";
    assert_ne(@ba, @ba.rev(), 'ba == ba.rev()');
    assert_ne(@ba_rev, @ba_rev.rev(), 'ba_rev == ba_rev.rev()');
    assert_eq(@ba, @ba_rev.rev(), 'ba != ba_rev.rev()');
    assert_eq(@palindrome, @palindrome.rev(), 'palindrome is not a palindrome');

    // Arrays of length 16
    let ba: ByteArray = "my length is 16.";
    let ba_rev: ByteArray = ".61 si htgnel ym";
    let palindrome: ByteArray = "nolemon  nomelon";
    assert_ne(@ba, @ba.rev(), 'ba == ba.rev()');
    assert_ne(@ba_rev, @ba_rev.rev(), 'ba_rev == ba_rev.rev()');
    assert_eq(@ba, @ba_rev.rev(), 'ba != ba_rev.rev()');
    assert_eq(@palindrome, @palindrome.rev(), 'palindrome is not a palindrome');

    // Arrays of 16 < length < 31
    let ba: ByteArray = "I am a medium byte array";
    let ba_rev: ByteArray = "yarra etyb muidem a ma I";
    let palindrome: ByteArray = "nolemon  nomelon";
    assert_ne(@ba, @ba.rev(), 'ba == ba.rev()');
    assert_ne(@ba_rev, @ba_rev.rev(), 'ba_rev == ba_rev.rev()');
    assert_eq(@ba, @ba_rev.rev(), 'ba != ba_rev.rev()');
    assert_eq(@palindrome, @palindrome.rev(), 'palindrome is not a palindrome');

    // Arrays of length 31
    let ba: ByteArray = "I didn't find a good palindrome";
    let ba_rev: ByteArray = "emordnilap doog a dnif t'ndid I";
    let palindrome: ByteArray = "kayak level rotator level kayak";
    assert_ne(@ba, @ba.rev(), 'ba == ba.rev()');
    assert_ne(@ba_rev, @ba_rev.rev(), 'ba_rev == ba_rev.rev()');
    assert_eq(@ba, @ba_rev.rev(), 'ba != ba_rev.rev()');
    assert_eq(@palindrome, @palindrome.rev(), 'palindrome is not a palindrome');

    // Arrays of 31 < length < 47 (31+16)
    let ba: ByteArray = "This time I did find a good palindrome!";
    let ba_rev: ByteArray = "!emordnilap doog a dnif did I emit sihT";
    let palindrome: ByteArray = "noitneverpropagatesifisetagaporprevention";
    assert_ne(@ba, @ba.rev(), 'ba == ba.rev()');
    assert_ne(@ba_rev, @ba_rev.rev(), 'ba_rev == ba_rev.rev()');
    assert_eq(@ba, @ba_rev.rev(), 'ba != ba_rev.rev()');
    assert_eq(@palindrome, @palindrome.rev(), 'palindrome is not a palindrome');

    // Arrays of length 47 (31+16)
    let ba: ByteArray = "I have found a palindrome, exactly 47 in length";
    let ba_rev: ByteArray = "htgnel ni 74 yltcaxe ,emordnilap a dnuof evah I";
    let palindrome: ByteArray = "onacloverifaliveeruptsavastpureevilafirevolcano";
    assert_ne(@ba, @ba.rev(), 'ba == ba.rev()');
    assert_ne(@ba_rev, @ba_rev.rev(), 'ba_rev == ba_rev.rev()');
    assert_eq(@ba, @ba_rev.rev(), 'ba != ba_rev.rev()');
    assert_eq(@palindrome, @palindrome.rev(), 'palindrome is not a palindrome');

    // Arrays of length > 47 (31+16)
    let ba: ByteArray = "This palindrome is not as good, but at least it's long enough";
    let ba_rev: ByteArray = "hguone gnol s'ti tsael ta tub ,doog sa ton si emordnilap sihT";
    let palindrome: ByteArray = "docnoteidissentafastneverpreventsafatnessidietoncod";
    assert_ne(@ba, @ba.rev(), 'ba == ba.rev()');
    assert_ne(@ba_rev, @ba_rev.rev(), 'ba_rev == ba_rev.rev()');
    assert_eq(@ba, @ba_rev.rev(), 'ba != ba_rev.rev()');
    assert_eq(@palindrome, @palindrome.rev(), 'palindrome is not a palindrome');
}

// ========= Test helper functions =========

use debug::PrintTrait;
fn compare_byte_array(
    mut ba: @ByteArray, mut data: Span<felt252>, pending_word_len: usize, pending_word: felt252
) {
    assert(ba.data.len() == data.len(), 'wrong data len');
    let mut ba_data = ba.data.span();

    let mut data_index = 0;
    loop {
        match ba_data.pop_front() {
            Option::Some(x) => {
                let actual_word = (*x).into();
                let expected_word = *data.pop_front().unwrap();
                if actual_word != expected_word {
                    'data_index:'.print();
                    data_index.print();
                    'expected word:'.print();
                    expected_word.print();
                    'actual word:'.print();
                    actual_word.print();

                    panic_with_felt252('wrong data');
                }
            },
            Option::None(_) => { break; }
        }
        data_index += 1;
    };

    if *ba.pending_word_len != pending_word_len {
        'expected pending word len:'.print();
        pending_word_len.print();
        'actual pending word len:'.print();
        (*ba.pending_word_len).print();
        panic_with_felt252('wrong pending_word_len');
    }
    let ba_pending_word_felt: felt252 = (*ba.pending_word).into();
    if ba_pending_word_felt != pending_word {
        'expected pending word:'.print();
        pending_word.print();
        'actual pending word:'.print();
        ba_pending_word_felt.print();
        panic_with_felt252('wrong pending_word');
    }
}

fn test_byte_array_1() -> ByteArray {
    let mut ba1 = Default::default();
    ba1.append_word(0x01, 1);
    ba1
}

fn test_byte_array_2() -> ByteArray {
    let mut ba1 = Default::default();
    ba1.append_word(0x0102, 2);
    ba1
}

fn test_byte_array_16() -> ByteArray {
    let mut ba1 = Default::default();
    ba1.append_word(0x0102030405060708091a0b0c0d0e0f10, 16);
    ba1
}

fn test_byte_array_17() -> ByteArray {
    let mut ba1 = Default::default();
    ba1.append_word(0x0102030405060708091a0b0c0d0e0f1011, 17);
    ba1
}

fn test_byte_array_30() -> ByteArray {
    let mut ba1 = Default::default();
    ba1.append_word(0x0102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e, 30);
    ba1
}

fn test_byte_array_31() -> ByteArray {
    let mut ba1 = Default::default();
    ba1.append_word(0x0102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e1f, 31);
    ba1
}

fn test_byte_array_32() -> ByteArray {
    let mut ba1 = Default::default();
    ba1.append_word(0x0102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e1f, 31);
    ba1.append_word(0x20, 1);
    ba1
}

fn test_byte_array_33() -> ByteArray {
    let mut ba2 = Default::default();
    ba2.append_word(0x0102030405060708091a0b0c0d0e0f101112131415161718191a1b1c1d1e1f, 31);
    ba2.append_word(0x2021, 2);
    ba2
}
