use array::{ArrayTrait, SpanTrait};
use byte_array::ByteArrayTrait;
use bytes_31::Bytes31IntoFelt252;
use option::OptionTrait;
use traits::Into;

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

    let mut expected_data: Array<felt252> = Default::default();
    expected_data.append(0x1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201);
    compare_byte_array(@ba, expected_data.span(), 2, 0x2120);
}

#[test]
#[available_gas(1000000)]
fn test_append_word() {
    let mut ba = Default::default();

    ba.append_word(0x1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201, 30);
    let mut expected_data: Array<felt252> = Default::default();
    compare_byte_array(
        @ba,
        expected_data.span(),
        30,
        0x1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201
    );

    ba.append_word(0x21201f, 3);
    expected_data.append(0x1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201);
    compare_byte_array(@ba, expected_data.span(), 2, 0x2120);

    ba.append_word(0x2322, 2);
    compare_byte_array(@ba, expected_data.span(), 4, 0x23222120);

    // Length is 0, so nothing is actually appended.
    ba.append_word(0xffee, 0);
    compare_byte_array(@ba, expected_data.span(), 4, 0x23222120);

    ba.append_word(0x3e3d3c3b3a393837363534333231302f2e2d2c2b2a292827262524, 27);
    expected_data.append(0x3e3d3c3b3a393837363534333231302f2e2d2c2b2a29282726252423222120);
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

    let mut expected_data: Array<felt252> = Default::default();
    expected_data.append(0x1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201);
    expected_data.append(0x1e1d1c1b1a191817161514131211100f0e0d0c0b0a09080706050403020120);
    compare_byte_array(@ba1, expected_data.span(), 2, 0x201f);
}

#[test]
#[available_gas(1000000)]
fn test_concat() {
    let ba1 = test_byte_array_32();
    let ba2 = test_byte_array_32();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let mut expected_data: Array<felt252> = Default::default();
    expected_data.append(0x1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201);
    expected_data.append(0x1e1d1c1b1a191817161514131211100f0e0d0c0b0a09080706050403020120);
    compare_byte_array(@ba3, expected_data.span(), 2, 0x201f);
}

// Test concat/append, first byte array empty.
#[test]
#[available_gas(1000000)]
fn test_concat_first_empty() {
    let ba1 = Default::default();
    let ba2 = test_byte_array_32();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let mut expected_data: Array<felt252> = Default::default();
    expected_data.append(0x1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201);
    compare_byte_array(@ba3, expected_data.span(), 1, 0x20);
}

// Test concat/append, second byte array empty.
#[test]
#[available_gas(1000000)]
fn test_concat_second_empty() {
    let ba1 = test_byte_array_32();
    let ba2 = Default::default();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let mut expected_data: Array<felt252> = Default::default();
    expected_data.append(0x1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201);
    compare_byte_array(@ba3, expected_data.span(), 1, 0x20);
}

// Test concat/append, first byte array pending word is empty.
#[test]
#[available_gas(1000000)]
fn test_concat_first_pending_0() {
    let ba1 = test_byte_array_31();
    let ba2 = test_byte_array_32();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let mut expected_data: Array<felt252> = Default::default();
    expected_data.append(0x1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201);
    expected_data.append(0x1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201);
    compare_byte_array(@ba3, expected_data.span(), 1, 0x20);
}

// Test concat/append, second byte array pending word is empty.
#[test]
#[available_gas(1000000)]
fn test_concat_test_concat_second_pending_0() {
    let ba1 = test_byte_array_32();
    let ba2 = test_byte_array_31();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let mut expected_data: Array<felt252> = Default::default();
    expected_data.append(0x1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201);
    expected_data.append(0x1e1d1c1b1a191817161514131211100f0e0d0c0b0a09080706050403020120);
    compare_byte_array(@ba3, expected_data.span(), 1, 0x1f);
}

// Test concat/append, first byte array pending word is of length 15 (which means splitting words
// from second byte array at index 16, which fits u128).
#[test]
#[available_gas(1000000)]
fn test_concat_test_concat_first_pending_15() {
    let ba1 = test_byte_array_15();
    let ba2 = test_byte_array_32();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let mut expected_data: Array<felt252> = Default::default();
    expected_data.append(0x100f0e0d0c0b0a0908070605040302010f0e0d0c0b0a090807060504030201);
    compare_byte_array(@ba3, expected_data.span(), 16, 0x201f1e1d1c1b1a191817161514131211);
}

// Split index of the words of the second byte array is > 16, specifically 30.
#[test]
#[available_gas(1000000)]
fn test_concat_split_index_big() {
    let ba1 = test_byte_array_1();
    let ba2 = test_byte_array_32();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let mut expected_data: Array<felt252> = Default::default();
    expected_data.append(0x1e1d1c1b1a191817161514131211100f0e0d0c0b0a09080706050403020101);
    compare_byte_array(@ba3, expected_data.span(), 2, 0x201f);
}

// Split index of the words of the second byte array is < 16, specifically 1.
#[test]
#[available_gas(1000000)]
fn test_concat_split_index_small() {
    let ba1 = test_byte_array_30();
    let ba2 = test_byte_array_33();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let mut expected_data: Array<felt252> = Default::default();
    expected_data.append(0x011e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201);
    expected_data.append(0x201f1e1d1c1b1a191817161514131211100f0e0d0c0b0a0908070605040302);
    compare_byte_array(@ba3, expected_data.span(), 1, 0x21);
}

// Sum of the lengths of the pending words of both byte arrays is 31 (a full word).
#[test]
#[available_gas(1000000)]
fn test_concat_pending_sum_up_to_full() {
    let ba1 = test_byte_array_32();
    let ba2 = test_byte_array_30();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let mut expected_data: Array<felt252> = Default::default();
    expected_data.append(0x1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201);
    expected_data.append(0x1e1d1c1b1a191817161514131211100f0e0d0c0b0a09080706050403020120);
    compare_byte_array(@ba3, expected_data.span(), 0, 0);
}

// Sum of the lengths of the pending words of both byte arrays is >31 and split index is exactly 16.
#[test]
#[available_gas(1000000)]
fn test_concat_pending_sum_up_to_more_than_word_medium() {
    let ba1 = test_byte_array_15();
    let ba2 = test_byte_array_30();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let mut expected_data: Array<felt252> = Default::default();
    expected_data.append(0x100f0e0d0c0b0a0908070605040302010f0e0d0c0b0a090807060504030201);
    compare_byte_array(@ba3, expected_data.span(), 14, 0x1e1d1c1b1a191817161514131211);
}

// Sum of the lengths of the pending words of both byte arrays is >31 and split index is >16.
#[test]
#[available_gas(1000000)]
fn test_concat_pending_sum_up_to_more_than_word_long() {
    let ba1 = test_byte_array_2();
    let ba2 = test_byte_array_30();

    let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

    let mut expected_data: Array<felt252> = Default::default();
    expected_data.append(0x1d1c1b1a191817161514131211100f0e0d0c0b0a0908070605040302010201);
    compare_byte_array(@ba3, expected_data.span(), 1, 0x1e);
}

// ========= Test helper functions =========

use debug::PrintTrait;
fn compare_byte_array(
    mut ba: @ByteArray, mut data: Span<felt252>, pending_word_len: u8, pending_word: felt252
) {
    assert(ba.data.len() == data.len(), 'wrong data len');
    let mut ba_data = ba.data.span();

    let mut data_index = 0;
    loop {
        match ba_data.pop_front() {
            Option::Some(x) => {
                if (*x).into() != *data.pop_front().unwrap() {
                    data_index.print();
                    panic_with_felt252('wrong data');
                }
            },
            Option::None(_) => {
                break;
            }
        }
        data_index += 1;
    };

    if *ba.pending_word_len != pending_word_len {
        (*ba.pending_word_len).print();
        pending_word_len.print();
        panic_with_felt252('wrong pending_word_len');
    }
    let ba_pending_word_felt: felt252 = (*ba.pending_word).into();
    if ba_pending_word_felt != pending_word {
        ba_pending_word_felt.print();
        pending_word.print();
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
    ba1.append_word(0x0201, 2);
    ba1
}

fn test_byte_array_15() -> ByteArray {
    let mut ba1 = Default::default();
    ba1.append_word(0x0f0e0d0c0b0a090807060504030201, 15);
    ba1
}

fn test_byte_array_30() -> ByteArray {
    let mut ba1 = Default::default();
    ba1.append_word(0x1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201, 30);
    ba1
}

fn test_byte_array_31() -> ByteArray {
    let mut ba1 = Default::default();
    ba1.append_word(0x1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201, 31);
    ba1
}

fn test_byte_array_32() -> ByteArray {
    let mut ba1 = Default::default();
    ba1.append_word(0x1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201, 31);
    ba1.append_word(0x20, 1);
    ba1
}

fn test_byte_array_33() -> ByteArray {
    let mut ba2 = Default::default();
    ba2.append_word(0x1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201, 31);
    ba2.append_word(0x2120, 2);
    ba2
}
