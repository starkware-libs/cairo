use core::byte_array::BYTE_ARRAY_MAGIC;
use core::{panics, panic_with_felt252};

#[test]
#[should_panic(expected: 'short_string')]
fn test_panic_with_short_string() {
    panic_with_felt252('short_string');
}

#[test]
#[should_panic(expected: 1)]
fn test_panic_with_felt252() {
    panic_with_felt252(1);
}

#[test]
#[should_panic(expected: "")]
fn test_panic_with_byte_array_empty() {
    let ba: ByteArray = Default::default();
    panics::panic_with_byte_array(@ba);
}

#[test]
#[should_panic(expected: "error")]
fn test_panic_with_byte_array_short() {
    let ba: ByteArray = "error";
    panics::panic_with_byte_array(@ba);
}

#[test]
#[should_panic(expected: "long error with more than 31 characters")]
fn test_panic_with_byte_array_long() {
    let ba: ByteArray = "long error with more than 31 characters";
    panics::panic_with_byte_array(@ba);
}

#[test]
#[should_panic(expected: "long err\0r with more than 31 characters")]
fn test_panic_with_byte_array_null_in_full_word() {
    let mut ba: ByteArray = "long err";
    ba.append_byte(0);
    let ba2: ByteArray = "r with more than 31 characters";
    ba.append(@ba2);
    panics::panic_with_byte_array(@ba);
}

#[test]
#[should_panic(expected: "long error with more than 31 character\0s")]
fn test_panic_with_byte_array_null_in_pending() {
    let mut ba: ByteArray = "long error with more than 31 character";
    ba.append_byte(0);
    let ba2: ByteArray = "s";
    ba.append(@ba2);
    panics::panic_with_byte_array(@ba);
}

#[test]
#[should_panic(expected: "\0error")]
fn test_panic_with_byte_array_null_in_beginning() {
    let mut ba: ByteArray = "";
    ba.append_byte(0);
    let ba2: ByteArray = "error";
    ba.append(@ba2);
    panics::panic_with_byte_array(@ba);
}

#[test]
#[should_panic(expected: ("error", 11, "hello", 5, 'short_string'))]
fn test_panic_with_stacked_errors() {
    let mut error = array![];
    let ba: ByteArray = "error";
    error.append(BYTE_ARRAY_MAGIC);
    ba.serialize(ref error);

    error.append(11);

    let ba: ByteArray = "hello";
    error.append(BYTE_ARRAY_MAGIC);
    ba.serialize(ref error);

    error.append(5);
    error.append('short_string');

    panic(error);
}

#[test]
#[should_panic(
    expected: (
        0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3, // BYTE_ARRAY_MAGIC
        1,
        0x161616161616161616161616161616161616161616161616161616161616161,
        0,
        0
    )
)]
fn test_panic_with_byte_array_invalid_full_word() {
    // This is a serialized ByteArray, but the full word is an invalid short string (> 2^248).
    let mut error = array![
        BYTE_ARRAY_MAGIC,
        1, // A single full word.
        0x161616161616161616161616161616161616161616161616161616161616161, // The invalid full word.
        0,
        0 // pending byte is empty.
    ];
    panic(error);
}

#[test]
#[should_panic(
    expected: (
        0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3, // BYTE_ARRAY_MAGIC
        0,
        'aa',
        1
    )
)]
fn test_panic_with_byte_array_invalid_pending_word() {
    // This is a serialized ByteArray, but the pending word length < the actual data in the pending
    // word.
    let mut error = array![
        BYTE_ARRAY_MAGIC,
        0, // No full words.
        'aa',
        1 // pending word length. Smaller than the actual data in the pending word.
    ];
    panic(error);
}

#[test]
#[should_panic(expected: "")]
fn test_panic_macro_empty() {
    panic!()
}

#[test]
#[should_panic(expected: "basic")]
fn test_panic_macro_basic_string() {
    panic!("basic")
}

#[test]
#[should_panic(expected: "some_format(1)")]
fn test_panic_macro_with_input() {
    panic!("some_format({})", 1)
}
