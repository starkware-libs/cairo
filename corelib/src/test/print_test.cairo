use core::byte_array::BYTE_ARRAY_MAGIC;
use core::debug::{PrintTrait, print_byte_array_as_string};

#[ignore]
#[test]
fn test_prints() {
    // Valid short string.
    'hello'.print();

    // felt252
    1.print();

    // Valid string with < 31 characters (no full words).
    let x: ByteArray = "short, but string";
    print_byte_array_as_string(@x);

    // Valid string with > 31 characters (with a full word).
    let x: ByteArray = "This is a long string with more than 31 characters.";
    print_byte_array_as_string(@x);

    // Valid string as an array.
    let x = array![
        BYTE_ARRAY_MAGIC,
        1, // A single full word.
        'This is a long string with more',
        ' than 31 characters.',
        20 // pending word length. Bigger than the actual data in the pending word.
    ];
    x.print();

    // Only magic.
    let x = array![BYTE_ARRAY_MAGIC];
    x.print();

    // num_full_words > usize.
    let x = array![BYTE_ARRAY_MAGIC, 0x100000000];
    x.print();

    // Not enough data after num_full_words.
    let x = array![BYTE_ARRAY_MAGIC, 0];
    x.print();

    // Not enough full words.
    let x = array![BYTE_ARRAY_MAGIC, 1, 0, 0];
    x.print();

    // Too much data in full word.
    let x = array![
        BYTE_ARRAY_MAGIC,
        1, // A single full word.
        0x161616161616161616161616161616161616161616161616161616161616161, // The invalid full word.
        0,
        0 // pending byte is empty.
    ];
    x.print();

    // num_pending_bytes > usize.
    let x = array![BYTE_ARRAY_MAGIC, 0, 0, 0x100000000];
    x.print();

    // "Not enough" data in pending_word (nulls in the beginning).
    let x = array![
        BYTE_ARRAY_MAGIC,
        0, // No full words.
        'a',
        2 // pending word length. Bigger than the actual data in the pending word.
    ];
    x.print();

    // Too much data in pending_word.
    let x = array![
        BYTE_ARRAY_MAGIC,
        0, // No full words.
        'aa',
        1 // pending word length. Smaller than the actual data in the pending word.
    ];
    x.print();

    // Valid string with Null.
    let mut x: ByteArray = "Hello";
    x.append_byte(0); // '\0'
    let suffix: ByteArray = "world";
    x.append(@suffix);
    print_byte_array_as_string(@x);

    // Valid string with a non printable character.
    let mut x: ByteArray = "Hello";
    x.append_byte(0x11); // Non printable character.
    let suffix: ByteArray = "world";
    x.append(@suffix);
    print_byte_array_as_string(@x);

    // Valid string with a newline.
    let mut x: ByteArray = "Hello";
    x.append_byte(0xA); // '\n'
    let suffix: ByteArray = "world";
    x.append(@suffix);
    print_byte_array_as_string(@x);

    // Multiple values: (felt, string, short_string, felt)
    let x = array![0x9999, BYTE_ARRAY_MAGIC, 0, 'hello', 5, 'world', 0x8888];
    x.print();
}

#[ignore]
#[test]
fn test_print_macro() {
    // With a ByteArray.
    let ba: ByteArray = "hello";
    print!("{}", ba);

    // With a felt252.
    print!("{}", 97_felt252);

    // With an integer.
    print!("{}", 97_usize);
}

#[ignore]
#[test]
fn test_println_macro() {
    // With a ByteArray.
    let ba: ByteArray = "hello";
    println!("{}", ba);

    // With a felt252.
    println!("{}", 97_felt252);

    // With an integer.
    println!("{}", 97_usize);
}
