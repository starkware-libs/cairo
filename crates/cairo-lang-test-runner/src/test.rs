use cairo_felt::{felt_str, Felt252};
use cairo_lang_utils::byte_array::BYTE_ARRAY_MAGIC;
use itertools::Itertools;

use crate::{format_for_panic, TestCompilation, TestCompiler};

#[test]
fn test_compiled_serialization() {
    use std::path::PathBuf;
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data");

    let compiler = TestCompiler::try_new(&path, true, false, true).unwrap();
    let compiled = compiler.build().unwrap();
    let serialized = serde_json::to_string_pretty(&compiled).unwrap();
    let deserialized: TestCompilation = serde_json::from_str(&serialized).unwrap();

    assert_eq!(compiled.sierra_program, deserialized.sierra_program);
    assert_eq!(compiled.function_set_costs, deserialized.function_set_costs);
    assert_eq!(compiled.named_tests, deserialized.named_tests);
    assert_eq!(
        compiled.contracts_info.values().collect_vec(),
        deserialized.contracts_info.values().collect_vec()
    );
}

#[test]
fn test_format_for_panic() {
    // Valid short string.
    let felts = vec![felt_str!("68656c6c6f", 16)];
    assert_eq!(format_for_panic(felts.into_iter()), "Panicked with 0x68656c6c6f ('hello').");

    // felt252
    let felts = vec![Felt252::from(1)];
    assert_eq!(format_for_panic(felts.into_iter()), "Panicked with 0x1.");

    // Valid string with < 31 characters (no full words).
    let felts = vec![
        felt_str!(BYTE_ARRAY_MAGIC, 16),
        Felt252::from(0),                                    // No full words.
        felt_str!("73686f72742c2062757420737472696e67", 16), // 'short, but string'
        Felt252::from(17),                                   // pending word length
    ];
    assert_eq!(format_for_panic(felts.into_iter()), "Panicked with \"short, but string\".");

    // Valid string with > 31 characters (with a full word).
    let felts = vec![
        felt_str!(BYTE_ARRAY_MAGIC, 16),
        // A single full word.
        Felt252::from(1),
        // full word: 'This is a long string with more'
        felt_str!("546869732069732061206c6f6e6720737472696e672077697468206d6f7265", 16),
        // pending word: ' than 31 characters.'
        felt_str!("207468616e20333120636861726163746572732e", 16),
        // pending word length
        Felt252::from(20),
    ];
    assert_eq!(
        format_for_panic(felts.into_iter()),
        "Panicked with \"This is a long string with more than 31 characters.\"."
    );

    // Only magic.
    let felts = vec![felt_str!(BYTE_ARRAY_MAGIC, 16)];
    assert_eq!(
        format_for_panic(felts.into_iter()),
        "Panicked with 0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3."
    );

    // num_full_words > usize.
    let felts = vec![felt_str!(BYTE_ARRAY_MAGIC, 16), felt_str!("100000000", 16)];
    assert_eq!(
        format_for_panic(felts.into_iter()),
        "Panicked with (0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3, \
         0x100000000)."
    );

    // Not enough data after num_full_words.
    let felts = vec![felt_str!(BYTE_ARRAY_MAGIC, 16), Felt252::from(0)];
    assert_eq!(
        format_for_panic(felts.into_iter()),
        "Panicked with (0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3, 0x0 \
         (''))."
    );

    // Not enough full words.
    let felts =
        vec![felt_str!(BYTE_ARRAY_MAGIC, 16), Felt252::from(1), Felt252::from(0), Felt252::from(0)];
    assert_eq!(
        format_for_panic(felts.into_iter()),
        "Panicked with (0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3, 0x1, \
         0x0 (''), 0x0 (''))."
    );

    // Too much data in full word.
    let felts = vec![
        felt_str!(BYTE_ARRAY_MAGIC, 16),
        Felt252::from(1),
        felt_str!("161616161616161616161616161616161616161616161616161616161616161", 16),
        Felt252::from(0),
        Felt252::from(0),
    ];
    assert_eq!(
        format_for_panic(felts.into_iter()),
        "Panicked with (0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3, 0x1, \
         0x161616161616161616161616161616161616161616161616161616161616161, 0x0 (''), 0x0 (''))."
    );

    // num_pending_bytes > usize.
    let felts = vec![
        felt_str!(BYTE_ARRAY_MAGIC, 16),
        Felt252::from(0),
        Felt252::from(0),
        felt_str!("100000000", 16),
    ];
    assert_eq!(
        format_for_panic(felts.into_iter()),
        "Panicked with (0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3, 0x0 \
         (''), 0x0 (''), 0x100000000)."
    );

    // "Not enough" data in pending_word (nulls in the beginning).
    let felts = vec![
        felt_str!(BYTE_ARRAY_MAGIC, 16),
        // No full words.
        Felt252::from(0),
        // 'a'
        felt_str!("61", 16),
        // pending word length. Bigger than the actual data in the pending word.
        Felt252::from(2),
    ];
    assert_eq!(format_for_panic(felts.into_iter()), "Panicked with \"\\0a\".");

    // Too much data in pending_word.
    let felts = vec![
        felt_str!(BYTE_ARRAY_MAGIC, 16),
        // No full words.
        Felt252::from(0),
        // 'aa'
        felt_str!("6161", 16),
        // pending word length. Smaller than the actual data in the pending word.
        Felt252::from(1),
    ];
    assert_eq!(
        format_for_panic(felts.into_iter()),
        "Panicked with (0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3, 0x0 \
         (''), 0x6161 ('aa'), 0x1)."
    );

    // Valid string with Null.
    let felts = vec![
        felt_str!(BYTE_ARRAY_MAGIC, 16),
        // No full word.
        Felt252::from(0),
        // pending word: 'Hello\0world'
        felt_str!("48656c6c6f00776f726c64", 16),
        // pending word length
        Felt252::from(11),
    ];
    assert_eq!(format_for_panic(felts.into_iter()), "Panicked with \"Hello\\0world\".");

    // Valid string with a non printable character.
    let felts = vec![
        felt_str!(BYTE_ARRAY_MAGIC, 16),
        // No full word.
        Felt252::from(0),
        // pending word: 'Hello\x11world'
        felt_str!("48656c6c6f11776f726c64", 16),
        // pending word length
        Felt252::from(11),
    ];
    assert_eq!(format_for_panic(felts.into_iter()), "Panicked with \"Hello\\x11world\".");

    // Valid string with a newline.
    let felts = vec![
        felt_str!(BYTE_ARRAY_MAGIC, 16),
        // No full word.
        Felt252::from(0),
        // pending word: 'Hello\nworld'
        felt_str!("48656c6c6f0a776f726c64", 16),
        // pending word length
        Felt252::from(11),
    ];
    assert_eq!(format_for_panic(felts.into_iter()), "Panicked with \"Hello\nworld\".");

    // Multiple values: (felt, string, short_string, felt)
    let felts = vec![
        // felt: 0x9999
        Felt252::from(0x9999),
        // String: "hello"
        felt_str!(BYTE_ARRAY_MAGIC, 16),
        Felt252::from(0),
        felt_str!("68656c6c6f", 16),
        Felt252::from(5),
        // Short string: 'world'
        felt_str!("776f726c64", 16),
        // felt: 0x8888
        Felt252::from(0x8888),
    ];
    assert_eq!(
        format_for_panic(felts.into_iter()),
        "Panicked with (0x9999, \"hello\", 0x776f726c64 ('world'), 0x8888)."
    );
}
