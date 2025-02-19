use cairo_lang_runner::casm_run::format_for_panic;
use cairo_lang_sierra::program::{Program, ProgramArtifact};
use cairo_lang_test_plugin::test_config::TestExpectation;
use cairo_lang_test_plugin::{TestCompilationMetadata, TestConfig, TestsCompilationConfig};
use cairo_lang_utils::byte_array::BYTE_ARRAY_MAGIC;
use itertools::Itertools;
use starknet_types_core::felt::Felt as Felt252;

use crate::{TestCompilation, TestCompiler, filter_test_cases};

#[test]
fn test_compiled_serialization() {
    use std::path::PathBuf;
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data");

    let compiler = TestCompiler::try_new(
        &path,
        true,
        false,
        TestsCompilationConfig {
            starknet: true,
            add_statements_functions: false,
            add_statements_code_locations: false,
            contract_declarations: None,
            contract_crate_ids: None,
            executable_crate_ids: None,
        },
    )
    .unwrap();
    let compiled = compiler.build().unwrap();
    let serialized = serde_json::to_string_pretty(&compiled).unwrap();
    let deserialized: TestCompilation = serde_json::from_str(&serialized).unwrap();

    assert_eq!(compiled.sierra_program, deserialized.sierra_program);
    assert_eq!(compiled.metadata.function_set_costs, deserialized.metadata.function_set_costs);
    assert_eq!(compiled.metadata.named_tests, deserialized.metadata.named_tests);
    assert_eq!(
        compiled.metadata.contracts_info.values().collect_vec(),
        deserialized.metadata.contracts_info.values().collect_vec()
    );
}

#[test]
fn test_format_for_panic() {
    // Valid short string.
    let felts = vec![Felt252::from_hex_unchecked("68656c6c6f")];
    assert_eq!(format_for_panic(felts.into_iter()), "Panicked with 0x68656c6c6f ('hello').");

    // felt252
    let felts = vec![Felt252::from(1)];
    assert_eq!(format_for_panic(felts.into_iter()), "Panicked with 0x1.");

    // Valid string with < 31 characters (no full words).
    let felts = vec![
        Felt252::from_hex_unchecked(BYTE_ARRAY_MAGIC),
        Felt252::from(0), // No full words.
        Felt252::from_hex_unchecked("73686f72742c2062757420737472696e67"), // 'short, but string'
        Felt252::from(17), // pending word length
    ];
    assert_eq!(format_for_panic(felts.into_iter()), "Panicked with \"short, but string\".");

    // Valid string with > 31 characters (with a full word).
    let felts = vec![
        Felt252::from_hex_unchecked(BYTE_ARRAY_MAGIC),
        // A single full word.
        Felt252::from(1),
        // full word: 'This is a long string with more'
        Felt252::from_hex_unchecked(
            "546869732069732061206c6f6e6720737472696e672077697468206d6f7265",
        ),
        // pending word: ' than 31 characters.'
        Felt252::from_hex_unchecked("207468616e20333120636861726163746572732e"),
        // pending word length
        Felt252::from(20),
    ];
    assert_eq!(
        format_for_panic(felts.into_iter()),
        "Panicked with \"This is a long string with more than 31 characters.\"."
    );

    // Only magic.
    let felts = vec![Felt252::from_hex_unchecked(BYTE_ARRAY_MAGIC)];
    assert_eq!(
        format_for_panic(felts.into_iter()),
        "Panicked with 0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3."
    );

    // num_full_words > usize.
    let felts = vec![
        Felt252::from_hex_unchecked(BYTE_ARRAY_MAGIC),
        Felt252::from_hex_unchecked("100000000"),
    ];
    assert_eq!(
        format_for_panic(felts.into_iter()),
        "Panicked with (0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3, \
         0x100000000)."
    );

    // Not enough data after num_full_words.
    let felts = vec![Felt252::from_hex_unchecked(BYTE_ARRAY_MAGIC), Felt252::from(0)];
    assert_eq!(
        format_for_panic(felts.into_iter()),
        "Panicked with (0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3, 0x0 \
         (''))."
    );

    // Not enough full words.
    let felts = vec![
        Felt252::from_hex_unchecked(BYTE_ARRAY_MAGIC),
        Felt252::from(1),
        Felt252::from(0),
        Felt252::from(0),
    ];
    assert_eq!(
        format_for_panic(felts.into_iter()),
        "Panicked with (0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3, 0x1, \
         0x0 (''), 0x0 (''))."
    );

    // Too much data in full word.
    let felts = vec![
        Felt252::from_hex_unchecked(BYTE_ARRAY_MAGIC),
        Felt252::from(1),
        Felt252::from_hex_unchecked(
            "161616161616161616161616161616161616161616161616161616161616161",
        ),
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
        Felt252::from_hex_unchecked(BYTE_ARRAY_MAGIC),
        Felt252::from(0),
        Felt252::from(0),
        Felt252::from_hex_unchecked("100000000"),
    ];
    assert_eq!(
        format_for_panic(felts.into_iter()),
        "Panicked with (0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3, 0x0 \
         (''), 0x0 (''), 0x100000000)."
    );

    // "Not enough" data in pending_word (nulls in the beginning).
    let felts = vec![
        Felt252::from_hex_unchecked(BYTE_ARRAY_MAGIC),
        // No full words.
        Felt252::from(0),
        // 'a'
        Felt252::from_hex_unchecked("61"),
        // pending word length. Bigger than the actual data in the pending word.
        Felt252::from(2),
    ];
    assert_eq!(format_for_panic(felts.into_iter()), "Panicked with \"\\0a\".");

    // Too much data in pending_word.
    let felts = vec![
        Felt252::from_hex_unchecked(BYTE_ARRAY_MAGIC),
        // No full words.
        Felt252::from(0),
        // 'aa'
        Felt252::from_hex_unchecked("6161"),
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
        Felt252::from_hex_unchecked(BYTE_ARRAY_MAGIC),
        // No full word.
        Felt252::from(0),
        // pending word: 'Hello\0world'
        Felt252::from_hex_unchecked("48656c6c6f00776f726c64"),
        // pending word length
        Felt252::from(11),
    ];
    assert_eq!(format_for_panic(felts.into_iter()), "Panicked with \"Hello\\0world\".");

    // Valid string with a non printable character.
    let felts = vec![
        Felt252::from_hex_unchecked(BYTE_ARRAY_MAGIC),
        // No full word.
        Felt252::from(0),
        // pending word: 'Hello\x11world'
        Felt252::from_hex_unchecked("48656c6c6f11776f726c64"),
        // pending word length
        Felt252::from(11),
    ];
    assert_eq!(format_for_panic(felts.into_iter()), "Panicked with \"Hello\\x11world\".");

    // Valid string with a newline.
    let felts = vec![
        Felt252::from_hex_unchecked(BYTE_ARRAY_MAGIC),
        // No full word.
        Felt252::from(0),
        // pending word: 'Hello\nworld'
        Felt252::from_hex_unchecked("48656c6c6f0a776f726c64"),
        // pending word length
        Felt252::from(11),
    ];
    assert_eq!(format_for_panic(felts.into_iter()), "Panicked with \"Hello\nworld\".");

    // Multiple values: (felt, string, short_string, felt)
    let felts = vec![
        // felt: 0x9999
        Felt252::from(0x9999),
        // String: "hello"
        Felt252::from_hex_unchecked(BYTE_ARRAY_MAGIC),
        Felt252::from(0),
        Felt252::from_hex_unchecked("68656c6c6f"),
        Felt252::from(5),
        // Short string: 'world'
        Felt252::from_hex_unchecked("776f726c64"),
        // felt: 0x8888
        Felt252::from(0x8888),
    ];
    assert_eq!(
        format_for_panic(felts.into_iter()),
        "Panicked with (0x9999, \"hello\", 0x776f726c64 ('world'), 0x8888)."
    );
}

/// Return a named test ([String], [TestConfig]) from a test name and its ignored status.
fn to_named_test(test: &(&str, bool)) -> (String, TestConfig) {
    (
        String::from(test.0),
        TestConfig { available_gas: None, expectation: TestExpectation::Success, ignored: test.1 },
    )
}

/// Return a [TestCompilation] from a list of test names and their ignored status.
fn to_test_compilation(tests: &[(&str, bool)]) -> TestCompilation {
    TestCompilation {
        sierra_program: ProgramArtifact::stripped(Program {
            type_declarations: vec![],
            libfunc_declarations: vec![],
            statements: vec![],
            funcs: vec![],
        }),
        metadata: TestCompilationMetadata {
            named_tests: tests.iter().map(to_named_test).collect(),
            contracts_info: Default::default(),
            function_set_costs: Default::default(),
            statements_locations: None,
        },
    }
}

#[test]
fn test_filter_test_cases() {
    assert_eq!(
        filter_test_cases(
            to_test_compilation(&[("test1", false), ("test2", true), ("test3", false)]),
            false,
            false,
            "test"
        ),
        (to_test_compilation(&[("test1", false), ("test2", true), ("test3", false)]), 0)
    );
}

#[test]
fn test_filter_test_cases_include_ignored() {
    assert_eq!(
        filter_test_cases(
            to_test_compilation(&[("test1", false), ("test2", true), ("test3", false)]),
            true,
            false,
            "test"
        ),
        (to_test_compilation(&[("test1", false), ("test2", false), ("test3", false)]), 0)
    );
}

#[test]
fn test_filter_test_cases_ignored() {
    assert_eq!(
        filter_test_cases(
            to_test_compilation(&[("test1", false), ("test2", true), ("test3", false)]),
            false,
            true,
            "test"
        ),
        (to_test_compilation(&[("test2", false)]), 2)
    );
}

#[test]
fn test_filter_test_cases_include_ignored_and_ignored() {
    assert_eq!(
        filter_test_cases(
            to_test_compilation(&[("test1", false), ("test2", true), ("test3", false)]),
            true,
            true,
            "test"
        ),
        (to_test_compilation(&[("test1", false), ("test2", false), ("test3", false)]), 0)
    );
}
