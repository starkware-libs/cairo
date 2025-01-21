use cairo_lang_casm::assembler::AssembledCairoProgram;
use cairo_lang_casm::inline::CasmContext;
use cairo_lang_casm::{casm, deref};
use cairo_lang_sierra_to_casm::compiler::{CairoProgram, CairoProgramDebugInfo};
use cairo_lang_utils::byte_array::BYTE_ARRAY_MAGIC;
use cairo_vm::vm::runners::cairo_runner::RunResources;
use indoc::indoc;
use itertools::Itertools;
use num_traits::ToPrimitive;
use starknet_types_core::felt::Felt as Felt252;
use test_case::test_case;

use super::format_for_debug;
use crate::casm_run::contract_address::calculate_contract_address;
use crate::casm_run::{RunFunctionResult, run_function};
use crate::short_string::{as_cairo_short_string, as_cairo_short_string_ex};
use crate::{CairoHintProcessor, StarknetState, build_hints_dict};

/// Creates a new `AssembledCairoProgram` from the given `CasmContext`.
fn assembled(casm: CasmContext) -> AssembledCairoProgram {
    CairoProgram {
        instructions: casm.instructions,
        consts_info: Default::default(),
        debug_info: CairoProgramDebugInfo { sierra_statement_info: vec![] },
    }
    .assemble()
}

#[test_case(
    casm! {
        [ap] = (-5), ap++;
        [ap] = 7, ap++;
        ret;
    },
    2,
    &[-5, 7];
    "simple ap sets"
)]
#[test_case(
    casm! {
        ap += 1;
        [ap] = 123, ap++;
        [ap] = 456, ap++;
        [fp] = [ap - 2] + [ap - 1];
        ret;
    },
    3,
    &[579, 123, 456];
    "sum ap into fp"
)]
#[test_case(
    casm! {
        [ap] = 1, ap++;
        jmp rel 2 if [ap - 1] != 0;
        [ap] = 5, ap++;
        [ap] = 0, ap++;
        jmp rel 2 if [ap - 1] != 0;
        [ap] = 3, ap++;
        [ap] = 4, ap++;
        ret;
    },
    5,
    &[1, 5, 0, 3, 4];
    "jumps"
)]
#[test_case(
    casm! {
        [ap] = 39, ap++;
        %{ memory[ap] = 13 < memory[ap - 1] %}
        ap += 1;
        [ap] = [ap - 1] + 83, ap++;
        ret;
    },
    3,
    &[39, 1, 84];
    "less than hint"
)]
#[test_case(
    casm! {
        [ap] = 5, ap++;
        [ap] = 39, ap++;
        %{ (memory[ap], memory[ap + 1]) = divmod(memory[ap - 1], memory[ap - 2]) %}
        ap += 2;
        ret;
    },
    4,
    &[5, 39, 7, 4];
    "divmod hint"
)]
#[test_case(
    casm! {
        [ap + 0] = 1, ap++;
        [ap + 0] = 1, ap++;
        [ap + 0] = 13, ap++;
        call rel 3;
        ret;
        jmp rel 5 if [fp + -3] != 0;
        [ap + 0] = [fp + -5], ap++;
        jmp rel 8;
        [ap + 0] = [fp + -4], ap++;
        [ap + 0] = [fp + -5] + [fp + -4], ap++;
        [fp + -3] = [ap + 0] + 1, ap++;
        call rel (-9);
        ret;
    },
    1,
    &[377];
    "fib(1, 1, 13)"
)]
#[test_case(
    casm! {
        [ap + 0] = 2, ap++;
        [ap + 0] = 1, ap++;
        [ap - 1] = [ap + 0] * [ap - 2], ap++; // Calculates.
        [ap - 2] = [ap - 1] * [ap - 3]; // Validates the calculation.
        ret;
    },
    0,
    &[];
    "simple_division"
)]
fn test_runner(function: CasmContext, n_returns: usize, expected: &[i128]) {
    let program = assembled(function);
    let (hints_dict, string_to_hint) = build_hints_dict(&program.hints);
    let mut hint_processor = CairoHintProcessor {
        runner: None,
        user_args: vec![],
        string_to_hint,
        starknet_state: StarknetState::default(),
        run_resources: RunResources::default(),
        syscalls_used_resources: Default::default(),
        no_temporary_segments: true,
        markers: Default::default(),
    };

    let RunFunctionResult { ap, memory, .. } =
        run_function(program.bytecode.iter(), vec![], |_| Ok(()), &mut hint_processor, hints_dict)
            .expect("Running code failed.");
    let ret_memory = memory.into_iter().skip(ap - n_returns);
    assert_eq!(
        ret_memory.take(n_returns).map(|cell| cell.unwrap()).collect_vec(),
        expected.iter().copied().map(Felt252::from).collect_vec()
    );
}

#[test]
fn test_allocate_segment() {
    let program = assembled(casm! {
        [ap] = 1337, ap++;
        %{ memory[ap] = segments.add() %}
        [ap - 1] = [[&deref!([ap])]];
        ret;
    });
    let (hints_dict, string_to_hint) = build_hints_dict(&program.hints);
    let mut hint_processor = CairoHintProcessor {
        runner: None,
        user_args: vec![],
        string_to_hint,
        starknet_state: StarknetState::default(),
        run_resources: RunResources::default(),
        syscalls_used_resources: Default::default(),
        no_temporary_segments: true,
        markers: Default::default(),
    };

    let RunFunctionResult { ap, memory, .. } =
        run_function(program.bytecode.iter(), vec![], |_| Ok(()), &mut hint_processor, hints_dict)
            .expect("Running code failed.");
    let ptr = memory[ap]
        .as_ref()
        .expect("Uninitialized value.")
        .to_usize()
        .expect("Number not in index range.");
    assert_eq!(memory[ptr], Some(Felt252::from(1337)));
}

#[test]
fn test_as_cairo_short_string() {
    // Simple short strings.
    assert_eq!(as_cairo_short_string(&Felt252::from(0)), Some("".to_string()));
    assert_eq!(as_cairo_short_string(&Felt252::from(0x61)), Some("a".to_string()));
    assert_eq!(
        as_cairo_short_string(&Felt252::from_hex_unchecked(
            "30313233343536373839303132333435363738393031323334353637383930"
        )),
        Some("0123456789012345678901234567890".to_string())
    );

    // With whitespace.
    assert_eq!(as_cairo_short_string(&Felt252::from(0x612062)), Some("a b".to_string()));
    assert_eq!(as_cairo_short_string(&Felt252::from(0x610962)), Some("a\tb".to_string()));

    // With null in the middle.
    assert_eq!(as_cairo_short_string(&Felt252::from(0x610062)), None);

    // Nulls in the end are OK.
    assert_eq!(as_cairo_short_string(&Felt252::from(0x6100)), Some("a".to_string()));
    assert_eq!(as_cairo_short_string(&Felt252::from(0x61000000)), Some("a".to_string()));

    // With a non printable character.
    assert_eq!(as_cairo_short_string(&Felt252::from(0x610162)), None);
}

#[test]
fn test_as_cairo_short_string_ex() {
    // Simple short strings.
    assert_eq!(as_cairo_short_string_ex(&Felt252::from(0), 0), Some("".to_string()));
    assert_eq!(as_cairo_short_string_ex(&Felt252::from(0x61), 1), Some("a".to_string()));
    assert_eq!(
        as_cairo_short_string_ex(
            &Felt252::from_hex_unchecked(
                "30313233343536373839303132333435363738393031323334353637383930",
            ),
            31
        ),
        Some("0123456789012345678901234567890".to_string())
    );

    // With whitespace.
    assert_eq!(as_cairo_short_string_ex(&Felt252::from(0x612062), 3), Some("a b".to_string()));
    assert_eq!(as_cairo_short_string_ex(&Felt252::from(0x610962), 3), Some("a\tb".to_string()));

    // Nulls are OK.
    assert_eq!(as_cairo_short_string_ex(&Felt252::from(0x6100), 2), Some(r"a\0".to_string()));
    assert_eq!(as_cairo_short_string_ex(&Felt252::from(0x610062), 3), Some(r"a\0b".to_string()));

    // With a non printable character.
    assert_eq!(as_cairo_short_string_ex(&Felt252::from(0x610162), 3), Some(r"a\x01b".to_string()));

    // More data than expected.
    assert_eq!(as_cairo_short_string_ex(&Felt252::from(0x61), 0), None);
    assert_eq!(as_cairo_short_string_ex(&Felt252::from(0x6161), 1), None);

    // length > 31.
    assert_eq!(
        as_cairo_short_string_ex(
            &Felt252::from_hex_unchecked(
                "100000000000000000000000000000000000000000000000000000000000000",
            ),
            32
        ),
        None
    );
}

#[test]
fn test_format_for_debug() {
    // Valid short string.
    let felts = vec![Felt252::from_hex_unchecked("68656c6c6f")];
    assert_eq!(format_for_debug(felts.into_iter()), "[DEBUG]\t0x68656c6c6f ('hello')\n");

    // felt252
    let felts = vec![Felt252::from(1)];
    assert_eq!(format_for_debug(felts.into_iter()), "[DEBUG]\t0x1\n");

    // Valid string with < 31 characters (no full words).
    let felts = vec![
        Felt252::from_hex_unchecked(BYTE_ARRAY_MAGIC),
        Felt252::from(0), // No full words.
        Felt252::from_hex_unchecked("73686f72742c2062757420737472696e67"), /* 'short, but
                           * string' */
        Felt252::from(17), // pending word length
    ];
    assert_eq!(format_for_debug(felts.into_iter()), "short, but string");

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
        format_for_debug(felts.into_iter()),
        "This is a long string with more than 31 characters."
    );

    // Only magic.
    let felts = vec![Felt252::from_hex_unchecked(BYTE_ARRAY_MAGIC)];
    assert_eq!(
        format_for_debug(felts.into_iter()),
        "[DEBUG]\t0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3\n"
    );

    // num_full_words > usize.
    let felts = vec![
        Felt252::from_hex_unchecked(BYTE_ARRAY_MAGIC),
        Felt252::from_hex_unchecked("100000000"),
    ];
    assert_eq!(
        format_for_debug(felts.into_iter()),
        indoc!(
            "[DEBUG]\t0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3
                [DEBUG]\t0x100000000
                "
        )
    );

    // Not enough data after num_full_words.
    let felts = vec![Felt252::from_hex_unchecked(BYTE_ARRAY_MAGIC), Felt252::from(0)];
    assert_eq!(
        format_for_debug(felts.into_iter()),
        "[DEBUG]\t0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3\n[DEBUG]\t0x0 \
         ('')\n"
    );

    // Not enough full words.
    let felts = vec![
        Felt252::from_hex_unchecked(BYTE_ARRAY_MAGIC),
        Felt252::from(1),
        Felt252::from(0),
        Felt252::from(0),
    ];
    assert_eq!(
        format_for_debug(felts.into_iter()),
        indoc!(
            "[DEBUG]\t0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3
            [DEBUG]\t0x1
            [DEBUG]\t0x0 ('')\n[DEBUG]\t0x0 ('')
            "
        )
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
        format_for_debug(felts.into_iter()),
        indoc!(
            "[DEBUG]\t0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3
            [DEBUG]\t0x1
            [DEBUG]\t0x161616161616161616161616161616161616161616161616161616161616161
            [DEBUG]\t0x0 ('')
            [DEBUG]\t0x0 ('')
            "
        )
    );

    // num_pending_bytes > usize.
    let felts = vec![
        Felt252::from_hex_unchecked(BYTE_ARRAY_MAGIC),
        Felt252::from(0),
        Felt252::from(0),
        Felt252::from_hex_unchecked("100000000"),
    ];
    assert_eq!(
        format_for_debug(felts.into_iter()),
        indoc!(
            "[DEBUG]\t0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3
            [DEBUG]\t0x0 ('')
            [DEBUG]\t0x0 ('')
            [DEBUG]\t0x100000000
            "
        )
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
    assert_eq!(format_for_debug(felts.into_iter()), "\\0a");

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
        format_for_debug(felts.into_iter()),
        indoc!(
            "[DEBUG]\t0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3
            [DEBUG]\t0x0 ('')
            [DEBUG]\t0x6161 ('aa')
            [DEBUG]\t0x1
            "
        )
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
    assert_eq!(format_for_debug(felts.into_iter()), "Hello\\0world");

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
    assert_eq!(format_for_debug(felts.into_iter()), "Hello\\x11world");

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
    assert_eq!(format_for_debug(felts.into_iter()), "Hello\nworld");

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
        format_for_debug(felts.into_iter()),
        indoc!(
            "[DEBUG]\t0x9999
            hello
            [DEBUG]\t0x776f726c64 ('world')
            [DEBUG]\t0x8888
            "
        )
    );
}

#[test]
fn test_calculate_contract_address() {
    let salt = Felt252::from_dec_str("122660764594045088044512115").unwrap();
    let deployer_address = Felt252::from(0x01);
    let class_hash = Felt252::from_dec_str(
        "1779576919126046589190499439779938629977579841313883525093195577363779864274",
    )
    .unwrap();
    let calldata = vec![deployer_address, salt];
    let deployed_contract_address =
        calculate_contract_address(&salt, &class_hash, &calldata, &deployer_address);

    assert_eq!(
        Felt252::from_dec_str(
            "2288343933438457476985546536845198482236255384896285993520343604559094835567"
        )
        .unwrap(),
        deployed_contract_address
    );
}
