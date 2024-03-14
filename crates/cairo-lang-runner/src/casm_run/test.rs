use cairo_felt::{felt_str, Felt252};
use cairo_lang_casm::inline::CasmContext;
use cairo_lang_casm::{casm, deref};
use cairo_lang_utils::byte_array::BYTE_ARRAY_MAGIC;
use cairo_vm::vm::runners::cairo_runner::RunResources;
use cairo_vm::vm::vm_core::VirtualMachine;
use indoc::indoc;
use itertools::Itertools;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use test_case::test_case;

use super::format_for_debug;
use crate::casm_run::contract_address::calculate_contract_address;
use crate::casm_run::{run_function, RunFunctionResult};
use crate::short_string::{as_cairo_short_string, as_cairo_short_string_ex};
use crate::{build_hints_dict, CairoHintProcessor, StarknetState};

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
    let (hints_dict, string_to_hint) = build_hints_dict(function.instructions.iter());
    let mut hint_processor = CairoHintProcessor {
        runner: None,
        string_to_hint,
        starknet_state: StarknetState::default(),
        run_resources: RunResources::default(),
        syscalls_used_resources: Default::default(),
    };
    let bytecode: Vec<BigInt> = function
        .instructions
        .iter()
        .flat_map(|instruction| instruction.assemble().encode())
        .collect();

    let RunFunctionResult { memory, ap, .. } = run_function(
        &mut VirtualMachine::new(true),
        bytecode.iter(),
        vec![],
        |_| Ok(()),
        &mut hint_processor,
        hints_dict,
    )
    .expect("Running code failed.");
    let ret_memory = memory.into_iter().skip(ap - n_returns);
    assert_eq!(
        ret_memory.take(n_returns).map(|cell| cell.unwrap()).collect_vec(),
        expected.iter().copied().map(Felt252::from).collect_vec()
    );
}

#[test]
fn test_allocate_segment() {
    let casm = casm! {
        [ap] = 1337, ap++;
        %{ memory[ap] = segments.add() %}
        [ap - 1] = [[&deref!([ap])]];
        ret;
    };

    let (hints_dict, string_to_hint) = build_hints_dict(casm.instructions.iter());
    let mut hint_processor = CairoHintProcessor {
        runner: None,
        string_to_hint,
        starknet_state: StarknetState::default(),
        run_resources: RunResources::default(),
        syscalls_used_resources: Default::default(),
    };
    let bytecode: Vec<BigInt> =
        casm.instructions.iter().flat_map(|instruction| instruction.assemble().encode()).collect();

    let RunFunctionResult { memory, ap, .. } = run_function(
        &mut VirtualMachine::new(true),
        bytecode.iter(),
        vec![],
        |_| Ok(()),
        &mut hint_processor,
        hints_dict,
    )
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
        as_cairo_short_string(&felt_str!(
            "30313233343536373839303132333435363738393031323334353637383930",
            16
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
            &felt_str!("30313233343536373839303132333435363738393031323334353637383930", 16),
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
            &felt_str!("100000000000000000000000000000000000000000000000000000000000000", 16),
            32
        ),
        None
    );
}

#[test]
fn test_format_for_debug() {
    // Valid short string.
    let felts = vec![felt_str!("68656c6c6f", 16)];
    assert_eq!(format_for_debug(felts.into_iter()), "[DEBUG]\t0x68656c6c6f ('hello')\n");

    // felt252
    let felts = vec![Felt252::from(1)];
    assert_eq!(format_for_debug(felts.into_iter()), "[DEBUG]\t0x1\n");

    // Valid string with < 31 characters (no full words).
    let felts = vec![
        felt_str!(BYTE_ARRAY_MAGIC, 16),
        Felt252::from(0),                                    // No full words.
        felt_str!("73686f72742c2062757420737472696e67", 16), // 'short, but string'
        Felt252::from(17),                                   // pending word length
    ];
    assert_eq!(format_for_debug(felts.into_iter()), "short, but string");

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
        format_for_debug(felts.into_iter()),
        "This is a long string with more than 31 characters."
    );

    // Only magic.
    let felts = vec![felt_str!(BYTE_ARRAY_MAGIC, 16)];
    assert_eq!(
        format_for_debug(felts.into_iter()),
        "[DEBUG]\t0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3\n"
    );

    // num_full_words > usize.
    let felts = vec![felt_str!(BYTE_ARRAY_MAGIC, 16), felt_str!("100000000", 16)];
    assert_eq!(
        format_for_debug(felts.into_iter()),
        indoc!(
            "[DEBUG]\t0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3
                [DEBUG]\t0x100000000
                "
        )
    );

    // Not enough data after num_full_words.
    let felts = vec![felt_str!(BYTE_ARRAY_MAGIC, 16), Felt252::from(0)];
    assert_eq!(
        format_for_debug(felts.into_iter()),
        "[DEBUG]\t0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3\n[DEBUG]\t0x0 \
         ('')\n"
    );

    // Not enough full words.
    let felts =
        vec![felt_str!(BYTE_ARRAY_MAGIC, 16), Felt252::from(1), Felt252::from(0), Felt252::from(0)];
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
        felt_str!(BYTE_ARRAY_MAGIC, 16),
        Felt252::from(1),
        felt_str!("161616161616161616161616161616161616161616161616161616161616161", 16),
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
        felt_str!(BYTE_ARRAY_MAGIC, 16),
        Felt252::from(0),
        Felt252::from(0),
        felt_str!("100000000", 16),
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
        felt_str!(BYTE_ARRAY_MAGIC, 16),
        // No full words.
        Felt252::from(0),
        // 'a'
        felt_str!("61", 16),
        // pending word length. Bigger than the actual data in the pending word.
        Felt252::from(2),
    ];
    assert_eq!(format_for_debug(felts.into_iter()), "\\0a");

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
        felt_str!(BYTE_ARRAY_MAGIC, 16),
        // No full word.
        Felt252::from(0),
        // pending word: 'Hello\0world'
        felt_str!("48656c6c6f00776f726c64", 16),
        // pending word length
        Felt252::from(11),
    ];
    assert_eq!(format_for_debug(felts.into_iter()), "Hello\\0world");

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
    assert_eq!(format_for_debug(felts.into_iter()), "Hello\\x11world");

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
    assert_eq!(format_for_debug(felts.into_iter()), "Hello\nworld");

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
    let salt = felt_str!("122660764594045088044512115");
    let deployer_address = Felt252::from(0x01);
    let class_hash =
        felt_str!("1779576919126046589190499439779938629977579841313883525093195577363779864274");
    let calldata = vec![deployer_address.clone(), salt.clone()];
    let deployed_contract_address =
        calculate_contract_address(&salt, &class_hash, &calldata, &deployer_address);

    assert_eq!(
        felt_str!("2288343933438457476985546536845198482236255384896285993520343604559094835567"),
        deployed_contract_address
    );
}
