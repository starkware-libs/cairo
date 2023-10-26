use cairo_felt::{felt_str, Felt252};
use cairo_lang_casm::inline::CasmContext;
use cairo_lang_casm::{casm, deref};
use cairo_vm::vm::runners::cairo_runner::RunResources;
use cairo_vm::vm::vm_core::VirtualMachine;
use itertools::Itertools;
use num_traits::ToPrimitive;
use test_case::test_case;

use crate::casm_run::run_function;
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
    };

    let (cells, ap) = run_function(
        &mut VirtualMachine::new(true),
        function.instructions.iter(),
        vec![],
        |_| Ok(()),
        &mut hint_processor,
        hints_dict,
    )
    .expect("Running code failed.");
    let cells = cells.into_iter().skip(ap - n_returns);
    assert_eq!(
        cells.take(n_returns).map(|cell| cell.unwrap()).collect_vec(),
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
    };

    let (memory, ap) = run_function(
        &mut VirtualMachine::new(true),
        casm.instructions.iter(),
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
