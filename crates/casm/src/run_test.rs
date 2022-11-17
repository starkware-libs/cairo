use cairo_rs::types::relocatable::MaybeRelocatable;
use num_bigint::BigInt;
use test_case::test_case;

use crate::casm;
use crate::inline::CasmContext;
use crate::run::run_function;

#[test_case(
    casm! {
        [ap] = (-5), ap++;
        [ap] = 7, ap++;
        ret;
    },
    2,
    &[(-5), 7]
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
    &[579, 123, 456]
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
    &[1, 5, 0, 3, 4]
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
    &[39, 1, 84]
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
    &[5, 39, 7, 4]
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
    &[377]
)]
fn test_runner(function: CasmContext, n_returns: usize, expected: &[i128]) {
    assert_eq!(
        run_function(function.instructions, n_returns),
        expected
            .iter()
            .map(|num| MaybeRelocatable::Int(BigInt::from(*num)))
            .collect::<Vec<MaybeRelocatable>>()
    );
}

#[test_case(
    casm! {
        %{ memory[ap] = segments.add() %}
        ap += 1;
        ret;
    },
    1,
    &[(4, 0)]
)]
fn test_runner_relocatable_returns(
    function: CasmContext,
    n_returns: usize,
    expected: &[(isize, usize)],
) {
    assert_eq!(
        run_function(function.instructions, n_returns),
        expected
            .iter()
            .map(|pair| MaybeRelocatable::from(*pair))
            .collect::<Vec<MaybeRelocatable>>()
    );
}
