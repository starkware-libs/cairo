use itertools::Itertools;
use num_bigint::{BigInt, Sign};
use test_case::test_case;

use crate::inline::CasmContext;
use crate::run::{run_function, run_function_return_values};
use crate::{casm, deref};

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
fn test_runner(function: CasmContext, n_returns: usize, expected: &[i128]) {
    assert_eq!(
        run_function_return_values(function.instructions, n_returns).expect("Running code failed."),
        expected.iter().copied().map(BigInt::from).collect_vec()
    );
}

#[test]
fn test_allocate_segment() {
    let (memory, ap) = run_function(
        casm! {
            [ap] = 1337, ap++;
            %{ memory[ap] = segments.add() %}
            [ap - 1] = [[&deref!([ap])]];
            ret;
        }
        .instructions,
    )
    .expect("Running code failed.");
    let ptr = memory[ap].as_ref().expect("Uninitialized value.");
    let (Sign::Plus, digits) = ptr.to_u64_digits() else {panic!("Negative number.");};
    let [ptr] = &digits[..] else {panic!("Number not in index range.");};
    assert_eq!(memory[*ptr as usize], Some(BigInt::from(1337)));
}
