use num_bigint::BigInt;
use test_case::test_case;

use crate::casm;
use crate::inline::CasmContext;
use crate::run::run_function;

fn as_felts(nums: &[i128]) -> Vec<BigInt> {
    nums.iter().map(|num| (BigInt::from(*num))).collect()
}

#[test_case(
    casm! {
        [ap] = (-5), ap++;
        [ap] = 7, ap++;
        ret;
    },
    2,
    &[-5, 7]
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
fn test_runner(function: CasmContext, n_returns: usize, expected: &[i128]) {
    assert_eq!(run_function(function, n_returns), as_felts(expected));
}
