use num_bigint::BigUint;
use num_integer::Integer;
use test_case::test_case;

use crate::casm_run::circuit::invert_or_nullify;

#[test_case(
    0_u32,
    8_u32,
    1_u32;
    "invert_or_nullify(0, 8)"
)]
#[test_case(
    1_u32,
    8_u32,
    1_u32;
    "invert_or_nullify(1, 8)"
)]
#[test_case(
    2_u32,
    8_u32,
    4_u32;
    "invert_or_nullify(2, 8)"
)]
#[test_case(
    3_u32,
    8_u32,
    3_u32;
    "invert_or_nullify(3, 8)"
)]
#[test_case(
    4_u32,
    8_u32,
    2_u32;
    "invert_or_nullify(4, 8)"
)]
#[test_case(
    5_u32,
    8_u32,
    5_u32;
    "invert_or_nullify(5, 8)"
)]
#[test_case(
    6_u32,
    8_u32,
    4_u32;
    "invert_or_nullify(6, 8)"
)]
#[test_case(
    7_u32,
    8_u32,
    7_u32;
    "invert_or_nullify(7, 8)"
)]
// Tests with prime modulus 7
#[test_case(
    1_u32,
    7_u32,
    1_u32;
    "invert_or_nullify(1, 7) - prime modulus"
)]
#[test_case(
    2_u32,
    7_u32,
    4_u32;
    "invert_or_nullify(2, 7) - prime modulus"
)]
#[test_case(
    3_u32,
    7_u32,
    5_u32;
    "invert_or_nullify(3, 7) - prime modulus"
)]
#[test_case(
    0_u32,
    7_u32,
    1_u32;
    "invert_or_nullify(0, 7) - prime modulus, non-invertible"
)]
// Tests with prime modulus 11
#[test_case(
    3_u32,
    11_u32,
    4_u32;
    "invert_or_nullify(3, 11) - prime modulus"
)]
#[test_case(
    5_u32,
    11_u32,
    9_u32;
    "invert_or_nullify(5, 11) - prime modulus"
)]
// Tests with composite modulus 9 (3^2)
#[test_case(
    1_u32,
    9_u32,
    1_u32;
    "invert_or_nullify(1, 9) - composite modulus"
)]
#[test_case(
    2_u32,
    9_u32,
    5_u32;
    "invert_or_nullify(2, 9) - composite modulus"
)]
#[test_case(
    3_u32,
    9_u32,
    3_u32;
    "invert_or_nullify(3, 9) - composite modulus, non-invertible"
)]
#[test_case(
    4_u32,
    9_u32,
    7_u32;
    "invert_or_nullify(4, 9) - composite modulus"
)]
// Tests with composite modulus 12 (2^2 * 3)
#[test_case(
    1_u32,
    12_u32,
    1_u32;
    "invert_or_nullify(1, 12) - composite modulus"
)]
#[test_case(
    5_u32,
    12_u32,
    5_u32;
    "invert_or_nullify(5, 12) - composite modulus"
)]
#[test_case(
    2_u32,
    12_u32,
    6_u32;
    "invert_or_nullify(2, 12) - composite modulus, non-invertible"
)]
// Tests with composite modulus 15 (3 * 5)
#[test_case(
    2_u32,
    15_u32,
    8_u32;
    "invert_or_nullify(2, 15) - composite modulus"
)]
#[test_case(
    7_u32,
    15_u32,
    13_u32;
    "invert_or_nullify(7, 15) - composite modulus"
)]
#[test_case(
    3_u32,
    15_u32,
    5_u32;
    "invert_or_nullify(3, 15) - composite modulus, non-invertible"
)]

fn test_invert_or_nullify(input: u32, modulus: u32, output: u32) {
    let (success, nullifier) = invert_or_nullify(BigUint::from(input), &BigUint::from(modulus));
    assert_eq!(success, (input * output).mod_floor(&modulus) == 1_u32);
    assert_eq!(nullifier, BigUint::from(output));
}
