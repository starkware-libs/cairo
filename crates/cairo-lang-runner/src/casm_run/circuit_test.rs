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

fn test_invert_or_nullify(input: u32, modulus: u32, output: u32) {
    let (success, nullifier) = invert_or_nullify(BigUint::from(input), &BigUint::from(modulus));
    assert_eq!(success, (input * output).mod_floor(&modulus) == 1_u32);
    assert_eq!(nullifier, BigUint::from(output));
}
