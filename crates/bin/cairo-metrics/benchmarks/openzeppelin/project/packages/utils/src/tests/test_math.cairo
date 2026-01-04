use core::integer::{u512, u512_safe_div_rem_by_u256};
use core::num::traits::{Bounded, OverflowingAdd};
use crate::math;
use crate::math::Rounding;

//
// average
//

#[cfg(feature: 'fuzzing')]
#[test]
#[fuzzer]
fn test_average_u8(a: u8, b: u8) {
    let actual = math::average(a, b);

    let a: u256 = a.into();
    let b: u256 = b.into();
    let expected = (a + b) / 2;

    assert_eq!(actual, expected.try_into().unwrap());
}

#[cfg(feature: 'fuzzing')]
#[test]
#[fuzzer]
fn test_average_u16(a: u16, b: u16) {
    let actual = math::average(a, b);

    let a: u256 = a.into();
    let b: u256 = b.into();
    let expected = (a + b) / 2;

    assert_eq!(actual, expected.try_into().unwrap());
}

#[cfg(feature: 'fuzzing')]
#[test]
#[fuzzer]
fn test_average_u32(a: u32, b: u32) {
    let actual = math::average(a, b);

    let a: u256 = a.into();
    let b: u256 = b.into();
    let expected = (a + b) / 2;

    assert_eq!(actual, expected.try_into().unwrap());
}

#[cfg(feature: 'fuzzing')]
#[test]
#[fuzzer]
fn test_average_u64(a: u64, b: u64) {
    let actual = math::average(a, b);

    let a: u256 = a.into();
    let b: u256 = b.into();
    let expected = (a + b) / 2;

    assert_eq!(actual, expected.try_into().unwrap());
}

#[cfg(feature: 'fuzzing')]
#[test]
#[fuzzer]
fn test_average_u128(a: u128, b: u128) {
    let actual = math::average(a, b);

    let a: u256 = a.into();
    let b: u256 = b.into();
    let expected = (a + b) / 2;

    assert_eq!(actual, expected.try_into().unwrap());
}

#[cfg(feature: 'fuzzing')]
#[test]
#[fuzzer]
fn test_average_u256(a: u256, b: u256) {
    let actual = math::average(a, b);
    let mut expected = 0;

    let (sum, overflow) = a.overflowing_add(b);
    if !overflow {
        expected = sum / 2;
    } else {
        let u512_sum = u512 { limb0: sum.low, limb1: sum.high, limb2: 1, limb3: 0 };
        let (res, _) = u512_safe_div_rem_by_u256(u512_sum, 2);
        expected = res.try_into().unwrap();
    }

    assert_eq!(actual, expected);
}

//
// mul_div
//

#[test]
#[should_panic(expected: 'mul_div division by zero')]
fn test_mul_div_divide_by_zero() {
    let x = 1;
    let y = 1;
    let denominator = 0;

    math::u256_mul_div(x, y, denominator, Rounding::Floor);
}

#[test]
#[should_panic(expected: 'mul_div quotient > u256')]
fn test_mul_div_result_gt_u256() {
    let x = 5;
    let y = Bounded::MAX;
    let denominator = 2;

    math::u256_mul_div(x, y, denominator, Rounding::Floor);
}

#[test]
fn test_mul_div_round_down_small_values() {
    let round_down = array![Rounding::Floor, Rounding::Trunc];
    let args_list = array![ // (x, y, denominator, expected result)
    (3, 4, 5, 2), (3, 5, 5, 3)]
        .span();

    for rounding in round_down {
        for args in args_list {
            let (x, y, denominator, expected) = args;
            assert_eq!(math::u256_mul_div(*x, *y, *denominator, rounding), *expected);
        }
    }
}

#[test]
fn test_mul_div_round_down_large_values() {
    let round_down = array![Rounding::Floor, Rounding::Trunc];
    let u256_max: u256 = Bounded::MAX;
    let args_list = array![
        // (x, y, denominator, expected result)
        (42, u256_max - 1, u256_max, 41),
        (17, u256_max, u256_max, 17),
        (u256_max - 1, u256_max - 1, u256_max, u256_max - 2),
        (u256_max, u256_max - 1, u256_max, u256_max - 1),
        (u256_max, u256_max, u256_max, u256_max),
    ]
        .span();

    for rounding in round_down {
        for args in args_list {
            let (x, y, denominator, expected) = args;
            assert_eq!(math::u256_mul_div(*x, *y, *denominator, rounding), *expected);
        };
    };
}

#[test]
fn test_mul_div_round_up_small_values() {
    let round_up = array![Rounding::Ceil, Rounding::Expand];
    let args_list = array![ // (x, y, denominator, expected result)
    (3, 4, 5, 3), (3, 5, 5, 3)]
        .span();

    for rounding in round_up {
        for args in args_list {
            let (x, y, denominator, expected) = args;
            assert_eq!(math::u256_mul_div(*x, *y, *denominator, rounding), *expected);
        }
    }
}

#[test]
fn test_mul_div_round_up_large_values() {
    let round_up = array![Rounding::Ceil, Rounding::Expand];
    let u256_max: u256 = Bounded::MAX;
    let args_list = array![
        // (x, y, denominator, expected result)
        (42, u256_max - 1, u256_max, 42),
        (17, u256_max, u256_max, 17),
        (u256_max - 1, u256_max - 1, u256_max, u256_max - 1),
        (u256_max, u256_max - 1, u256_max, u256_max - 1),
        (u256_max, u256_max, u256_max, u256_max),
    ]
        .span();

    for rounding in round_up {
        for args in args_list {
            let (x, y, denominator, expected) = args;
            assert_eq!(math::u256_mul_div(*x, *y, *denominator, rounding), *expected);
        };
    };
}
