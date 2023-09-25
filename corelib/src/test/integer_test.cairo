use integer::{
    BoundedInt, u128_wrapping_sub, u16_sqrt, u32_sqrt, u64_sqrt, u8_sqrt, u512, u256_wide_mul,
    u256_as_non_zero, u512_safe_div_rem_by_u256, u128_as_non_zero
};
use test::test_utils::{assert_eq, assert_ne, assert_le, assert_lt, assert_gt, assert_ge};

#[test]
fn test_u8_operators() {
    assert_eq(@1_u8, @1_u8, '1 == 1');
    assert_ne(@1_u8, @2_u8, '1 != 2');
    assert_eq(@(1_u8 + 3_u8), @4_u8, '1 + 3 == 4');
    assert_eq(@(3_u8 + 6_u8), @9_u8, '3 + 6 == 9');
    assert_eq(@(3_u8 - 1_u8), @2_u8, '3 - 1 == 2');
    assert_eq(@(1_u8 * 3_u8), @3_u8, '1 * 3 == 3');
    assert_eq(@(2_u8 * 4_u8), @8_u8, '2 * 4 == 8');
    assert_eq(@(19_u8 / 7_u8), @2_u8, '19 / 7 == 2');
    assert_eq(@(19_u8 % 7_u8), @5_u8, '19 % 7 == 5');
    assert_eq(@(231_u8 - 131_u8), @100_u8, '231-131=100');
    assert_eq(@((1_u8 | 2_u8)), @3_u8, '1 | 2 == 3');
    assert_eq(@((1_u8 & 2_u8)), @0_u8, '1 & 2 == 0');
    assert_eq(@((1_u8 ^ 2_u8)), @3_u8, '1 ^ 2 == 3');
    assert_eq(@((2_u8 | 2_u8)), @2_u8, '2 | 2 == 2');
    assert_eq(@((2_u8 & 2_u8)), @2_u8, '2 & 2 == 2');
    assert_eq(@((2_u8 & 3_u8)), @2_u8, '2 & 3 == 2');
    assert_eq(@((3_u8 ^ 6_u8)), @5_u8, '3 ^ 6 == 5');
    assert_lt(1_u8, 4_u8, '1 < 4');
    assert_le(1_u8, 4_u8, '1 <= 4');
    assert(!(4_u8 < 4_u8), '!(4 < 4)');
    assert_le(5_u8, 5_u8, '5 <= 5');
    assert(!(5_u8 <= 4_u8), '!(5 <= 8)');
    assert_gt(5_u8, 2_u8, '5 > 2');
    assert_ge(5_u8, 2_u8, '5 >= 2');
    assert(!(3_u8 > 3_u8), '!(3 > 3)');
    assert_ge(3_u8, 3_u8, '3 >= 3');
    assert_eq(@u8_sqrt(9), @3, 'u8_sqrt(9) == 3');
    assert_eq(@u8_sqrt(10), @3, 'u8_sqrt(10) == 3');
    assert_eq(@u8_sqrt(0x40), @0x8, 'u8_sqrt(2^6) == 2^3');
    assert_eq(@u8_sqrt(0xff), @0xf, 'Wrong square root result.');
    assert_eq(@u8_sqrt(1), @1, 'u8_sqrt(1) == 1');
    assert_eq(@u8_sqrt(0), @0, 'u8_sqrt(0) == 0');
    assert_eq(@~0x00_u8, @0xff, '~0x00 == 0xff');
    assert_eq(@~0x81_u8, @0x7e, '~0x81 == 0x7e');
}

#[test]
#[should_panic]
fn test_u8_sub_overflow_1() {
    0_u8 - 1_u8;
}

#[test]
#[should_panic]
fn test_u8_sub_overflow_2() {
    0_u8 - 3_u8;
}

#[test]
#[should_panic]
fn test_u8_sub_overflow_3() {
    1_u8 - 3_u8;
}

#[test]
#[should_panic]
fn test_u8_sub_overflow_4() {
    100_u8 - 250_u8;
}

#[test]
#[should_panic]
fn test_u8_add_overflow_1() {
    128_u8 + 128_u8;
}

#[test]
#[should_panic]
fn test_u8_add_overflow_2() {
    200_u8 + 60_u8;
}

#[test]
#[should_panic]
fn test_u8_mul_overflow_1() {
    0x10_u8 * 0x10_u8;
}

#[test]
#[should_panic]
fn test_u8_mul_overflow_2() {
    0x11_u8 * 0x10_u8;
}

#[test]
#[should_panic]
fn test_u8_mul_overflow_3() {
    2_u8 * 0x80_u8;
}

#[test]
#[should_panic]
fn test_u8_div_by_0() {
    2_u8 / 0_u8;
}

#[test]
#[should_panic]
fn test_u8_mod_by_0() {
    2_u8 % 0_u8;
}

#[test]
fn test_u16_operators() {
    assert_eq(@1_u16, @1_u16, '1 == 1');
    assert_ne(@1_u16, @2_u16, '1 != 2');
    assert_eq(@(1_u16 + 3_u16), @4_u16, '1 + 3 == 4');
    assert_eq(@(3_u16 + 6_u16), @9_u16, '3 + 6 == 9');
    assert_eq(@(3_u16 - 1_u16), @2_u16, '3 - 1 == 2');
    assert_eq(@(231_u16 - 131_u16), @100_u16, '231-131=100');
    assert_eq(@(1_u16 * 3_u16), @3_u16, '1 * 3 == 3');
    assert_eq(@(2_u16 * 4_u16), @8_u16, '2 * 4 == 8');
    assert_eq(@(51725_u16 / 7_u16), @7389_u16, '51725 / 7 == 7389');
    assert_eq(@(51725_u16 % 7_u16), @2_u16, '51725 % 7 == 2');
    assert_eq(@((1_u16 | 2_u16)), @3_u16, '1 | 2 == 3');
    assert_eq(@((1_u16 & 2_u16)), @0_u16, '1 & 2 == 0');
    assert_eq(@((1_u16 ^ 2_u16)), @3_u16, '1 ^ 2 == 3');
    assert_eq(@((2_u16 | 2_u16)), @2_u16, '2 | 2 == 2');
    assert_eq(@((2_u16 & 2_u16)), @2_u16, '2 & 2 == 2');
    assert_eq(@((2_u16 & 3_u16)), @2_u16, '2 & 3 == 2');
    assert_eq(@((3_u16 ^ 6_u16)), @5_u16, '3 ^ 6 == 5');
    assert_lt(1_u16, 4_u16, '1 < 4');
    assert_le(1_u16, 4_u16, '1 <= 4');
    assert(!(4_u16 < 4_u16), '!(4 < 4)');
    assert_le(4_u16, 4_u16, '4 <= 4');
    assert_gt(5_u16, 2_u16, '5 > 2');
    assert_ge(5_u16, 2_u16, '5 >= 2');
    assert(!(3_u16 > 3_u16), '!(3 > 3)');
    assert_ge(3_u16, 3_u16, '3 >= 3');
    assert_eq(@u16_sqrt(9), @3, 'u16_sqrt(9) == 3');
    assert_eq(@u16_sqrt(10), @3, 'u16_sqrt(10) == 3');
    assert_eq(@u16_sqrt(0x400), @0x20, 'u16_sqrt(2^10) == 2^5');
    assert_eq(@u16_sqrt(0xffff), @0xff, 'Wrong square root result.');
    assert_eq(@u16_sqrt(1), @1, 'u64_sqrt(1) == 1');
    assert_eq(@u16_sqrt(0), @0, 'u64_sqrt(0) == 0');
    assert_eq(@~0x0000_u16, @0xffff, '~0x0000 == 0xffff');
    assert_eq(@~0x8421_u16, @0x7bde, '~0x8421 == 0x7bde');
}

#[test]
#[should_panic]
fn test_u16_sub_overflow_1() {
    0_u16 - 1_u16;
}

#[test]
#[should_panic]
fn test_u16_sub_overflow_2() {
    0_u16 - 3_u16;
}

#[test]
#[should_panic]
fn test_u16_sub_overflow_3() {
    1_u16 - 3_u16;
}

#[test]
#[should_panic]
fn test_u16_sub_overflow_4() {
    100_u16 - 250_u16;
}

#[test]
#[should_panic]
fn test_u16_add_overflow_1() {
    0x8000_u16 + 0x8000_u16;
}

#[test]
#[should_panic]
fn test_u16_add_overflow_2() {
    0x9000_u16 + 0x8001_u16;
}

#[test]
#[should_panic]
fn test_u16_mul_overflow_1() {
    0x100_u16 * 0x100_u16;
}

#[test]
#[should_panic]
fn test_u16_mul_overflow_2() {
    0x101_u16 * 0x100_u16;
}

#[test]
#[should_panic]
fn test_u16_mul_overflow_3() {
    2_u16 * 0x8000_u16;
}

#[test]
#[should_panic]
fn test_u16_div_by_0() {
    2_u16 / 0_u16;
}

#[test]
#[should_panic]
fn test_u16_mod_by_0() {
    0_u16 % 0_u16;
}

#[test]
fn test_u32_operators() {
    assert_eq(@1_u32, @1_u32, '1 == 1');
    assert_ne(@1_u32, @2_u32, '1 != 2');
    assert_eq(@(1_u32 + 3_u32), @4_u32, '1 + 3 == 4');
    assert_eq(@(3_u32 + 6_u32), @9_u32, '3 + 6 == 9');
    assert_eq(@(3_u32 - 1_u32), @2_u32, '3 - 1 == 2');
    assert_eq(@(231_u32 - 131_u32), @100_u32, '231-131=100');
    assert_eq(@(1_u32 * 3_u32), @3_u32, '1 * 3 == 3');
    assert_eq(@(2_u32 * 4_u32), @8_u32, '2 * 4 == 8');
    assert_eq(@(510670725_u32 / 7_u32), @72952960_u32, '510670725 / 7 == 72952960');
    assert_eq(@(510670725_u32 % 7_u32), @5_u32, '510670725 % 7 == 5');
    assert_eq(@((1_u32 | 2_u32)), @3_u32, '1 | 2 == 3');
    assert_eq(@((1_u32 & 2_u32)), @0_u32, '1 & 2 == 0');
    assert_eq(@((1_u32 ^ 2_u32)), @3_u32, '1 ^ 2 == 3');
    assert_eq(@((2_u32 | 2_u32)), @2_u32, '2 | 2 == 2');
    assert_eq(@((2_u32 & 2_u32)), @2_u32, '2 & 2 == 2');
    assert_eq(@((2_u32 & 3_u32)), @2_u32, '2 & 3 == 2');
    assert_eq(@((3_u32 ^ 6_u32)), @5_u32, '3 ^ 6 == 5');
    assert_lt(1_u32, 4_u32, '1 < 4');
    assert_le(1_u32, 4_u32, '1 <= 4');
    assert(!(4_u32 < 4_u32), '!(4 < 4)');
    assert_le(4_u32, 4_u32, '4 <= 4');
    assert_gt(5_u32, 2_u32, '5 > 2');
    assert_ge(5_u32, 2_u32, '5 >= 2');
    assert(!(3_u32 > 3_u32), '!(3 > 3)');
    assert_ge(3_u32, 3_u32, '3 >= 3');
    assert_eq(@u32_sqrt(9), @3, 'u32_sqrt(9) == 3');
    assert_eq(@u32_sqrt(10), @3, 'u32_sqrt(10) == 3');
    assert_eq(@u32_sqrt(0x100000), @0x400, 'u32_sqrt(2^20) == 2^10');
    assert_eq(@u32_sqrt(0xffffffff), @0xffff, 'Wrong square root result.');
    assert_eq(@u32_sqrt(1), @1, 'u64_sqrt(1) == 1');
    assert_eq(@u32_sqrt(0), @0, 'u64_sqrt(0) == 0');
    assert_eq(@~0x00000000_u32, @0xffffffff, '~0x00000000 == 0xffffffff');
    assert_eq(@~0x12345678_u32, @0xedcba987, '~0x12345678 == 0xedcba987');
}

#[test]
#[should_panic]
fn test_u32_sub_overflow_1() {
    0_u32 - 1_u32;
}

#[test]
#[should_panic]
fn test_u32_sub_overflow_2() {
    0_u32 - 3_u32;
}

#[test]
#[should_panic]
fn test_u32_sub_overflow_3() {
    1_u32 - 3_u32;
}

#[test]
#[should_panic]
fn test_u32_sub_overflow_4() {
    100_u32 - 250_u32;
}

#[test]
#[should_panic]
fn test_u32_add_overflow_1() {
    0x80000000_u32 + 0x80000000_u32;
}

#[test]
#[should_panic]
fn test_u32_add_overflow_2() {
    0x90000000_u32 + 0x80000001_u32;
}

#[test]
#[should_panic]
fn test_u32_mul_overflow_1() {
    0x10000_u32 * 0x10000_u32;
}

#[test]
#[should_panic]
fn test_u32_mul_overflow_2() {
    0x10001_u32 * 0x10000_u32;
}

#[test]
#[should_panic]
fn test_u32_mul_overflow_3() {
    2_u32 * 0x80000000_u32;
}

#[test]
#[should_panic]
fn test_u32_div_by_0() {
    2_u32 / 0_u32;
}

#[test]
#[should_panic]
fn test_u32_mod_by_0() {
    0_u32 % 0_u32;
}

#[test]
fn test_u64_operators() {
    assert_eq(@1_u64, @1_u64, '1 == 1');
    assert_ne(@1_u64, @2_u64, '1 != 2');
    assert_eq(@(1_u64 + 3_u64), @4_u64, '1 + 3 == 4');
    assert_eq(@(3_u64 + 6_u64), @9_u64, '3 + 6 == 9');
    assert_eq(@(3_u64 - 1_u64), @2_u64, '3 - 1 == 2');
    assert_eq(@(231_u64 - 131_u64), @100_u64, '231-131=100');
    assert_eq(@(1_u64 * 3_u64), @3_u64, '1 * 3 == 3');
    assert_eq(@(2_u64 * 4_u64), @8_u64, '2 * 4 == 8');
    assert_eq(
        @(5010670477878974275_u64 / 7_u64), @715810068268424896_u64, 'Wrong division result.'
    );
    assert_eq(@(5010670477878974275_u64 % 7_u64), @3_u64, '5010670477878974275 % 7 == 3');
    assert_eq(@((1_u64 | 2_u64)), @3_u64, '1 | 2 == 3');
    assert_eq(@((1_u64 & 2_u64)), @0_u64, '1 & 2 == 0');
    assert_eq(@((1_u64 ^ 2_u64)), @3_u64, '1 ^ 2 == 3');
    assert_eq(@((2_u64 | 2_u64)), @2_u64, '2 | 2 == 2');
    assert_eq(@((2_u64 & 2_u64)), @2_u64, '2 & 2 == 2');
    assert_eq(@((2_u64 & 3_u64)), @2_u64, '2 & 3 == 2');
    assert_eq(@((3_u64 ^ 6_u64)), @5_u64, '3 ^ 6 == 5');
    assert_lt(1_u64, 4_u64, '1 < 4');
    assert_le(1_u64, 4_u64, '1 <= 4');
    assert(!(4_u64 < 4_u64), '!(4 < 4)');
    assert_le(4_u64, 4_u64, '4 <= 4');
    assert_gt(5_u64, 2_u64, '5 > 2');
    assert_ge(5_u64, 2_u64, '5 >= 2');
    assert(!(3_u64 > 3_u64), '!(3 > 3)');
    assert_ge(3_u64, 3_u64, '3 >= 3');
    assert_eq(@u64_sqrt(9), @3, 'u64_sqrt(9) == 3');
    assert_eq(@u64_sqrt(10), @3, 'u64_sqrt(10) == 3');
    assert_eq(@u64_sqrt(0x10000000000), @0x100000, 'u64_sqrt(2^40) == 2^20');
    assert_eq(@u64_sqrt(0xffffffffffffffff), @0xffffffff, 'Wrong square root result.');
    assert_eq(@u64_sqrt(1), @1, 'u64_sqrt(1) == 1');
    assert_eq(@u64_sqrt(0), @0, 'u64_sqrt(0) == 0');
    assert_eq(@~0x0000000000000000_u64, @0xffffffffffffffff, '~0x0..0 == 0xf..f');
    assert_eq(@~0x123456789abcdef1_u64, @0xedcba9876543210e, '~0x12..ef1 == 0xed..10e');
}

#[test]
#[should_panic]
fn test_u64_sub_overflow_1() {
    0_u64 - 1_u64;
}

#[test]
#[should_panic]
fn test_u64_sub_overflow_2() {
    0_u64 - 3_u64;
}

#[test]
#[should_panic]
fn test_u64_sub_overflow_3() {
    1_u64 - 3_u64;
}

#[test]
#[should_panic]
fn test_u64_sub_overflow_4() {
    100_u64 - 250_u64;
}

#[test]
#[should_panic]
fn test_u64_add_overflow_1() {
    0x8000000000000000_u64 + 0x8000000000000000_u64;
}

#[test]
#[should_panic]
fn test_u64_add_overflow_2() {
    0x9000000000000000_u64 + 0x8000000000000001_u64;
}

#[test]
#[should_panic]
fn test_u64_mul_overflow_1() {
    0x100000000_u64 * 0x100000000_u64;
}

#[test]
#[should_panic]
fn test_u64_mul_overflow_2() {
    0x100000001_u64 * 0x100000000_u64;
}

#[test]
#[should_panic]
fn test_u64_mul_overflow_3() {
    2_u64 * 0x8000000000000000_u64;
}

#[test]
#[should_panic]
fn test_u64_div_by_0() {
    2_u64 / 0_u64;
}

#[test]
#[should_panic]
fn test_u64_mod_by_0() {
    0_u64 % 0_u64;
}

#[test]
fn test_u128_operators() {
    assert_eq(@1_u128, @1_u128, '1 == 1');
    assert_ne(@1_u128, @2_u128, '1 != 2');
    assert_eq(@(1_u128 + 3_u128), @4_u128, '1 + 3 == 4');
    assert_eq(@(3_u128 + 6_u128), @9_u128, '3 + 6 == 9');
    assert_eq(@(3_u128 - 1_u128), @2_u128, '3 - 1 == 2');
    assert_eq(@(1231_u128 - 231_u128), @1000_u128, '1231-231=1000');
    assert_eq(@(1_u128 * 3_u128), @3_u128, '1 * 3 == 3');
    assert_eq(@(2_u128 * 4_u128), @8_u128, '2 * 4 == 8');
    assert_eq(@(8_u128 / 2_u128), @4_u128, '8 / 2 == 4');
    assert_eq(@(8_u128 % 2_u128), @0_u128, '8 % 2 == 0');
    assert_eq(@(7_u128 / 3_u128), @2_u128, '7 / 3 == 2');
    assert_eq(@(7_u128 % 3_u128), @1_u128, '7 % 3 == 1');
    assert_lt(1_u128, 4_u128, '1 < 4');
    assert_le(1_u128, 4_u128, '1 <= 4');
    assert(!(4_u128 < 4_u128), '!(4 < 4)');
    assert_le(4_u128, 4_u128, '4 <= 4');
    assert_gt(5_u128, 2_u128, '5 > 2');
    assert_ge(5_u128, 2_u128, '5 >= 2');
    assert(!(3_u128 > 3_u128), '!(3 > 3)');
    assert_ge(3_u128, 3_u128, '3 >= 3');
    assert_eq(@((1_u128 | 2_u128)), @3_u128, '1 | 2 == 3');
    assert_eq(@((1_u128 & 2_u128)), @0_u128, '1 & 2 == 0');
    assert_eq(@((1_u128 ^ 2_u128)), @3_u128, '1 ^ 2 == 3');
    assert_eq(@((2_u128 | 2_u128)), @2_u128, '2 | 2 == 2');
    assert_eq(@((2_u128 & 2_u128)), @2_u128, '2 & 2 == 2');
    assert_eq(@((2_u128 & 3_u128)), @2_u128, '2 & 3 == 2');
    assert_eq(@((3_u128 ^ 6_u128)), @5_u128, '3 ^ 6 == 5');
    assert_eq(@u128_sqrt(9), @3, 'u128_sqrt(9) == 3');
    assert_eq(@u128_sqrt(10), @3, 'u128_sqrt(10) == 3');
    assert_eq(
        @u128_sqrt(0x10000000000000000000000000), @0x4000000000000, 'u128_sqrt(2^100) == 2^50'
    );
    assert_eq(
        @u128_sqrt(0xffffffffffffffffffffffffffffffff),
        @0xffffffffffffffff,
        'Wrong square root result.'
    );
    assert_eq(@u128_sqrt(1), @1, 'u128_sqrt(1) == 1');
    assert_eq(@u128_sqrt(0), @0, 'u128_sqrt(0) == 0');
    assert_eq(
        @~0x00000000000000000000000000000000_u128,
        @0xffffffffffffffffffffffffffffffff,
        '~0x0..0 == 0xf..f'
    );
    assert_eq(
        @~0x123456789abcdef123456789abcdef12_u128,
        @0xedcba9876543210edcba9876543210ed,
        '~0x12..ef12 == 0xed..10ed'
    );
}

fn pow_2_64() -> u128 {
    0x10000000000000000_u128
}

#[test]
#[should_panic]
fn test_u128_sub_overflow_1() {
    0_u128 - 1_u128;
}

#[test]
#[should_panic]
fn test_u128_sub_overflow_2() {
    0_u128 - 3_u128;
}

#[test]
#[should_panic]
fn test_u128_sub_overflow_3() {
    1_u128 - 3_u128;
}

#[test]
#[should_panic]
fn test_u128_sub_overflow_4() {
    100_u128 - 1000_u128;
}

#[test]
fn test_u128_wrapping_sub_1() {
    let max_u128: u128 = BoundedInt::max();
    let should_be_max = u128_wrapping_sub(0_u128, 1_u128);
    assert_eq(@max_u128, @should_be_max, 'Should be max u128')
}

#[test]
fn test_u128_wrapping_sub_2() {
    let max_u128_minus_two: u128 = BoundedInt::max() - 2;
    let should_be_max = u128_wrapping_sub(0_u128, 3_u128);
    assert_eq(@max_u128_minus_two, @should_be_max, 'Should be max u128 - 2')
}

#[test]
fn test_u128_wrapping_sub_3() {
    let max_u128_minus_899: u128 = BoundedInt::max() - 899;
    let should_be_max = u128_wrapping_sub(100, 1000);
    assert_eq(@max_u128_minus_899, @should_be_max, 'Should be max u128 - 899')
}

#[test]
fn test_u128_wrapping_sub_4() {
    let should_be_zero = u128_wrapping_sub(0_u128, 0_u128);
    assert_eq(@should_be_zero, @0, 'Should be 0')
}

#[test]
#[should_panic]
fn test_u128_add_overflow_1() {
    0x80000000000000000000000000000000_u128 + 0x80000000000000000000000000000000_u128;
}

#[test]
#[should_panic]
fn test_u128_add_overflow_2() {
    (0x80000000000000000000000000000000_u128 + 12_u128) + 0x80000000000000000000000000000000_u128;
}

#[test]
#[should_panic]
fn test_u128_mul_overflow_1() {
    pow_2_64() * pow_2_64();
}

#[test]
#[should_panic]
fn test_u128_mul_overflow_2() {
    (pow_2_64() + 1_u128) * pow_2_64();
}

#[test]
#[should_panic]
fn test_u128_mul_overflow_3() {
    2_u128 * 0x80000000000000000000000000000000_u128;
}

#[test]
#[should_panic]
fn test_u128_div_by_0() {
    2_u128 / 0_u128;
}

#[test]
#[should_panic]
fn test_u128_mod_by_0() {
    2_u128 % 0_u128;
}

fn pow_2_127() -> u256 {
    0x80000000000000000000000000000000_u256
}

#[test]
fn test_u256_from_felt252() {
    assert_eq(@1.into(), @1_u256, 'into 1');
    assert_eq(
        @(170141183460469231731687303715884105728 * 2).into(),
        @0x100000000000000000000000000000000_u256,
        'into 2**128'
    );
}

#[test]
fn test_u256_operators() {
    let max_u128 = 0xffffffffffffffffffffffffffffffff_u256;
    assert_eq(
        @(0x100000000000000000000000000000001 + 0x300000000000000000000000000000002),
        @0x400000000000000000000000000000003_u256,
        'no Overflow'
    );
    assert_eq(
        @(0x180000000000000000000000000000000 + 0x380000000000000000000000000000000),
        @0x500000000000000000000000000000000_u256,
        'basic Overflow'
    );
    assert_eq(
        @(0x400000000000000000000000000000003 - 0x100000000000000000000000000000001),
        @0x300000000000000000000000000000002_u256,
        'no UF'
    );
    assert_eq(
        @(0x500000000000000000000000000000000 - 0x180000000000000000000000000000000),
        @0x380000000000000000000000000000000_u256,
        'basic UF'
    );
    assert_eq(
        @(0x400000000000000000000000000000003 * 1),
        @0x400000000000000000000000000000003_u256,
        'mul by 1'
    );
    assert_eq(
        @(0x400000000000000000000000000000003 * 2),
        @0x800000000000000000000000000000006_u256,
        'mul by 2'
    );
    assert_eq(
        @(0x80000000000000000000000000000000 * 2),
        @0x100000000000000000000000000000000_u256,
        'basic mul Overflow'
    );
    assert_eq(
        @(max_u128 * max_u128),
        @0xfffffffffffffffffffffffffffffffe00000000000000000000000000000001_u256,
        'max_u128 * max_u128'
    );
    assert_eq(@(max_u128 * 1), @max_u128, 'max_u128 * 1');
    assert_eq(@(1 * max_u128), @max_u128, '1 * max_u128');
    let v0_2: u256 = 0x000000000000000000000000000000002;
    let v0_3: u256 = 0x000000000000000000000000000000003;
    let v1_1: u256 = 0x100000000000000000000000000000001;
    let v1_2: u256 = 0x100000000000000000000000000000002;
    let v2_0: u256 = 0x200000000000000000000000000000000;
    let v2_1: u256 = 0x200000000000000000000000000000001;
    let v2_2: u256 = 0x200000000000000000000000000000002;
    let v2_3: u256 = 0x200000000000000000000000000000003;
    let v3_0: u256 = 0x300000000000000000000000000000000;
    let v3_2: u256 = 0x300000000000000000000000000000002;
    assert_eq(@(v1_2 | v2_2), @v3_2, '1.2|2.2==3.2');
    assert_eq(@(v2_1 | v2_2), @v2_3, '2.1|2.2==2.3');
    assert_eq(@(v2_2 | v1_2), @v3_2, '2.2|1.2==3.2');
    assert_eq(@(v2_2 | v2_1), @v2_3, '2.2|2.1==2.3');
    assert_eq(@(v1_2 & v2_2), @v0_2, '1.2&2.2==0.2');
    assert_eq(@(v2_1 & v2_2), @v2_0, '2.1&2.2==2.0');
    assert_eq(@(v2_2 & v1_2), @v0_2, '2.2&1.2==0.2');
    assert_eq(@(v2_2 & v2_1), @v2_0, '2.2&2.1==2.0');
    assert_eq(@(v1_2 ^ v2_2), @v3_0, '1.2^2.2==3.0');
    assert_eq(@(v2_1 ^ v2_2), @v0_3, '2.1^2.2==0.3');
    assert_eq(@(v2_2 ^ v1_2), @v3_0, '2.2^1.2==3.0');
    assert_eq(@(v2_2 ^ v2_1), @v0_3, '2.2^2.1==0.3');
    assert_lt(v1_2, v2_2, '1.2<2.2');
    assert_lt(v2_1, v2_2, '2.1<2.2');
    assert(!(v2_2 < v1_2), '2.2<1.2');
    assert(!(v2_2 < v2_1), '2.2<2.1');
    assert(!(v2_2 < v2_2), '2.2<2.2');
    assert_le(v1_2, v2_2, '1.2<=2.2');
    assert_le(v2_1, v2_2, '2.1<=2.2');
    assert(!(v2_2 <= v1_2), '2.2<=1.2');
    assert(!(v2_2 <= v2_1), '2.2<=2.1');
    assert_le(v2_2, v2_2, '2.2<=2.2');
    assert(!(v1_2 > v2_2), '1.2>2.2');
    assert(!(v2_1 > v2_2), '2.1>2.2');
    assert_gt(v2_2, v1_2, '2.2>1.2');
    assert_gt(v2_2, v2_1, '2.2>2.1');
    assert(!(v2_2 > v2_2), '2.2>2.2');
    assert(!(v1_2 >= v2_2), '1.2>=2.2');
    assert(!(v2_1 >= v2_2), '2.1>=2.2');
    assert_ge(v2_2, v1_2, '2.2>=1.2');
    assert_ge(v2_2, v2_1, '2.2>=2.1');
    assert_ge(v2_2, v2_2, '2.2>=2.2');

    assert_eq(@(v3_2 / v1_1), @v0_2, 'u256 div');
    assert_eq(
        @(0x400000000000000000000000000000002 / 3),
        @0x155555555555555555555555555555556_u256,
        'u256 div'
    );
    assert_eq(@(0x400000000000000000000000000000002 % 3), @0_u256, 'u256 mod');
    assert_eq(@(0x10000000000000000 / 0x10000000000000000), @1_u256, 'u256 div');
    assert_eq(@(0x10000000000000000 % 0x10000000000000000), @0_u256, 'u256 mod');
    assert_eq(
        @(0x1000000000000000000000000000000000000000000000000
            / 0x1000000000000000000000000000000000000000000000000),
        @1_u256,
        'u256 div'
    );
    assert_eq(
        @(0x1000000000000000000000000000000000000000000000000 % 0x1000000000000000000000000000000000000000000000000),
        @0_u256,
        'u256 mod'
    );
    assert_eq(@(BoundedInt::max() % 0x100000000), @0xffffffff_u256, 'u256 mod');
    assert_eq(@(BoundedInt::max() % 0x10000000000000000), @0xffffffffffffffff_u256, 'u256 mod');
    assert_eq(
        @(BoundedInt::max() / 0x10000000000000000000000000000000000000000),
        @0xffffffffffffffffffffffff_u256,
        'u256 div'
    );
    assert_eq(
        @(BoundedInt::max() / 0x1000000000000000000000000000000000000000000000000),
        @0xffffffffffffffff_u256,
        'u256 div'
    );
    assert_eq(
        @~max_u128,
        @0xffffffffffffffffffffffffffffffff00000000000000000000000000000000,
        '~0x0..0f..f == 0xf..f0..0'
    );
    assert_eq(
        @~0xffffffffffffffffffffffffffffffff00000000000000000000000000000000,
        @max_u128,
        '~0xf..f0..0 == 0x0..0f..f'
    );
}

#[test]
#[should_panic]
fn test_u256_add_overflow() {
    let v = 0x8000000000000000000000000000000000000000000000000000000000000001_u256;
    v + v;
}

#[test]
#[should_panic]
fn test_u256_sub_overflow() {
    0x100000000000000000000000000000001_u256 - 0x100000000000000000000000000000002;
}

#[test]
#[should_panic]
fn test_u256_mul_overflow_1() {
    0x100000000000000000000000000000001_u256 * 0x100000000000000000000000000000002;
}

#[test]
#[should_panic]
fn test_u256_mul_overflow_2() {
    pow_2_127() * 0x200000000000000000000000000000000;
}

#[test]
fn test_u256_wide_mul() {
    assert_eq(@u256_wide_mul(0, 0), @u512 { limb0: 0, limb1: 0, limb2: 0, limb3: 0 }, '0 * 0 != 0');
    assert_eq(
        @u256_wide_mul(
            0x1001001001001001001001001001001001001001001001001001,
            0x1000100010001000100010001000100010001000100010001000100010001
        ),
        @u512 {
            limb0: 0x33233223222222122112111111011001,
            limb1: 0x54455445544554454444443443343333,
            limb2: 0x21222222322332333333433443444444,
            limb3: 0x1001101111112112
        },
        'long calculation failed'
    );
}

#[test]
fn test_u512_safe_div_rem_by_u256() {
    let zero = u512 { limb0: 0, limb1: 0, limb2: 0, limb3: 0 };
    let one = u512 { limb0: 1, limb1: 0, limb2: 0, limb3: 0 };
    let large_num = u512 {
        limb0: 0x33233223222222122112111111011001,
        limb1: 0x54455445544554454444443443343333,
        limb2: 0x21222222322332333333433443444444,
        limb3: 0x1001101111112112
    };
    let (q, r) = u512_safe_div_rem_by_u256(zero, 1_u256.try_into().unwrap());
    assert(q == zero, '0 / 1 != 0');
    assert(r == 0, '0 % 1 != 0');
    let (q, r) = u512_safe_div_rem_by_u256(one, 1_u256.try_into().unwrap());
    assert(q == one, '1 / 1 != 1');
    assert(r == 0, '1 % 1 != 0');
    let (q, r) = u512_safe_div_rem_by_u256(large_num, 1_u256.try_into().unwrap());
    assert(q == large_num, 'LARGE / 1 != LARGE');
    assert(r == 0, 'LARGE % 1 != 0');
    let (q, r) = u512_safe_div_rem_by_u256(
        large_num, 0x33233223222222122112111111011001_u256.try_into().unwrap()
    );
    assert(
        q == u512 {
            limb0: 0x365ec98ac1c2c57afaff780a20a0b2b1,
            limb1: 0xf3dfa68ede27c4236ef0c6eb66a8e0a2,
            limb2: 0x501e5b7ba7f4ec12,
            limb3: 0
        },
        'large div failed'
    );
    assert(r == 0x1e0eb905027d0150d2618bbd71844d50, 'large rem failed');
}

#[test]
fn test_min() {
    let min_u8: u8 = BoundedInt::min();
    let min_u16: u16 = BoundedInt::min();
    let min_u32: u32 = BoundedInt::min();
    let min_u64: u64 = BoundedInt::min();
    let min_u128: u128 = BoundedInt::min();
    let min_u256: u256 = BoundedInt::min();
    assert_eq(@min_u8, @0_u8, 'not zero');
    assert_eq(@min_u16, @0_u16, 'not zero');
    assert_eq(@min_u32, @0_u32, 'not zero');
    assert_eq(@min_u64, @0_u64, 'not zero');
    assert_eq(@min_u128, @0_u128, 'not zero');
    assert_eq(@min_u256, @0_u256, 'not zero');
}

#[test]
fn test_max() {
    let max_u8: u8 = BoundedInt::max();
    let max_u16: u16 = BoundedInt::max();
    let max_u32: u32 = BoundedInt::max();
    let max_u64: u64 = BoundedInt::max();
    let max_u128: u128 = BoundedInt::max();
    let max_u256: u256 = BoundedInt::max();
    assert_eq(@max_u8, @0xff_u8, 'not max');
    assert_eq(@max_u16, @0xffff_u16, 'not max');
    assert_eq(@max_u32, @0xffffffff_u32, 'not max');
    assert_eq(@max_u64, @0xffffffffffffffff_u64, 'not max');
    assert_eq(@max_u128, @0xffffffffffffffffffffffffffffffff_u128, 'not max');
    assert_eq(
        @max_u256,
        @0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff_u256,
        'not max'
    );
}

#[test]
#[should_panic]
fn test_max_u8_plus_1_overflow() {
    BoundedInt::max() + 1_u8;
}

#[test]
#[should_panic]
fn test_max_u16_plus_1_overflow() {
    BoundedInt::max() + 1_u16;
}

#[test]
#[should_panic]
fn test_max_u32_plus_1_overflow() {
    BoundedInt::max() + 1_u32;
}
#[test]
#[should_panic]
fn test_max_u64_plus_1_overflow() {
    BoundedInt::max() + 1_u64;
}

#[test]
#[should_panic]
fn test_max_u128_plus_1_overflow() {
    BoundedInt::max() + 1_u128;
}

#[test]
#[should_panic]
fn test_max_u256_plus_1_overflow() {
    BoundedInt::max() + Into::<felt252, u256>::into(1);
}

#[test]
fn test_default_values() {
    assert_eq(@Default::default(), @0, '0 == 0');
    assert_eq(@Default::default(), @0_u8, '0 == 0');
    assert_eq(@Default::default(), @0_u16, '0 == 0');
    assert_eq(@Default::default(), @0_u32, '0 == 0');
    assert_eq(@Default::default(), @0_u64, '0 == 0');
    assert_eq(@Default::default(), @0_u128, '0 == 0');
    assert_eq(@Default::default(), @0_u256, '0 == 0');
}

#[test]
fn test_default_felt252dict_values() {
    assert_eq(@Felt252DictValue::zero_default(), @0, '0 == 0');
    assert_eq(@Felt252DictValue::zero_default(), @0_u8, '0 == 0');
    assert_eq(@Felt252DictValue::zero_default(), @0_u16, '0 == 0');
    assert_eq(@Felt252DictValue::zero_default(), @0_u32, '0 == 0');
    assert_eq(@Felt252DictValue::zero_default(), @0_u64, '0 == 0');
    assert_eq(@Felt252DictValue::zero_default(), @0_u128, '0 == 0');
}

#[test]
fn test_u256_sqrt() {
    assert_eq(@u256_sqrt(9.into()), @3, 'u256_sqrt(9) == 3');
    assert_eq(@u256_sqrt(10.into()), @3, 'u256_sqrt(10) == 3');
    assert_eq(
        @u256_sqrt(1267650600228229401496703205376.into()),
        @1125899906842624,
        'u256_sqrt(2^100) == 2^50'
    );
    assert_eq(
        @u256_sqrt(340282366920938463463374607431768211455.into()),
        @18446744073709551615,
        'Wrong square root result.'
    );
    assert_eq(@u256_sqrt(1.into()), @1, 'u256_sqrt(1) == 1');
    assert_eq(@u256_sqrt(0.into()), @0, 'u256_sqrt(0) == 0');

    assert_eq(@u256_sqrt(BoundedInt::max()), @BoundedInt::max(), 'u256::MAX**0.5==u128::MAX');
    let (high, low) = integer::u128_wide_mul(BoundedInt::max(), BoundedInt::max());
    assert_eq(@u256_sqrt(u256 { low, high }), @BoundedInt::max(), '(u128::MAX**2)**0.5==u128::MAX');
}

#[test]
fn test_u256_try_into_felt252() {
    let FELT252_PRIME = 0x800000000000011000000000000000000000000000000000000000000000001_u256;
    assert_eq(@1_u256.try_into().unwrap(), @1_felt252, '1 == 1'_felt252);
    assert_eq(
        @0x800000000000011000000000000000000000000000000000000000000000000_u256.try_into().unwrap(),
        @0x800000000000011000000000000000000000000000000000000000000000000_felt252,
        'P-1 == P-1'_felt252
    );
    assert_eq(
        @0x800000000000010ffffffffffffffffffffffffffffffffffffffffffffffff_u256.try_into().unwrap(),
        @0x800000000000010ffffffffffffffffffffffffffffffffffffffffffffffff_felt252,
        'P-2 == P-2'_felt252
    );
    let f: Option<felt252> = 0x800000000000011000000000000000000000000000000000000000000000001_u256
        .try_into();
    assert(f.is_none(), 'prime is not felt252');
    let f: Option<felt252> = 0x800000000000011000000000000000000000000000000000000000000000002_u256
        .try_into();
    assert(f.is_none(), 'prime+1 is not felt252');
    let f: Option<felt252> = 0x800000000000011000000000000000100000000000000000000000000000001_u256
        .try_into();
    assert(f.is_none(), 'prime+2**128 is not felt252');
}

fn cast_must_pass<
    A,
    B,
    +Drop<A>,
    +Drop<B>,
    +Copy<B>,
    +Copy<A>,
    +PartialEq<A>,
    +PartialEq<B>,
    +BoundedInt<A>,
    +BoundedInt<B>,
    +Into<A, B>,
    +TryInto<B, A>
>(
    ui: A, uj: B
) -> bool {
    uj == ui.into()
        && ui == uj.try_into().unwrap()
        && BoundedInt::<B>::min() == BoundedInt::<A>::min().into()
        && BoundedInt::<A>::min() == BoundedInt::<B>::min().try_into().unwrap()
}
#[test]
fn proper_cast() {
    assert(cast_must_pass(0xFF_u8, 0xFF_u16), 'u8 to_and_fro u16');
    assert(cast_must_pass(0xFF_u8, 0xFF_u32), 'u8 to_and_fro u32');
    assert(cast_must_pass(0xFF_u8, 0xFF_u64), 'u8 to_and_fro u64');
    assert(cast_must_pass(0xFF_u8, 0xFF_u128), 'u8 to_and_fro u128');
    assert(cast_must_pass(0xFFFF_u16, 0xFFFF_u32), 'u16 to_and_fro u32');
    assert(cast_must_pass(0xFFFF_u16, 0xFFFF_u64), 'u16 to_and_fro u64');
    assert(cast_must_pass(0xFFFF_u16, 0xFFFF_u128), 'u16 to_and_fro u128');
    assert(cast_must_pass(0xFFFFFFFF_u32, 0xFFFFFFFF_u64), 'u32 to_and_fro u64');
    assert(cast_must_pass(0xFFFFFFFF_u32, 0xFFFFFFFF_u128), 'u32 to_and_fro u128');
    assert(cast_must_pass(0xFFFFFFFFFFFFFFFF_u64, 0xFFFFFFFFFFFFFFFF_u128), 'u64 to_and_fro u128');
}

#[test]
fn test_into_self_type() {
    assert_eq(@0xFF_u8.into(), @0xFF_u8, 'u8 into u8');
    assert_eq(@0xFFFF_u16.into(), @0xFFFF_u16, 'u16 into u16');
    assert_eq(@0xFFFFFFFF_u32.into(), @0xFFFFFFFF_u32, 'u32 into u32');
    assert_eq(@0xFFFFFFFFFFFFFFFF_u64.into(), @0xFFFFFFFFFFFFFFFF_u64, 'u64 into u64');
    assert_eq(
        @0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF_u128.into(),
        @0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF_u128,
        'u128 into u128'
    );
    assert_eq(
        @u256 { low: 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF, high: 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF }
            .into(),
        @u256 { high: 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF, low: 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF },
        'u256 into u256'
    );
}

#[test]
#[should_panic]
fn panic_u16_u8_1() {
    let out: u8 = (0xFF_u16 + 1_u16).try_into().unwrap();
}

#[test]
#[should_panic]
fn panic_u16_u8_2() {
    let max_u16: u16 = 0xFFFF;
    let out: u8 = max_u16.try_into().unwrap();
}
#[test]
#[should_panic]
fn panic_u32_u8_1() {
    let out: u8 = (0xFF_u32 + 1_u32).try_into().unwrap();
}

#[test]
#[should_panic]
fn panic_u32_u8_2() {
    let max_u32: u32 = 0xFFFFFFFF;
    let out: u8 = max_u32.try_into().unwrap();
}
#[test]
#[should_panic]
fn panic_u64_u8_1() {
    let out: u8 = (0xFF_u64 + 1_u64).try_into().unwrap();
}

#[test]
#[should_panic]
fn panic_u64_u8_2() {
    let max_u64: u64 = 0xFFFFFFFFFFFFFFFF;
    let out: u8 = max_u64.try_into().unwrap();
}
#[test]
#[should_panic]
fn panic_u128_u8_1() {
    let out: u8 = (0xFF_u128 + 1_u128).try_into().unwrap();
}

#[test]
#[should_panic]
fn panic_u128_u8_2() {
    let max_u128: u128 = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF;
    let out: u8 = max_u128.try_into().unwrap();
}
#[test]
#[should_panic]
fn panic_u32_u16_1() {
    let out: u16 = (0xFFFF_u32 + 1_u32).try_into().unwrap();
}

#[test]
#[should_panic]
fn panic_u32_u16_2() {
    let max_u32: u32 = 0xFFFFFFFF;
    let out: u16 = max_u32.try_into().unwrap();
}
#[test]
#[should_panic]
fn panic_u64_u16_1() {
    let out: u16 = (0xFFFF_u64 + 1_u64).try_into().unwrap();
}

#[test]
#[should_panic]
fn panic_u64_u16_2() {
    let max_u64: u64 = 0xFFFFFFFFFFFFFFFF;
    let out: u16 = max_u64.try_into().unwrap();
}
#[test]
#[should_panic]
fn panic_u128_u16_1() {
    let out: u16 = (0xFFFF_u128 + 1_u128).try_into().unwrap();
}

#[test]
#[should_panic]
fn panic_u128_u16_2() {
    let max_u128: u128 = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF;
    let out: u16 = max_u128.try_into().unwrap();
}
#[test]
#[should_panic]
fn panic_u64_u32_1() {
    let out: u32 = (0xFFFFFFFF_u64 + 1_u64).try_into().unwrap();
}

#[test]
#[should_panic]
fn panic_u64_u32_2() {
    let max_u64: u64 = 0xFFFFFFFFFFFFFFFF;
    let out: u32 = max_u64.try_into().unwrap();
}

#[test]
#[should_panic]
fn panic_u128_u32_1() {
    let out: u32 = (0xFFFFFFFF_u128 + 1_u128).try_into().unwrap();
}

#[test]
#[should_panic]
fn panic_u128_u32_2() {
    let max_u128: u128 = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF;
    let out: u32 = max_u128.try_into().unwrap();
}

#[test]
#[should_panic]
fn panic_u128_u64_1() {
    let out: u64 = (0xFFFFFFFFFFFFFFFF_u128 + 1_u128).try_into().unwrap();
}

#[test]
#[should_panic]
fn panic_u128_u64_2() {
    let max_u128: u128 = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF;
    let out: u64 = max_u128.try_into().unwrap();
}

#[test]
fn test_u128_byte_reverse() {
    assert_eq(
        @integer::u128_byte_reverse(0x000102030405060708090a0b0c0d0e0f),
        @0x0f0e0d0c0b0a09080706050403020100,
        'Wrong byte reverse'
    );
}

#[test]
fn test_i8_operators() {
    assert_eq(@1_i8, @1_i8, '1 == 1');
    assert_ne(@1_i8, @2_i8, '1 != 2');
    assert_eq(@0x7f_felt252.try_into().unwrap(), @0x7f_i8, '0x7f is not i8');
    let v: Option<i8> = 0x80_felt252.try_into();
    assert(v.is_none(), '0x80 is i8');
    assert_eq(@(-0x80_felt252).try_into().unwrap(), @-0x80_i8, '-0x80 is not i8');
    let v: Option<i8> = (-0x81_felt252).try_into();
    assert(v.is_none(), '-0x81 is i8');
    assert_eq(@(1_i8 + 3_i8), @4_i8, '1 + 3 == 4');
    assert_eq(@(3_i8 + 6_i8), @9_i8, '3 + 6 == 9');
    assert_eq(@(3_i8 - 1_i8), @2_i8, '3 - 1 == 2');
    assert_eq(@(121_i8 - 21_i8), @100_i8, '121-21=100');
    assert_eq(@(-1_i8 + -3_i8), @-4_i8, '-1 + -3 == -4');
    assert_eq(@(-3_i8 + -6_i8), @-9_i8, '-3 + -6 == -9');
    assert_eq(@(-3_i8 - -1_i8), @-2_i8, '-3 - -1 == -2');
    assert_eq(@(-121_i8 - -21_i8), @-100_i8, '-121--21=-100');
    assert_eq(@(1_i8 * 3_i8), @3_i8, '1 * 3 == 3');
    assert_eq(@(2_i8 * 4_i8), @8_i8, '2 * 4 == 8');
    assert_eq(@(-1_i8 * 3_i8), @-3_i8, '-1 * 3 == 3');
    assert_eq(@(-2_i8 * 4_i8), @-8_i8, '-2 * 4 == 8');
    assert_eq(@(1_i8 * -3_i8), @-3_i8, '1 * -3 == -3');
    assert_eq(@(2_i8 * -4_i8), @-8_i8, '2 * -4 == -8');
    assert_eq(@(-1_i8 * -3_i8), @3_i8, '-1 * -3 == 3');
    assert_eq(@(-2_i8 * -4_i8), @8_i8, '-2 * -4 == 8');
    assert_lt(1_i8, 4_i8, '1 < 4');
    assert_le(1_i8, 4_i8, '1 <= 4');
    assert(!(4_i8 < 4_i8), '!(4 < 4)');
    assert_le(5_i8, 5_i8, '5 <= 5');
    assert(!(5_i8 <= 4_i8), '!(5 <= 8)');
    assert_gt(5_i8, 2_i8, '5 > 2');
    assert_ge(5_i8, 2_i8, '5 >= 2');
    assert(!(3_i8 > 3_i8), '!(3 > 3)');
    assert_ge(3_i8, 3_i8, '3 >= 3');
}

#[test]
#[should_panic(expected: ('i8_sub Underflow',))]
fn test_i8_sub_underflow_1() {
    -0x80_i8 - 1_i8;
}

#[test]
#[should_panic(expected: ('i8_sub Underflow',))]
fn test_i8_sub_underflow_2() {
    -0x80_i8 - 3_i8;
}

#[test]
#[should_panic(expected: ('i8_sub Underflow',))]
fn test_i8_sub_underflow_3() {
    -0x7f_i8 - 3_i8;
}

#[test]
#[should_panic(expected: ('i8_sub Underflow',))]
fn test_i8_sub_underflow_4() {
    -0x32_i8 - 0x7d_i8;
}

#[test]
#[should_panic(expected: ('i8_sub Overflow',))]
fn test_i8_sub_overflow() {
    0x32_i8 - -0x7d_i8;
}

#[test]
#[should_panic(expected: ('i8_add Overflow',))]
fn test_i8_add_overflow_1() {
    0x40_i8 + 0x40_i8;
}

#[test]
#[should_panic(expected: ('i8_add Overflow',))]
fn test_i8_add_overflow_2() {
    0x64_i8 + 0x1e_i8;
}

#[test]
#[should_panic(expected: ('i8_add Underflow',))]
fn test_i8_add_underflow() {
    -0x64_i8 + -0x1e_i8;
}

#[test]
#[should_panic]
fn test_i8_mul_overflow_1() {
    0x10_i8 * 0x10_i8;
}

#[test]
#[should_panic]
fn test_i8_mul_overflow_2() {
    0x11_i8 * 0x10_i8;
}

#[test]
#[should_panic]
fn test_i8_mul_overflow_3() {
    2_i8 * 0x40_i8;
}

#[test]
fn test_i16_operators() {
    assert_eq(@1_i16, @1_i16, '1 == 1');
    assert_ne(@1_i16, @2_i16, '1 != 2');
    assert_eq(@0x7fff_felt252.try_into().unwrap(), @0x7fff_i16, '0x7fff is not i16');
    let v: Option<i16> = 0x8000_felt252.try_into();
    assert(v.is_none(), '0x8000 is i16');
    assert_eq(@(-0x8000_felt252).try_into().unwrap(), @-0x8000_i16, '-0x8000 is not i16');
    let v: Option<i16> = (-0x8001_felt252).try_into();
    assert(v.is_none(), '-0x8001 is i16');
    assert_eq(@(1_i16 + 3_i16), @4_i16, '1 + 3 == 4');
    assert_eq(@(3_i16 + 6_i16), @9_i16, '3 + 6 == 9');
    assert_eq(@(3_i16 - 1_i16), @2_i16, '3 - 1 == 2');
    assert_eq(@(231_i16 - 131_i16), @100_i16, '231-131=100');
    assert_eq(@(-1_i16 + -3_i16), @-4_i16, '-1 + -3 == -4');
    assert_eq(@(-3_i16 + -6_i16), @-9_i16, '-3 + -6 == -9');
    assert_eq(@(-3_i16 - -1_i16), @-2_i16, '-3 - -1 == -2');
    assert_eq(@(-231_i16 - -131_i16), @-100_i16, '-231--131=-100');
    assert_eq(@(1_i16 * 3_i16), @3_i16, '1 * 3 == 3');
    assert_eq(@(2_i16 * 4_i16), @8_i16, '2 * 4 == 8');
    assert_eq(@(-1_i16 * 3_i16), @-3_i16, '-1 * 3 == 3');
    assert_eq(@(-2_i16 * 4_i16), @-8_i16, '-2 * 4 == 8');
    assert_eq(@(1_i16 * -3_i16), @-3_i16, '1 * -3 == -3');
    assert_eq(@(2_i16 * -4_i16), @-8_i16, '2 * -4 == -8');
    assert_eq(@(-1_i16 * -3_i16), @3_i16, '-1 * -3 == 3');
    assert_eq(@(-2_i16 * -4_i16), @8_i16, '-2 * -4 == 8');
    assert_lt(1_i16, 4_i16, '1 < 4');
    assert_le(1_i16, 4_i16, '1 <= 4');
    assert(!(4_i16 < 4_i16), '!(4 < 4)');
    assert_le(5_i16, 5_i16, '5 <= 5');
    assert(!(5_i16 <= 4_i16), '!(5 <= 8)');
    assert_gt(5_i16, 2_i16, '5 > 2');
    assert_ge(5_i16, 2_i16, '5 >= 2');
    assert(!(3_i16 > 3_i16), '!(3 > 3)');
    assert_ge(3_i16, 3_i16, '3 >= 3');
}

#[test]
#[should_panic(expected: ('i16_sub Underflow',))]
fn test_i16_sub_underflow_1() {
    -0x8000_i16 - 1_i16;
}

#[test]
#[should_panic(expected: ('i16_sub Underflow',))]
fn test_i16_sub_underflow_2() {
    -0x8000_i16 - 3_i16;
}

#[test]
#[should_panic(expected: ('i16_sub Underflow',))]
fn test_i16_sub_underflow_3() {
    -0x7fff_i16 - 3_i16;
}

#[test]
#[should_panic(expected: ('i16_sub Underflow',))]
fn test_i16_sub_underflow_4() {
    -0x3200_i16 - 0x7d00_i16;
}

#[test]
#[should_panic(expected: ('i16_sub Overflow',))]
fn test_i16_sub_overflow() {
    0x3200_i16 - -0x7d00_i16;
}

#[test]
#[should_panic(expected: ('i16_add Overflow',))]
fn test_i16_add_overflow_1() {
    0x4000_i16 + 0x4000_i16;
}

#[test]
#[should_panic(expected: ('i16_add Overflow',))]
fn test_i16_add_overflow_2() {
    0x6400_i16 + 0x1e00_i16;
}

#[test]
#[should_panic(expected: ('i16_add Underflow',))]
fn test_i16_add_underflow() {
    -0x6400_i16 + -0x1e00_i16;
}

#[test]
#[should_panic]
fn test_i16_mul_overflow_1() {
    0x1000_i16 * 0x1000_i16;
}

#[test]
#[should_panic]
fn test_i16_mul_overflow_2() {
    0x1100_i16 * 0x1000_i16;
}

#[test]
#[should_panic]
fn test_i16_mul_overflow_3() {
    2_i16 * 0x4000_i16;
}

#[test]
fn test_i32_operators() {
    assert_eq(@1_i32, @1_i32, '1 == 1');
    assert_ne(@1_i32, @2_i32, '1 != 2');
    assert_eq(@0x7fffffff_felt252.try_into().unwrap(), @0x7fffffff_i32, '0x7fffffff is not i32');
    let v: Option<i32> = 0x80000000_felt252.try_into();
    assert(v.is_none(), '0x80000000 is i32');
    assert_eq(@(-0x80000000_felt252).try_into().unwrap(), @-0x80000000_i32, '-0x8000 is not i32');
    let v: Option<i32> = (-0x80000001_felt252).try_into();
    assert(v.is_none(), '-0x80000001 is i32');
    assert_eq(@(1_i32 + 3_i32), @4_i32, '1 + 3 == 4');
    assert_eq(@(3_i32 + 6_i32), @9_i32, '3 + 6 == 9');
    assert_eq(@(3_i32 - 1_i32), @2_i32, '3 - 1 == 2');
    assert_eq(@(231_i32 - 131_i32), @100_i32, '231-131=100');
    assert_eq(@(-1_i32 + -3_i32), @-4_i32, '-1 + -3 == -4');
    assert_eq(@(-3_i32 + -6_i32), @-9_i32, '-3 + -6 == -9');
    assert_eq(@(-3_i32 - -1_i32), @-2_i32, '-3 - -1 == -2');
    assert_eq(@(-231_i32 - -131_i32), @-100_i32, '-231--131=-100');
    assert_eq(@(1_i32 * 3_i32), @3_i32, '1 * 3 == 3');
    assert_eq(@(2_i32 * 4_i32), @8_i32, '2 * 4 == 8');
    assert_eq(@(-1_i32 * 3_i32), @-3_i32, '-1 * 3 == 3');
    assert_eq(@(-2_i32 * 4_i32), @-8_i32, '-2 * 4 == 8');
    assert_eq(@(1_i32 * -3_i32), @-3_i32, '1 * -3 == -3');
    assert_eq(@(2_i32 * -4_i32), @-8_i32, '2 * -4 == -8');
    assert_eq(@(-1_i32 * -3_i32), @3_i32, '-1 * -3 == 3');
    assert_eq(@(-2_i32 * -4_i32), @8_i32, '-2 * -4 == 8');
    assert_lt(1_i32, 4_i32, '1 < 4');
    assert_le(1_i32, 4_i32, '1 <= 4');
    assert(!(4_i32 < 4_i32), '!(4 < 4)');
    assert_le(5_i32, 5_i32, '5 <= 5');
    assert(!(5_i32 <= 4_i32), '!(5 <= 8)');
    assert_gt(5_i32, 2_i32, '5 > 2');
    assert_ge(5_i32, 2_i32, '5 >= 2');
    assert(!(3_i32 > 3_i32), '!(3 > 3)');
    assert_ge(3_i32, 3_i32, '3 >= 3');
}

#[test]
#[should_panic(expected: ('i32_sub Underflow',))]
fn test_i32_sub_underflow_1() {
    -0x80000000_i32 - 1_i32;
}

#[test]
#[should_panic(expected: ('i32_sub Underflow',))]
fn test_i32_sub_underflow_2() {
    -0x80000000_i32 - 3_i32;
}

#[test]
#[should_panic(expected: ('i32_sub Underflow',))]
fn test_i32_sub_underflow_3() {
    -0x7fffffff_i32 - 3_i32;
}

#[test]
#[should_panic(expected: ('i32_sub Underflow',))]
fn test_i32_sub_underflow_4() {
    -0x32000000_i32 - 0x7d000000_i32;
}

#[test]
#[should_panic(expected: ('i32_sub Overflow',))]
fn test_i32_sub_overflow() {
    0x32000000_i32 - -0x7d000000_i32;
}

#[test]
#[should_panic(expected: ('i32_add Overflow',))]
fn test_i32_add_overflow_1() {
    0x40000000_i32 + 0x40000000_i32;
}

#[test]
#[should_panic(expected: ('i32_add Overflow',))]
fn test_i32_add_overflow_2() {
    0x64000000_i32 + 0x1e000000_i32;
}

#[test]
#[should_panic(expected: ('i32_add Underflow',))]
fn test_i32_add_underflow() {
    -0x64000000_i32 + -0x1e000000_i32;
}

#[test]
#[should_panic]
fn test_i32_mul_overflow_1() {
    0x10000000_i32 * 0x10000000_i32;
}

#[test]
#[should_panic]
fn test_i32_mul_overflow_2() {
    0x11000000_i32 * 0x10000000_i32;
}

#[test]
#[should_panic]
fn test_i32_mul_overflow_3() {
    2_i32 * 0x40000000_i32;
}

#[test]
fn test_i64_operators() {
    assert_eq(@1_i64, @1_i64, '1 == 1');
    assert_ne(@1_i64, @2_i64, '1 != 2');
    assert_eq(
        @0x7fffffffffffffff_felt252.try_into().unwrap(),
        @0x7fffffffffffffff_i64,
        '0x7fffffffffffffff is not i64'
    );
    let v: Option<i64> = 0x8000000000000000_felt252.try_into();
    assert(v.is_none(), '0x8000000000000000 is i64');
    assert_eq(
        @(-0x8000000000000000_felt252).try_into().unwrap(),
        @-0x8000000000000000_i64,
        '-0x8000000000000000 is not i64'
    );
    let v: Option<i64> = (-0x8000000000000001_felt252).try_into();
    assert(v.is_none(), '-0x8000000000000001 is i64');
    assert_eq(@(1_i64 + 3_i64), @4_i64, '1 + 3 == 4');
    assert_eq(@(3_i64 + 6_i64), @9_i64, '3 + 6 == 9');
    assert_eq(@(3_i64 - 1_i64), @2_i64, '3 - 1 == 2');
    assert_eq(@(231_i64 - 131_i64), @100_i64, '231-131=100');
    assert_eq(@(-1_i64 + -3_i64), @-4_i64, '-1 + -3 == -4');
    assert_eq(@(-3_i64 + -6_i64), @-9_i64, '-3 + -6 == -9');
    assert_eq(@(-3_i64 - -1_i64), @-2_i64, '-3 - -1 == -2');
    assert_eq(@(-231_i64 - -131_i64), @-100_i64, '-231--131=-100');
    assert_eq(@(1_i64 * 3_i64), @3_i64, '1 * 3 == 3');
    assert_eq(@(2_i64 * 4_i64), @8_i64, '2 * 4 == 8');
    assert_eq(@(-1_i64 * 3_i64), @-3_i64, '-1 * 3 == 3');
    assert_eq(@(-2_i64 * 4_i64), @-8_i64, '-2 * 4 == 8');
    assert_eq(@(1_i64 * -3_i64), @-3_i64, '1 * -3 == -3');
    assert_eq(@(2_i64 * -4_i64), @-8_i64, '2 * -4 == -8');
    assert_eq(@(-1_i64 * -3_i64), @3_i64, '-1 * -3 == 3');
    assert_eq(@(-2_i64 * -4_i64), @8_i64, '-2 * -4 == 8');
    assert_lt(1_i64, 4_i64, '1 < 4');
    assert_le(1_i64, 4_i64, '1 <= 4');
    assert(!(4_i64 < 4_i64), '!(4 < 4)');
    assert_le(5_i64, 5_i64, '5 <= 5');
    assert(!(5_i64 <= 4_i64), '!(5 <= 8)');
    assert_gt(5_i64, 2_i64, '5 > 2');
    assert_ge(5_i64, 2_i64, '5 >= 2');
    assert(!(3_i64 > 3_i64), '!(3 > 3)');
    assert_ge(3_i64, 3_i64, '3 >= 3');
}

#[test]
#[should_panic(expected: ('i64_sub Underflow',))]
fn test_i64_sub_underflow_1() {
    -0x8000000000000000_i64 - 1_i64;
}

#[test]
#[should_panic(expected: ('i64_sub Underflow',))]
fn test_i64_sub_underflow_2() {
    -0x8000000000000000_i64 - 3_i64;
}

#[test]
#[should_panic(expected: ('i64_sub Underflow',))]
fn test_i64_sub_underflow_3() {
    -0x7fffffffffffffff_i64 - 3_i64;
}

#[test]
#[should_panic(expected: ('i64_sub Underflow',))]
fn test_i64_sub_underflow_4() {
    -0x3200000000000000_i64 - 0x7d00000000000000_i64;
}

#[test]
#[should_panic(expected: ('i64_sub Overflow',))]
fn test_i64_sub_overflow() {
    0x3200000000000000_i64 - -0x7d00000000000000_i64;
}

#[test]
#[should_panic(expected: ('i64_add Overflow',))]
fn test_i64_add_overflow_1() {
    0x4000000000000000_i64 + 0x4000000000000000_i64;
}

#[test]
#[should_panic(expected: ('i64_add Overflow',))]
fn test_i64_add_overflow_2() {
    0x6400000000000000_i64 + 0x1e00000000000000_i64;
}

#[test]
#[should_panic(expected: ('i64_add Underflow',))]
fn test_i64_add_underflow() {
    -0x6400000000000000_i64 + -0x1e00000000000000_i64;
}

#[test]
#[should_panic]
fn test_i64_mul_overflow_1() {
    0x1000000000000000_i64 * 0x1000000000000000_i64;
}

#[test]
#[should_panic]
fn test_i64_mul_overflow_2() {
    0x1100000000000000_i64 * 0x1000000000000000_i64;
}

#[test]
#[should_panic]
fn test_i64_mul_overflow_3() {
    2_i64 * 0x4000000000000000_i64;
}

#[test]
fn test_i128_operators() {
    assert_eq(@1_i128, @1_i128, '1 == 1');
    assert_ne(@1_i128, @2_i128, '1 != 2');
    assert_eq(
        @0x7fffffffffffffffffffffffffffffff_felt252.try_into().unwrap(),
        @0x7fffffffffffffffffffffffffffffff_i128,
        '0x7f..f is not i128'
    );
    let v: Option<i128> = 0x80000000000000000000000000000000_felt252.try_into();
    assert(v.is_none(), '0x80..0 is i128');
    assert_eq(
        @(-0x80000000000000000000000000000000_felt252).try_into().unwrap(),
        @-0x80000000000000000000000000000000_i128,
        '-0x80..0 is not i128'
    );
    let v: Option<i128> = (-0x80000000000000000000000000000001_felt252).try_into();
    assert(v.is_none(), '-0x80..01 is i128');
    assert_eq(@(1_i128 + 3_i128), @4_i128, '1 + 3 == 4');
    assert_eq(@(3_i128 + 6_i128), @9_i128, '3 + 6 == 9');
    assert_eq(@(3_i128 - 1_i128), @2_i128, '3 - 1 == 2');
    assert_eq(@(231_i128 - 131_i128), @100_i128, '231-131=100');
    assert_eq(@(-1_i128 + -3_i128), @-4_i128, '-1 + -3 == -4');
    assert_eq(@(-3_i128 + -6_i128), @-9_i128, '-3 + -6 == -9');
    assert_eq(@(-3_i128 - -1_i128), @-2_i128, '-3 - -1 == -2');
    assert_eq(@(-231_i128 - -131_i128), @-100_i128, '-231--131=-100');
    assert_eq(@(1_i128 * 3_i128), @3_i128, '1 * 3 == 3');
    assert_eq(@(7_i128 * 0_i128), @0_i128, '7 * 0 == 0');
    assert_eq(@(2_i128 * 4_i128), @8_i128, '2 * 4 == 8');
    assert_eq(@(-1_i128 * 3_i128), @-3_i128, '-1 * 3 == -3');
    assert_eq(@(-2_i128 * 4_i128), @-8_i128, '-2 * 4 == -8');
    assert_eq(@(1_i128 * -3_i128), @-3_i128, '1 * -3 == -3');
    assert_eq(@(2_i128 * -4_i128), @-8_i128, '2 * -4 == -8');
    assert_eq(@(-1_i128 * -3_i128), @3_i128, '-1 * -3 == 3');
    assert_eq(@(-2_i128 * -4_i128), @8_i128, '-2 * -4 == 8');
    assert_eq(
        @(0x800000000000000_i128 * -0x100000000000000000_i128),
        @-0x80000000000000000000000000000000_i128,
        'failed MIN_I128 as mul result'
    );
    assert_lt(1_i128, 4_i128, '1 < 4');
    assert_le(1_i128, 4_i128, '1 <= 4');
    assert(!(4_i128 < 4_i128), '!(4 < 4)');
    assert_le(5_i128, 5_i128, '5 <= 5');
    assert(!(5_i128 <= 4_i128), '!(5 <= 8)');
    assert_gt(5_i128, 2_i128, '5 > 2');
    assert_ge(5_i128, 2_i128, '5 >= 2');
    assert(!(3_i128 > 3_i128), '!(3 > 3)');
    assert_ge(3_i128, 3_i128, '3 >= 3');
}

#[test]
#[should_panic]
fn test_i128_sub_underflow_1() {
    -0x80000000000000000000000000000000_i128 - 1_i128;
}

#[test]
#[should_panic(expected: ('i128_sub Underflow',))]
fn test_i128_sub_underflow_2() {
    -0x80000000000000000000000000000000_i128 - 3_i128;
}

#[test]
#[should_panic(expected: ('i128_sub Underflow',))]
fn test_i128_sub_underflow_3() {
    -0x7fffffffffffffffffffffffffffffff_i128 - 3_i128;
}

#[test]
#[should_panic(expected: ('i128_sub Underflow',))]
fn test_i128_sub_underflow_4() {
    -0x32000000000000000000000000000000_i128 - 0x7d000000000000000000000000000000_i128;
}

#[test]
#[should_panic(expected: ('i128_sub Overflow',))]
fn test_i128_sub_overflow() {
    0x32000000000000000000000000000000_i128 - -0x7d000000000000000000000000000000_i128;
}

#[test]
#[should_panic(expected: ('i128_add Overflow',))]
fn test_i128_add_overflow_1() {
    0x40000000000000000000000000000000_i128 + 0x40000000000000000000000000000000_i128;
}

#[test]
#[should_panic(expected: ('i128_add Overflow',))]
fn test_i128_add_overflow_2() {
    0x64000000000000000000000000000000_i128 + 0x1e000000000000000000000000000000_i128;
}

#[test]
#[should_panic(expected: ('i128_add Underflow',))]
fn test_i128_add_underflow() {
    -0x64000000000000000000000000000000_i128 + -0x1e000000000000000000000000000000_i128;
}

#[test]
#[should_panic]
fn test_i128_mul_overflow_1() {
    0x10000000000000000000000000000000_i128 * 0x10000000000000000000000000000000_i128;
}

#[test]
#[should_panic]
fn test_i128_mul_overflow_2() {
    0x11000000000000000000000000000000_i128 * 0x10000000000000000000000000000000_i128;
}

#[test]
#[should_panic]
fn test_i128_mul_overflow_3() {
    2_i128 * 0x40000000000000000000000000000000_i128;
}

#[test]
fn test_signed_int_diff() {
    assert_eq(@integer::i8_diff(3, 3).unwrap(), @0, 'i8: 3 - 3 == 0');
    assert_eq(@integer::i8_diff(4, 3).unwrap(), @1, 'i8: 4 - 3 == 1');
    assert_eq(@integer::i8_diff(3, 5).unwrap_err(), @~(2 - 1), 'i8: 3 - 5 == -2');
    assert_eq(@integer::i16_diff(3, 3).unwrap(), @0, 'i16: 3 - 3 == 0');
    assert_eq(@integer::i16_diff(4, 3).unwrap(), @1, 'i16: 4 - 3 == 1');
    assert_eq(@integer::i16_diff(3, 5).unwrap_err(), @~(2 - 1), 'i16: 3 - 5 == -2');
    assert_eq(@integer::i32_diff(3, 3).unwrap(), @0, 'i32: 3 - 3 == 0');
    assert_eq(@integer::i32_diff(4, 3).unwrap(), @1, 'i32: 4 - 3 == 1');
    assert_eq(@integer::i32_diff(3, 5).unwrap_err(), @~(2 - 1), 'i32: 3 - 5 == -2');
    assert_eq(@integer::i64_diff(3, 3).unwrap(), @0, 'i64: 3 - 3 == 0');
    assert_eq(@integer::i64_diff(4, 3).unwrap(), @1, 'i64: 4 - 3 == 1');
    assert_eq(@integer::i64_diff(3, 5).unwrap_err(), @~(2 - 1), 'i64: 3 - 5 == -2');
    assert_eq(@integer::i128_diff(3, 3).unwrap(), @0, 'i128: 3 - 3 == 0');
    assert_eq(@integer::i128_diff(4, 3).unwrap(), @1, 'i128: 4 - 3 == 1');
    assert_eq(@integer::i128_diff(3, 5).unwrap_err(), @~(2 - 1), 'i128: 3 - 5 == -2');
}
