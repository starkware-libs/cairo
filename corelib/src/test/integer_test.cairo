use core::traits::Into;
use core::traits::Default;
use integer::BoundedInt;

#[test]
fn test_u8_operators() {
    assert(1_u8 == 1_u8, '1 == 1');
    assert(1_u8 != 2_u8, '1 != 2');
    assert(1_u8 + 3_u8 == 4_u8, '1 + 3 == 4');
    assert(3_u8 + 6_u8 == 9_u8, '3 + 6 == 9');
    assert(3_u8 - 1_u8 == 2_u8, '3 - 1 == 2');
    assert(1_u8 * 3_u8 == 3_u8, '1 * 3 == 3');
    assert(2_u8 * 4_u8 == 8_u8, '2 * 4 == 8');
    assert(19_u8 / 7_u8 == 2_u8, '19 / 7 == 2');
    assert(19_u8 % 7_u8 == 5_u8, '19 % 7 == 5');
    assert(231_u8 - 131_u8 == 100_u8, '231-131=100');
    assert(1_u8 < 4_u8, '1 < 4');
    assert(1_u8 <= 4_u8, '1 <= 4');
    assert(!(4_u8 < 4_u8), '!(4 < 4)');
    assert(4_u8 <= 4_u8, '4 <= 4');
    assert(5_u8 > 2_u8, '5 > 2');
    assert(5_u8 >= 2_u8, '5 >= 2');
    assert(!(3_u8 > 3_u8), '!(3 > 3)');
    assert(3_u8 >= 3_u8, '3 >= 3');
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
    assert(1_u16 == 1_u16, '1 == 1');
    assert(1_u16 != 2_u16, '1 != 2');
    assert(1_u16 + 3_u16 == 4_u16, '1 + 3 == 4');
    assert(3_u16 + 6_u16 == 9_u16, '3 + 6 == 9');
    assert(3_u16 - 1_u16 == 2_u16, '3 - 1 == 2');
    assert(231_u16 - 131_u16 == 100_u16, '231-131=100');
    assert(1_u16 * 3_u16 == 3_u16, '1 * 3 == 3');
    assert(2_u16 * 4_u16 == 8_u16, '2 * 4 == 8');
    assert(51725_u16 / 7_u16 == 7389_u16, '51725 / 7 == 7389');
    assert(51725_u16 % 7_u16 == 2_u16, '51725 % 7 == 2');
    assert(1_u16 < 4_u16, '1 < 4');
    assert(1_u16 <= 4_u16, '1 <= 4');
    assert(!(4_u16 < 4_u16), '!(4 < 4)');
    assert(4_u16 <= 4_u16, '4 <= 4');
    assert(5_u16 > 2_u16, '5 > 2');
    assert(5_u16 >= 2_u16, '5 >= 2');
    assert(!(3_u16 > 3_u16), '!(3 > 3)');
    assert(3_u16 >= 3_u16, '3 >= 3');
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
    assert(1_u32 == 1_u32, '1 == 1');
    assert(1_u32 != 2_u32, '1 != 2');
    assert(1_u32 + 3_u32 == 4_u32, '1 + 3 == 4');
    assert(3_u32 + 6_u32 == 9_u32, '3 + 6 == 9');
    assert(3_u32 - 1_u32 == 2_u32, '3 - 1 == 2');
    assert(231_u32 - 131_u32 == 100_u32, '231-131=100');
    assert(1_u32 * 3_u32 == 3_u32, '1 * 3 == 3');
    assert(2_u32 * 4_u32 == 8_u32, '2 * 4 == 8');
    assert(510670725_u32 / 7_u32 == 72952960_u32, '510670725 / 7 == 72952960');
    assert(510670725_u32 % 7_u32 == 5_u32, '510670725 % 7 == 5');
    assert(1_u32 < 4_u32, '1 < 4');
    assert(1_u32 <= 4_u32, '1 <= 4');
    assert(!(4_u32 < 4_u32), '!(4 < 4)');
    assert(4_u32 <= 4_u32, '4 <= 4');
    assert(5_u32 > 2_u32, '5 > 2');
    assert(5_u32 >= 2_u32, '5 >= 2');
    assert(!(3_u32 > 3_u32), '!(3 > 3)');
    assert(3_u32 >= 3_u32, '3 >= 3');
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
    assert(1_u64 == 1_u64, '1 == 1');
    assert(1_u64 != 2_u64, '1 != 2');
    assert(1_u64 + 3_u64 == 4_u64, '1 + 3 == 4');
    assert(3_u64 + 6_u64 == 9_u64, '3 + 6 == 9');
    assert(3_u64 - 1_u64 == 2_u64, '3 - 1 == 2');
    assert(231_u64 - 131_u64 == 100_u64, '231-131=100');
    assert(1_u64 * 3_u64 == 3_u64, '1 * 3 == 3');
    assert(2_u64 * 4_u64 == 8_u64, '2 * 4 == 8');
    assert(5010670477878974275_u64 / 7_u64 == 715810068268424896_u64, 'Wrong division result.');
    assert(5010670477878974275_u64 % 7_u64 == 3_u64, '5010670477878974275 % 7 == 3');
    assert(1_u64 < 4_u64, '1 < 4');
    assert(1_u64 <= 4_u64, '1 <= 4');
    assert(!(4_u64 < 4_u64), '!(4 < 4)');
    assert(4_u64 <= 4_u64, '4 <= 4');
    assert(5_u64 > 2_u64, '5 > 2');
    assert(5_u64 >= 2_u64, '5 >= 2');
    assert(!(3_u64 > 3_u64), '!(3 > 3)');
    assert(3_u64 >= 3_u64, '3 >= 3');
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
    assert(1_u128 == 1_u128, '1 == 1');
    assert(!(1_u128 == 2_u128), '!(1 == 2)');
    assert(1_u128 + 3_u128 == 4_u128, '1 + 3 == 4');
    assert(3_u128 + 6_u128 == 9_u128, '3 + 6 == 9');
    assert(3_u128 - 1_u128 == 2_u128, '3 - 1 == 2');
    assert(1231_u128 - 231_u128 == 1000_u128, '1231-231=1000');
    assert(1_u128 * 3_u128 == 3_u128, '1 * 3 == 3');
    assert(2_u128 * 4_u128 == 8_u128, '2 * 4 == 8');
    assert(8_u128 / 2_u128 == 4_u128, '8 / 2 == 4');
    assert(8_u128 % 2_u128 == 0_u128, '8 % 2 == 0');
    assert(7_u128 / 3_u128 == 2_u128, '7 / 3 == 2');
    assert(7_u128 % 3_u128 == 1_u128, '7 % 3 == 1');
    assert(1_u128 < 4_u128, '1 < 4');
    assert(1_u128 <= 4_u128, '1 <= 4');
    assert(!(4_u128 < 4_u128), '!(4 < 4)');
    assert(4_u128 <= 4_u128, '4 <= 4');
    assert(5_u128 > 2_u128, '5 > 2');
    assert(5_u128 >= 2_u128, '5 >= 2');
    assert(!(3_u128 > 3_u128), '!(3 > 3)');
    assert(3_u128 >= 3_u128, '3 >= 3');
    assert((1_u128 | 2_u128) == 3_u128, '1 | 2 == 3');
    assert((1_u128 & 2_u128) == 0_u128, '1 & 2 == 0');
    assert((1_u128 ^ 2_u128) == 3_u128, '1 ^ 2 == 3');
    assert((2_u128 | 2_u128) == 2_u128, '2 | 2 == 2');
    assert((2_u128 & 2_u128) == 2_u128, '2 & 2 == 2');
    assert((2_u128 & 3_u128) == 2_u128, '2 & 3 == 2');
    assert((3_u128 ^ 6_u128) == 5_u128, '3 ^ 6 == 5');
    assert(u128_sqrt(9_u128) == 3_u128, 'u128_sqrt(9) == 3');
    assert(u128_sqrt(10_u128) == 3_u128, 'u128_sqrt(10) == 3');
    assert(
        u128_sqrt(1267650600228229401496703205376_u128) == 1125899906842624_u128,
        'u128_sqrt(2^100) == 2^50'
    );
    assert(
        u128_sqrt(340282366920938463463374607431768211455_u128) == 18446744073709551615_u128,
        'Wrong square root result.'
    );
    assert(u128_sqrt(1_u128) == 1_u128, 'u128_sqrt(1) == 1');
    assert(u128_sqrt(0_u128) == 0_u128, 'u128_sqrt(0) == 0');
}

fn pow_2_127() -> u128 {
    0x80000000000000000000000000000000_u128
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
#[should_panic]
fn test_u128_add_overflow_1() {
    pow_2_127() + pow_2_127();
}

#[test]
#[should_panic]
fn test_u128_add_overflow_2() {
    (pow_2_127() + 12_u128) + pow_2_127();
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
    2_u128 * pow_2_127();
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

// TODO(orizi): Remove when u256 literals are supported.
fn as_u256(high: u128, low: u128) -> u256 {
    u256 { low, high }
}

#[test]
fn test_u256_from_felt252() {
    assert(1.into() == as_u256(0_u128, 1_u128), 'into 1');
    assert(
        (170141183460469231731687303715884105728 * 2).into() == as_u256(1_u128, 0_u128),
        'into 2**128'
    );
}

// TODO(orizi): Use u256 literals when supported.
#[test]
fn test_u256_operators() {
    let max_u128 = 0xffffffffffffffffffffffffffffffff;
    assert(as_u256(1, 1) + as_u256(3, 2) == as_u256(4, 3), 'no Overflow');
    assert(as_u256(1, pow_2_127()) + as_u256(3, pow_2_127()) == as_u256(5, 0), 'basic Overflow');
    assert(as_u256(4, 3) - as_u256(1, 1) == as_u256(3, 2), 'no UF');
    assert(as_u256(5, 0) - as_u256(1, pow_2_127()) == as_u256(3, pow_2_127()), 'basic UF');
    assert(as_u256(4, 3) * as_u256(0, 1) == as_u256(4, 3), 'mul by 1');
    assert(as_u256(4, 3) * as_u256(0, 2) == as_u256(8, 6), 'mul by 2');
    assert(as_u256(0, pow_2_127()) * as_u256(0, 2) == as_u256(1, 0), 'basic mul Overflow');
    assert(
        as_u256(0, max_u128)
            * as_u256(0, max_u128) == as_u256(0xfffffffffffffffffffffffffffffffe, 1),
        'max_u128 * max_u128'
    );
    assert(as_u256(0, max_u128) * as_u256(0, 1) == as_u256(0, max_u128), 'max_u128 * 1');
    assert(as_u256(0, 1) * as_u256(0, max_u128) == as_u256(0, max_u128), '1 * max_u128');
    assert((as_u256(1, 2) | as_u256(2, 2)) == as_u256(3, 2), '1.2|2.2==3.2');
    assert((as_u256(2, 1) | as_u256(2, 2)) == as_u256(2, 3), '2.1|2.2==2.3');
    assert((as_u256(2, 2) | as_u256(1, 2)) == as_u256(3, 2), '2.2|1.2==3.2');
    assert((as_u256(2, 2) | as_u256(2, 1)) == as_u256(2, 3), '2.2|2.1==2.3');
    assert((as_u256(1, 2) & as_u256(2, 2)) == as_u256(0, 2), '1.2&2.2==0.2');
    assert((as_u256(2, 1) & as_u256(2, 2)) == as_u256(2, 0), '2.1&2.2==2.0');
    assert((as_u256(2, 2) & as_u256(1, 2)) == as_u256(0, 2), '2.2&1.2==0.2');
    assert((as_u256(2, 2) & as_u256(2, 1)) == as_u256(2, 0), '2.2&2.1==2.0');
    assert((as_u256(1, 2) ^ as_u256(2, 2)) == as_u256(3, 0), '1.2^2.2==3.0');
    assert((as_u256(2, 1) ^ as_u256(2, 2)) == as_u256(0, 3), '2.1^2.2==0.3');
    assert((as_u256(2, 2) ^ as_u256(1, 2)) == as_u256(3, 0), '2.2^1.2==3.0');
    assert((as_u256(2, 2) ^ as_u256(2, 1)) == as_u256(0, 3), '2.2^2.1==0.3');
    assert(as_u256(1, 2) < as_u256(2, 2), '1.2<2.2');
    assert(as_u256(2, 1) < as_u256(2, 2), '2.1<2.2');
    assert(!(as_u256(2, 2) < as_u256(1, 2)), '2.2<1.2');
    assert(!(as_u256(2, 2) < as_u256(2, 1)), '2.2<2.1');
    assert(!(as_u256(2, 2) < as_u256(2, 2)), '2.2<2.2');
    assert(as_u256(1, 2) <= as_u256(2, 2), '1.2<=2.2');
    assert(as_u256(2, 1) <= as_u256(2, 2), '2.1<=2.2');
    assert(!(as_u256(2, 2) <= as_u256(1, 2)), '2.2<=1.2');
    assert(!(as_u256(2, 2) <= as_u256(2, 1)), '2.2<=2.1');
    assert(as_u256(2, 2) <= as_u256(2, 2), '2.2<=2.2');
    assert(!(as_u256(1, 2) > as_u256(2, 2)), '1.2>2.2');
    assert(!(as_u256(2, 1) > as_u256(2, 2)), '2.1>2.2');
    assert(as_u256(2, 2) > as_u256(1, 2), '2.2>1.2');
    assert(as_u256(2, 2) > as_u256(2, 1), '2.2>2.1');
    assert(!(as_u256(2, 2) > as_u256(2, 2)), '2.2>2.2');
    assert(!(as_u256(1, 2) >= as_u256(2, 2)), '1.2>=2.2');
    assert(!(as_u256(2, 1) >= as_u256(2, 2)), '2.1>=2.2');
    assert(as_u256(2, 2) >= as_u256(1, 2), '2.2>=1.2');
    assert(as_u256(2, 2) >= as_u256(2, 1), '2.2>=2.1');
    assert(as_u256(2, 2) >= as_u256(2, 2), '2.2>=2.2');

    assert(as_u256(3, 2) / as_u256(1, 1) == as_u256(0, 2), 'u256 div');
    assert(
        as_u256(4, 2) / as_u256(0, 3) == as_u256(1, 113427455640312821154458202477256070486),
        'u256 div'
    );
    assert(
        as_u256(0, 18446744073709551616) / as_u256(0, 18446744073709551616) == as_u256(0, 1),
        'u256 div'
    );
}

#[test]
#[should_panic]
fn test_u256_add_overflow() {
    as_u256(pow_2_127(), 1_u128) + as_u256(pow_2_127(), 1_u128);
}

#[test]
#[should_panic]
fn test_u256_sub_overflow() {
    as_u256(1_u128, 1_u128) - as_u256(1_u128, 2_u128);
}

#[test]
#[should_panic]
fn test_u256_mul_overflow_1() {
    as_u256(1_u128, 1_u128) * as_u256(1_u128, 2_u128);
}

#[test]
#[should_panic]
fn test_u256_mul_overflow_2() {
    as_u256(0_u128, pow_2_127()) * as_u256(2_u128, 0_u128);
}

#[test]
fn test_min() {
    let min_u8: u8 = BoundedInt::min();
    let min_u16: u16 = BoundedInt::min();
    let min_u32: u32 = BoundedInt::min();
    let min_u64: u64 = BoundedInt::min();
    let min_u128: u128 = BoundedInt::min();
    let min_u256: u256 = BoundedInt::min();
    assert(min_u8 == 0_u8, 'not zero');
    assert(min_u16 == 0_u16, 'not zero');
    assert(min_u32 == 0_u32, 'not zero');
    assert(min_u64 == 0_u64, 'not zero');
    assert(min_u128 == 0_u128, 'not zero');
    assert(min_u256 == as_u256(0_u128, 0_u128), 'not zero');
}

#[test]
fn test_max() {
    let max_u8: u8 = BoundedInt::max();
    let max_u16: u16 = BoundedInt::max();
    let max_u32: u32 = BoundedInt::max();
    let max_u64: u64 = BoundedInt::max();
    let max_u128: u128 = BoundedInt::max();
    let max_u256: u256 = BoundedInt::max();
    assert(max_u8 == 0xff_u8, 'not max');
    assert(max_u16 == 0xffff_u16, 'not max');
    assert(max_u32 == 0xffffffff_u32, 'not max');
    assert(max_u64 == 0xffffffffffffffff_u64, 'not max');
    assert(max_u128 == 0xffffffffffffffffffffffffffffffff_u128, 'not max');
    assert(max_u256 == as_u256(max_u128, max_u128), 'not max');
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
    BoundedInt::max() + 1.into();
}


#[test]
fn test_default_values() {
    assert(Default::default() == 0, '0 == 0');
    assert(Default::default() == 0_u8, '0 == 0');
    assert(Default::default() == 0_u16, '0 == 0');
    assert(Default::default() == 0_u32, '0 == 0');
    assert(Default::default() == 0_u64, '0 == 0');
    assert(Default::default() == 0_u128, '0 == 0');
    assert(Default::default() == u256 { low: 0_u128, high: 0_u128 }, '0 == 0');
}

#[test]
fn test_default_felt252dict_values() {
    assert(Felt252DictValue::zero_default() == 0, '0 == 0');
    assert(Felt252DictValue::zero_default() == 0_u8, '0 == 0');
    assert(Felt252DictValue::zero_default() == 0_u16, '0 == 0');
    assert(Felt252DictValue::zero_default() == 0_u32, '0 == 0');
    assert(Felt252DictValue::zero_default() == 0_u64, '0 == 0');
    assert(Felt252DictValue::zero_default() == 0_u128, '0 == 0');
}
