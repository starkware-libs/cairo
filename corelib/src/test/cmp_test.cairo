use cmp::min;
use cmp::max;

// Integer tests

#[test]
fn test_min_u8() {
    assert(min(0_u8, 1_u8) == 0_u8, '0 < 1');
    assert(min(0_u8, 1_u8) == 0_u8, '0 < 1');
    assert(min(5_u8, 7_u8) == 5_u8, '5 < 7');
    assert(min(255_u8, 128_u8) == 128_u8, '128 < 255');
    assert(min(10_u8, 10_u8) == 10_u8, '10 == 10');
    assert(min(0_u8, 0_u8) == 0_u8, '0 == 0');
    assert(min(255_u8, 255_u8) == 255_u8, '255 == 255');
    assert(min(3_u8, 4_u8) == 3_u8, '3 < 4');
    assert(min(255_u8, 200_u8) == 200_u8, '200 < 255');
    assert(min(250_u8, 253_u8) == 250_u8, '250 < 253');
}

#[test]
fn test_max_u8() {
    assert(max(0_u8, 1_u8) == 1_u8, '1 > 0');
    assert(max(5_u8, 7_u8) == 7_u8, '7 > 5');
    assert(max(255_u8, 128_u8) == 255_u8, '255 > 128');
    assert(max(10_u8, 10_u8) == 10_u8, '10 == 10');
    assert(max(0_u8, 0_u8) == 0_u8, '0 == 0');
    assert(max(255_u8, 255_u8) == 255_u8, '255 == 255');
    assert(max(100_u8, 200_u8) == 200_u8, '200 > 100');
    assert(max(1_u8, 2_u8) == 2_u8, '2 > 1');
    assert(max(120_u8, 130_u8) == 130_u8, '130 > 120');
    assert(max(200_u8, 150_u8) == 200_u8, '200 > 150');
}

#[test]
fn test_min_u16() {
    assert(min(0_u16, 1_u16) == 0_u16, '0 < 1');
    assert(min(5_u16, 7_u16) == 5_u16, '5 < 7');
    assert(min(65535_u16, 32768_u16) == 32768_u16, '32768 < 65535');
    assert(min(10_u16, 10_u16) == 10_u16, '10 == 10');
    assert(min(0_u16, 0_u16) == 0_u16, '0 == 0');
    assert(min(65535_u16, 65535_u16) == 65535_u16, '65535 == 65535');
    assert(min(100_u16, 200_u16) == 100_u16, '100 < 200');
    assert(min(1_u16, 2_u16) == 1_u16, '1 < 2');
    assert(min(32767_u16, 32766_u16) == 32766_u16, '32766 < 32767');
    assert(min(400_u16, 300_u16) == 300_u16, '300 < 400');
}

#[test]
fn test_max_u16() {
    assert(max(0_u16, 1_u16) == 1_u16, '1 > 0');
    assert(max(5_u16, 7_u16) == 7_u16, '7 > 5');
    assert(max(65535_u16, 32768_u16) == 65535_u16, '65535 > 32768');
    assert(max(10_u16, 10_u16) == 10_u16, '10 == 10');
    assert(max(0_u16, 0_u16) == 0_u16, '0 == 0');
    assert(max(65535_u16, 65535_u16) == 65535_u16, '65535 == 65535');
    assert(max(100_u16, 200_u16) == 200_u16, '200 > 100');
    assert(max(1_u16, 2_u16) == 2_u16, '2 > 1');
    assert(max(32767_u16, 32766_u16) == 32767_u16, '32767 > 32766');
    assert(max(400_u16, 300_u16) == 400_u16, '400 > 300');
}

#[test]
fn test_min_u32() {
    assert(min(0_u32, 1_u32) == 0_u32, '0 < 1');
    assert(min(5_u32, 7_u32) == 5_u32, '5 < 7');
    assert(min(4294967295_u32, 2147483648_u32) == 2147483648_u32, '2147483648 < 4294967295');
    assert(min(10_u32, 10_u32) == 10_u32, '10 == 10');
    assert(min(0_u32, 0_u32) == 0_u32, '0 == 0');
    assert(min(4294967295_u32, 4294967295_u32) == 4294967295_u32, '4294967295 == 4294967295');
    assert(min(100_u32, 200_u32) == 100_u32, '100 < 200');
    assert(min(1_u32, 2_u32) == 1_u32, '1 < 2');
    assert(min(2147483647_u32, 2147483646_u32) == 2147483646_u32, '2147483646 < 2147483647');
    assert(min(400_u32, 300_u32) == 300_u32, '300 < 400');
}

#[test]
fn test_max_u32() {
    assert(max(0_u32, 1_u32) == 1_u32, '1 > 0');
    assert(max(5_u32, 7_u32) == 7_u32, '7 > 5');
    assert(max(4294967295_u32, 2147483648_u32) == 4294967295_u32, '4294967295 > 2147483648');
    assert(max(10_u32, 10_u32) == 10_u32, '10 == 10');
    assert(max(0_u32, 0_u32) == 0_u32, '0 == 0');
    assert(max(4294967295_u32, 4294967295_u32) == 4294967295_u32, '4294967295 == 4294967295');
    assert(max(100_u32, 200_u32) == 200_u32, '200 > 100');
    assert(max(1_u32, 2_u32) == 2_u32, '2 > 1');
    assert(max(2147483647_u32, 2147483646_u32) == 2147483647_u32, '2147483647 > 2147483646');
    assert(max(400_u32, 300_u32) == 400_u32, '400 > 300');
}

#[test]
fn test_min_u64() {
    assert(min(0_u64, 1_u64) == 0_u64, '0 < 1');
    assert(min(5_u64, 7_u64) == 5_u64, '5 < 7');
    assert(
        min(18446744073709551615_u64, 9223372036854775808_u64) == 9223372036854775808_u64,
        '92233... < 18446...'
    );
    assert(min(10_u64, 10_u64) == 10_u64, '10 == 10');
    assert(min(0_u64, 0_u64) == 0_u64, '0 == 0');
    assert(
        min(18446744073709551615_u64, 18446744073709551615_u64) == 18446744073709551615_u64,
        '18446... == 18446...'
    );
    assert(min(100_u64, 200_u64) == 100_u64, '100 < 200');
    assert(min(1_u64, 2_u64) == 1_u64, '1 < 2');
    assert(
        min(9223372036854775807_u64, 9223372036854775806_u64) == 9223372036854775806_u64,
        '92233... < 92233...'
    );
    assert(min(400_u64, 300_u64) == 300_u64, '300 < 400');
}

#[test]
fn test_max_u64() {
    assert(max(0_u64, 1_u64) == 1_u64, '1 > 0');
    assert(max(5_u64, 7_u64) == 7_u64, '7 > 5');
    assert(
        max(18446744073709551615_u64, 9223372036854775808_u64) == 18446744073709551615_u64,
        '18446... > 92233...'
    );
    assert(max(10_u64, 10_u64) == 10_u64, '10 == 10');
    assert(max(0_u64, 0_u64) == 0_u64, '0 == 0');
    assert(
        max(18446744073709551615_u64, 18446744073709551615_u64) == 18446744073709551615_u64,
        '18446... == 18446...'
    );
    assert(max(100_u64, 200_u64) == 200_u64, '200 > 100');
    assert(max(1_u64, 2_u64) == 2_u64, '2 > 1');
    assert(
        max(9223372036854775807_u64, 9223372036854775806_u64) == 9223372036854775807_u64,
        '92233... > 92233...'
    );
    assert(max(400_u64, 300_u64) == 400_u64, '400 > 300');
}

#[test]
fn test_min_u128() {
    assert(min(0_u128, 1_u128) == 0_u128, '0 < 1');
    assert(min(5_u128, 7_u128) == 5_u128, '5 < 7');
    assert(
        min(
            340282366920938463463374607431768211455_u128,
            170141183460469231731687303715884105728_u128
        ) == 170141183460469231731687303715884105728_u128,
        '170141... < 340282...'
    );
    assert(min(10_u128, 10_u128) == 10_u128, '10 == 10');
    assert(min(0_u128, 0_u128) == 0_u128, '0 == 0');
    assert(
        min(
            340282366920938463463374607431768211455_u128,
            340282366920938463463374607431768211455_u128
        ) == 340282366920938463463374607431768211455_u128,
        '340282366... == 340282366...'
    );
    assert(min(100_u128, 200_u128) == 100_u128, '100 < 200');
    assert(min(1_u128, 2_u128) == 1_u128, '1 < 2');
    assert(
        min(
            170141183460469231731687303715884105727_u128,
            170141183460469231731687303715884105726_u128
        ) == 170141183460469231731687303715884105726_u128,
        '17014118... < 1701411834...'
    );
    assert(min(400_u128, 300_u128) == 300_u128, '300 < 400');
}

#[test]
fn test_max_u128() {
    assert(max(0_u128, 1_u128) == 1_u128, '1 > 0');
    assert(max(5_u128, 7_u128) == 7_u128, '7 > 5');
    assert(
        max(18446744073709551615_u128, 9223372036854775808_u128) == 18446744073709551615_u128,
        '18446744... > 922337...'
    );
    assert(max(10_u128, 10_u128) == 10_u128, '10 == 10');
    assert(max(0_u128, 0_u128) == 0_u128, '0 == 0');
    assert(
        max(18446744073709551615_u128, 18446744073709551615_u128) == 18446744073709551615_u128,
        '184467...== 184467'
    );
    assert(max(100_u128, 200_u128) == 200_u128, '200 > 100');
    assert(max(1_u128, 2_u128) == 2_u128, '2 > 1');
    assert(
        max(9223372036854775807_u128, 9223372036854775806_u128) == 9223372036854775807_u128,
        '922337203... > 922337...'
    );
    assert(max(400_u128, 300_u128) == 400_u128, '400 > 300');
}

#[test]
fn test_min_u256() {
    let a = u256 { low: 0, high: 0 };
    let b = u256 { low: 1, high: 0 };
    let c = u256 { low: 5, high: 0 };
    let d = u256 { low: 7, high: 0 };
    let e = u256 { low: 0, high: 1 };
    let f = u256 { low: 0, high: 2 };

    assert(min(a, b) == a, 'a < b');
    assert(min(c, d) == c, 'c < d');
    assert(min(e, f) == e, 'e < f');
    assert(min(a, a) == a, 'a == a');
    assert(min(b, b) == b, 'b == b');
    assert(min(f, f) == f, 'f == f');
}

#[test]
fn test_max_u256() {
    let a = u256 { low: 0, high: 0 };
    let b = u256 { low: 1, high: 0 };
    let c = u256 { low: 5, high: 0 };
    let d = u256 { low: 7, high: 0 };
    let e = u256 { low: 0, high: 1 };
    let f = u256 { low: 0, high: 2 };

    assert(max(a, b) == b, 'b > a');
    assert(max(c, d) == d, 'd > c');
    assert(max(e, f) == f, 'f > e');
    assert(max(a, a) == a, 'a == a');
    assert(max(b, b) == b, 'b == b');
    assert(max(f, f) == f, 'f == f');
}

// User-defined types
#[derive(Drop, Copy)]
struct Foo {
    val: u128
}

impl FooPartialOrd of PartialOrd<Foo> {
    fn le(lhs: Foo, rhs: Foo) -> bool {
        lhs.val <= rhs.val
    }

    fn ge(lhs: Foo, rhs: Foo) -> bool {
        lhs.val >= rhs.val
    }

    fn lt(lhs: Foo, rhs: Foo) -> bool {
        lhs.val < rhs.val
    }

    fn gt(lhs: Foo, rhs: Foo) -> bool {
        lhs.val > rhs.val
    }
}

#[test]
fn test_min_foo() {
    let a = Foo { val: 0 };
    let b = Foo { val: 1 };
    let c = Foo { val: 5 };
    let d = Foo { val: 7 };

    assert(min(a, b).val == a.val, 'a < b');
    assert(min(c, d).val == c.val, 'c < d');
    assert(min(a, a).val == a.val, 'a == a');
    assert(min(b, b).val == b.val, 'b == b');
}

#[test]
fn test_max_foo() {
    let a = Foo { val: 0 };
    let b = Foo { val: 1 };
    let c = Foo { val: 5 };
    let d = Foo { val: 7 };

    assert(max(a, b).val == b.val, 'b > a');
    assert(max(c, d).val == d.val, 'd > c');
    assert(max(a, a).val == a.val, 'a == a');
    assert(max(b, b).val == b.val, 'b == b');
}
