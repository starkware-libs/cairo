use core::cmp::{max, min, minmax};
use core::test::test_utils::{assert_eq, assert_ne};

// Integer tests

#[test]
fn test_min_u8() {
    assert_eq(@min(0_u8, 1_u8), @0_u8, '0 < 1');
    assert_eq(@min(0_u8, 1_u8), @0_u8, '0 < 1');
    assert_eq(@min(5_u8, 7_u8), @5_u8, '5 < 7');
    assert_eq(@min(255_u8, 128_u8), @128_u8, '128 < 255');
    assert_eq(@min(10_u8, 10_u8), @10_u8, '10 == 10');
    assert_eq(@min(0_u8, 0_u8), @0_u8, '0 == 0');
    assert_eq(@min(255_u8, 255_u8), @255_u8, '255 == 255');
    assert_eq(@min(3_u8, 4_u8), @3_u8, '3 < 4');
    assert_eq(@min(255_u8, 200_u8), @200_u8, '200 < 255');
    assert_eq(@min(250_u8, 253_u8), @250_u8, '250 < 253');
}

#[test]
fn test_max_u8() {
    assert_eq(@max(0_u8, 1_u8), @1_u8, '1 > 0');
    assert_eq(@max(5_u8, 7_u8), @7_u8, '7 > 5');
    assert_eq(@max(255_u8, 128_u8), @255_u8, '255 > 128');
    assert_eq(@max(10_u8, 10_u8), @10_u8, '10 == 10');
    assert_eq(@max(0_u8, 0_u8), @0_u8, '0 == 0');
    assert_eq(@max(255_u8, 255_u8), @255_u8, '255 == 255');
    assert_eq(@max(100_u8, 200_u8), @200_u8, '200 > 100');
    assert_eq(@max(1_u8, 2_u8), @2_u8, '2 > 1');
    assert_eq(@max(120_u8, 130_u8), @130_u8, '130 > 120');
    assert_eq(@max(200_u8, 150_u8), @200_u8, '200 > 150');
}

#[test]
fn test_minmax_u8() {
    assert!(minmax(0_u8, 1_u8) == (0_u8, 1_u8), "1 > 0");
    assert!(minmax(5_u8, 7_u8) == (5_u8, 7_u8), "7 > 5");
    assert!(minmax(255_u8, 128_u8) == (128_u8, 255_u8), "255 > 128");
    assert!(minmax(10_u8, 10_u8) == (10_u8, 10_u8), "10 == 10");
    assert!(minmax(0_u8, 0_u8) == (0_u8, 0_u8), "0 == 0");
    assert!(minmax(255_u8, 255_u8) == (255_u8, 255_u8), "255 == 255");
    assert!(minmax(100_u8, 200_u8) == (100_u8, 200_u8), "200 > 100");
    assert!(minmax(1_u8, 2_u8) == (1_u8, 2_u8), "2 > 1");
    assert!(minmax(120_u8, 130_u8) == (120_u8, 130_u8), "130 > 120");
    assert!(minmax(200_u8, 150_u8) == (150_u8, 200_u8), "200 > 150");
}

#[test]
fn test_min_u16() {
    assert_eq(@min(0_u16, 1_u16), @0_u16, '0 < 1');
    assert_eq(@min(5_u16, 7_u16), @5_u16, '5 < 7');
    assert_eq(@min(65535_u16, 32768_u16), @32768_u16, '32768 < 65535');
    assert_eq(@min(10_u16, 10_u16), @10_u16, '10 == 10');
    assert_eq(@min(0_u16, 0_u16), @0_u16, '0 == 0');
    assert_eq(@min(65535_u16, 65535_u16), @65535_u16, '65535 == 65535');
    assert_eq(@min(100_u16, 200_u16), @100_u16, '100 < 200');
    assert_eq(@min(1_u16, 2_u16), @1_u16, '1 < 2');
    assert_eq(@min(32767_u16, 32766_u16), @32766_u16, '32766 < 32767');
    assert_eq(@min(400_u16, 300_u16), @300_u16, '300 < 400');
}

#[test]
fn test_max_u16() {
    assert_eq(@max(0_u16, 1_u16), @1_u16, '1 > 0');
    assert_eq(@max(5_u16, 7_u16), @7_u16, '7 > 5');
    assert_eq(@max(65535_u16, 32768_u16), @65535_u16, '65535 > 32768');
    assert_eq(@max(10_u16, 10_u16), @10_u16, '10 == 10');
    assert_eq(@max(0_u16, 0_u16), @0_u16, '0 == 0');
    assert_eq(@max(65535_u16, 65535_u16), @65535_u16, '65535 == 65535');
    assert_eq(@max(100_u16, 200_u16), @200_u16, '200 > 100');
    assert_eq(@max(1_u16, 2_u16), @2_u16, '2 > 1');
    assert_eq(@max(32767_u16, 32766_u16), @32767_u16, '32767 > 32766');
    assert_eq(@max(400_u16, 300_u16), @400_u16, '400 > 300');
}

#[test]
fn test_min_u32() {
    assert_eq(@min(0_u32, 1_u32), @0_u32, '0 < 1');
    assert_eq(@min(5_u32, 7_u32), @5_u32, '5 < 7');
    assert_eq(@min(4294967295_u32, 2147483648_u32), @2147483648_u32, '2147483648 < 4294967295');
    assert_eq(@min(10_u32, 10_u32), @10_u32, '10 == 10');
    assert_eq(@min(0_u32, 0_u32), @0_u32, '0 == 0');
    assert_eq(@min(4294967295_u32, 4294967295_u32), @4294967295_u32, '4294967295 == 4294967295');
    assert_eq(@min(100_u32, 200_u32), @100_u32, '100 < 200');
    assert_eq(@min(1_u32, 2_u32), @1_u32, '1 < 2');
    assert_eq(@min(2147483647_u32, 2147483646_u32), @2147483646_u32, '2147483646 < 2147483647');
    assert_eq(@min(400_u32, 300_u32), @300_u32, '300 < 400');
}

#[test]
fn test_max_u32() {
    assert_eq(@max(0_u32, 1_u32), @1_u32, '1 > 0');
    assert_eq(@max(5_u32, 7_u32), @7_u32, '7 > 5');
    assert_eq(@max(4294967295_u32, 2147483648_u32), @4294967295_u32, '4294967295 > 2147483648');
    assert_eq(@max(10_u32, 10_u32), @10_u32, '10 == 10');
    assert_eq(@max(0_u32, 0_u32), @0_u32, '0 == 0');
    assert_eq(@max(4294967295_u32, 4294967295_u32), @4294967295_u32, '4294967295 == 4294967295');
    assert_eq(@max(100_u32, 200_u32), @200_u32, '200 > 100');
    assert_eq(@max(1_u32, 2_u32), @2_u32, '2 > 1');
    assert_eq(@max(2147483647_u32, 2147483646_u32), @2147483647_u32, '2147483647 > 2147483646');
    assert_eq(@max(400_u32, 300_u32), @400_u32, '400 > 300');
}

#[test]
fn test_min_u64() {
    assert_eq(@min(0_u64, 1_u64), @0_u64, '0 < 1');
    assert_eq(@min(5_u64, 7_u64), @5_u64, '5 < 7');
    assert_eq(
        @min(18446744073709551615_u64, 9223372036854775808_u64),
        @9223372036854775808_u64,
        '92233... < 18446...'
    );
    assert_eq(@min(10_u64, 10_u64), @10_u64, '10 == 10');
    assert_eq(@min(0_u64, 0_u64), @0_u64, '0 == 0');
    assert_eq(
        @min(18446744073709551615_u64, 18446744073709551615_u64),
        @18446744073709551615_u64,
        '18446... == 18446...'
    );
    assert_eq(@min(100_u64, 200_u64), @100_u64, '100 < 200');
    assert_eq(@min(1_u64, 2_u64), @1_u64, '1 < 2');
    assert_eq(
        @min(9223372036854775807_u64, 9223372036854775806_u64),
        @9223372036854775806_u64,
        '92233... < 92233...'
    );
    assert_eq(@min(400_u64, 300_u64), @300_u64, '300 < 400');
}

#[test]
fn test_max_u64() {
    assert_eq(@max(0_u64, 1_u64), @1_u64, '1 > 0');
    assert_eq(@max(5_u64, 7_u64), @7_u64, '7 > 5');
    assert_eq(
        @max(18446744073709551615_u64, 9223372036854775808_u64),
        @18446744073709551615_u64,
        '18446... > 92233...'
    );
    assert_eq(@max(10_u64, 10_u64), @10_u64, '10 == 10');
    assert_eq(@max(0_u64, 0_u64), @0_u64, '0 == 0');
    assert_eq(
        @max(18446744073709551615_u64, 18446744073709551615_u64),
        @18446744073709551615_u64,
        '18446... == 18446...'
    );
    assert_eq(@max(100_u64, 200_u64), @200_u64, '200 > 100');
    assert_eq(@max(1_u64, 2_u64), @2_u64, '2 > 1');
    assert_eq(
        @max(9223372036854775807_u64, 9223372036854775806_u64),
        @9223372036854775807_u64,
        '92233... > 92233...'
    );
    assert_eq(@max(400_u64, 300_u64), @400_u64, '400 > 300');
}

#[test]
fn test_min_u128() {
    assert_eq(@min(0_u128, 1_u128), @0_u128, '0 < 1');
    assert_eq(@min(5_u128, 7_u128), @5_u128, '5 < 7');
    assert_eq(
        @min(
            340282366920938463463374607431768211455_u128,
            170141183460469231731687303715884105728_u128
        ),
        @170141183460469231731687303715884105728_u128,
        '170141... < 340282...'
    );
    assert_eq(@min(10_u128, 10_u128), @10_u128, '10 == 10');
    assert_eq(@min(0_u128, 0_u128), @0_u128, '0 == 0');
    assert_eq(
        @min(
            340282366920938463463374607431768211455_u128,
            340282366920938463463374607431768211455_u128
        ),
        @340282366920938463463374607431768211455_u128,
        '340282366... == 340282366...'
    );
    assert_eq(@min(100_u128, 200_u128), @100_u128, '100 < 200');
    assert_eq(@min(1_u128, 2_u128), @1_u128, '1 < 2');
    assert_eq(
        @min(
            170141183460469231731687303715884105727_u128,
            170141183460469231731687303715884105726_u128
        ),
        @170141183460469231731687303715884105726_u128,
        '17014118... < 1701411834...'
    );
    assert_eq(@min(400_u128, 300_u128), @300_u128, '300 < 400');
}

#[test]
fn test_max_u128() {
    assert_eq(@max(0_u128, 1_u128), @1_u128, '1 > 0');
    assert_eq(@max(5_u128, 7_u128), @7_u128, '7 > 5');
    assert_eq(
        @max(18446744073709551615_u128, 9223372036854775808_u128),
        @18446744073709551615_u128,
        '18446744... > 922337...'
    );
    assert_eq(@max(10_u128, 10_u128), @10_u128, '10 == 10');
    assert_eq(@max(0_u128, 0_u128), @0_u128, '0 == 0');
    assert_eq(
        @max(18446744073709551615_u128, 18446744073709551615_u128),
        @18446744073709551615_u128,
        '184467...== 184467'
    );
    assert_eq(@max(100_u128, 200_u128), @200_u128, '200 > 100');
    assert_eq(@max(1_u128, 2_u128), @2_u128, '2 > 1');
    assert_eq(
        @max(9223372036854775807_u128, 9223372036854775806_u128),
        @9223372036854775807_u128,
        '922337203... > 922337...'
    );
    assert_eq(@max(400_u128, 300_u128), @400_u128, '400 > 300');
}

#[test]
fn test_min_u256() {
    let a = u256 { low: 0, high: 0 };
    let b = u256 { low: 1, high: 0 };
    let c = u256 { low: 5, high: 0 };
    let d = u256 { low: 7, high: 0 };
    let e = u256 { low: 0, high: 1 };
    let f = u256 { low: 0, high: 2 };

    assert_eq(@min(a, b), @a, 'a < b');
    assert_eq(@min(c, d), @c, 'c < d');
    assert_eq(@min(e, f), @e, 'e < f');
    assert_eq(@min(a, a), @a, 'a == a');
    assert_eq(@min(b, b), @b, 'b == b');
    assert_eq(@min(f, f), @f, 'f == f');
}

#[test]
fn test_max_u256() {
    let a = u256 { low: 0, high: 0 };
    let b = u256 { low: 1, high: 0 };
    let c = u256 { low: 5, high: 0 };
    let d = u256 { low: 7, high: 0 };
    let e = u256 { low: 0, high: 1 };
    let f = u256 { low: 0, high: 2 };

    assert_eq(@max(a, b), @b, 'b > a');
    assert_eq(@max(c, d), @d, 'd > c');
    assert_eq(@max(e, f), @f, 'f > e');
    assert_eq(@max(a, a), @a, 'a == a');
    assert_eq(@max(b, b), @b, 'b == b');
    assert_eq(@max(f, f), @f, 'f == f');
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

    assert_eq(@min(a, b).val, @a.val, 'a < b');
    assert_eq(@min(c, d).val, @c.val, 'c < d');
    assert_eq(@min(a, a).val, @a.val, 'a == a');
    assert_eq(@min(b, b).val, @b.val, 'b == b');
}

#[test]
fn test_max_foo() {
    let a = Foo { val: 0 };
    let b = Foo { val: 1 };
    let c = Foo { val: 5 };
    let d = Foo { val: 7 };

    assert_eq(@max(a, b).val, @b.val, 'b > a');
    assert_eq(@max(c, d).val, @d.val, 'd > c');
    assert_eq(@max(a, a).val, @a.val, 'a == a');
    assert_eq(@max(b, b).val, @b.val, 'b == b');
}
