use core::num::traits::BitSize;
use core::num::traits::{
    OverflowingAdd, OverflowingSub, OverflowingMul, WrappingAdd, WrappingSub, WrappingMul
};
use core::integer::BoundedInt;


#[test]
fn test_bit_size() {
    assert!(BitSize::<u8>::bits() == 8);
    assert!(BitSize::<u16>::bits() == 16);
    assert!(BitSize::<u32>::bits() == 32);
    assert!(BitSize::<u64>::bits() == 64);
    assert!(BitSize::<u128>::bits() == 128);
    assert!(BitSize::<u256>::bits() == 256);
    assert!(BitSize::<i8>::bits() == 8);
    assert!(BitSize::<i16>::bits() == 16);
    assert!(BitSize::<i32>::bits() == 32);
    assert!(BitSize::<i64>::bits() == 64);
    assert!(BitSize::<i128>::bits() == 128);
    assert!(BitSize::<bytes31>::bits() == 248);
}

// Overflowing tests

#[test]
fn tests_overflowing_add_unsigned_integers() {
    assert_eq!(1_u8.overflowing_add(2), (3, false));
    assert_eq!(BoundedInt::<u8>::max().overflowing_add(1), (0, true));
    assert_eq!(1_u16.overflowing_add(2), (3, false));
    assert_eq!(BoundedInt::<u16>::max().overflowing_add(1), (0, true));
    assert_eq!(1_u32.overflowing_add(2), (3, false));
    assert_eq!(BoundedInt::<u32>::max().overflowing_add(1), (0, true));
    assert_eq!(1_u64.overflowing_add(2), (3, false));
    assert_eq!(BoundedInt::<u64>::max().overflowing_add(1), (0, true));
    assert_eq!(1_u128.overflowing_add(2), (3, false));
    assert_eq!(BoundedInt::<u128>::max().overflowing_add(1), (0, true));
    assert_eq!(1_u256.overflowing_add(2), (3, false));
    assert_eq!(BoundedInt::<u256>::max().overflowing_add(1), (0, true));
}

#[test]
fn test_overflowing_add_positive_signed_integers() {
    assert!(1_i8.overflowing_add(2) == (3, false));
    assert!(BoundedInt::<i8>::max().overflowing_add(1) == (-0x80, true));
    assert!(1_i16.overflowing_add(2) == (3, false));
    assert!(BoundedInt::<i16>::max().overflowing_add(1) == (-0x8000, true));
    assert!(1_i32.overflowing_add(2) == (3, false));
    assert!(BoundedInt::<i32>::max().overflowing_add(1) == (-0x80000000, true));
    assert!(1_i64.overflowing_add(2) == (3, false));
    assert!(BoundedInt::<i64>::max().overflowing_add(1) == (-0x8000000000000000, true));
    assert!(1_i128.overflowing_add(2) == (3, false));
    assert!(
        BoundedInt::<i128>::max().overflowing_add(1) == (-0x80000000000000000000000000000000, true)
    );
}

#[test]
fn test_overflowing_add_negative_signed_integers() {
    assert!((-1_i8).overflowing_add(-2) == (-3, false));
    assert!(BoundedInt::<i8>::min().overflowing_add(-1) == (0x7f, true));
    assert!((-1_i16).overflowing_add(-2) == (-3, false));
    assert!(BoundedInt::<i16>::min().overflowing_add(-1) == (0x7fff, true));
    assert!((-1_i32).overflowing_add(-2) == (-3, false));
    assert!(BoundedInt::<i32>::min().overflowing_add(-1) == (0x7fffffff, true));
    assert!((-1_i64).overflowing_add(-2) == (-3, false));
    assert!(BoundedInt::<i64>::min().overflowing_add(-1) == (0x7fffffffffffffff, true));
    assert!((-1_i128).overflowing_add(-2) == (-3, false));
    assert!(
        BoundedInt::<i128>::min().overflowing_add(-1) == (0x7fffffffffffffffffffffffffffffff, true)
    );
}

fn test_overflowing_sub_unsigned_integers() {
    assert_eq!(3_u8.overflowing_sub(2), (1, false));
    assert_eq!(0_u8.overflowing_sub(1), (BoundedInt::<u8>::max(), true));
    assert_eq!(3_u16.overflowing_sub(2), (1, false));
    assert_eq!(0_u16.overflowing_sub(1), (BoundedInt::<u16>::max(), true));
    assert_eq!(3_u32.overflowing_sub(2), (1, false));
    assert_eq!(0_u32.overflowing_sub(1), (BoundedInt::<u32>::max(), true));
    assert_eq!(3_u64.overflowing_sub(2), (1, false));
    assert_eq!(0_u64.overflowing_sub(1), (BoundedInt::<u64>::max(), true));
    assert_eq!(3_u128.overflowing_sub(2), (1, false));
    assert_eq!(0_u128.overflowing_sub(1), (BoundedInt::<u128>::max(), true));
    assert_eq!(3_u256.overflowing_sub(2), (1, false));
    assert_eq!(0_u256.overflowing_sub(1), (BoundedInt::<u256>::max(), true));
}

#[test]
fn test_overflowing_sub_positive_signed_integers() {
    assert!(3_i8.overflowing_sub(2) == (1, false));
    assert!(BoundedInt::<i8>::min().overflowing_sub(1) == (BoundedInt::<i8>::max(), true));
    assert!(3_i16.overflowing_sub(2) == (1, false));
    assert!(BoundedInt::<i16>::min().overflowing_sub(1) == (BoundedInt::<i16>::max(), true));
    assert!(3_i32.overflowing_sub(2) == (1, false));
    assert!(BoundedInt::<i32>::min().overflowing_sub(1) == (BoundedInt::<i32>::max(), true));
    assert!(3_i64.overflowing_sub(2) == (1, false));
    assert!(BoundedInt::<i64>::min().overflowing_sub(1) == (BoundedInt::<i64>::max(), true));
    assert!(3_i128.overflowing_sub(2) == (1, false));
    assert!(BoundedInt::<i128>::min().overflowing_sub(1) == (BoundedInt::<i128>::max(), true));
}

#[test]
fn test_overflowing_sub_negative_signed_integers() {
    assert!((-3_i8).overflowing_sub(-2) == (-1, false));
    assert!(BoundedInt::<i8>::max().overflowing_sub(-1) == (BoundedInt::<i8>::min(), true));
    assert!((-3_i16).overflowing_sub(-2) == (-1, false));
    assert!(BoundedInt::<i16>::max().overflowing_sub(-1) == (BoundedInt::<i16>::min(), true));
    assert!((-3_i32).overflowing_sub(-2) == (-1, false));
    assert!(BoundedInt::<i32>::max().overflowing_sub(-1) == (BoundedInt::<i32>::min(), true));
    assert!((-3_i64).overflowing_sub(-2) == (-1, false));
    assert!(BoundedInt::<i64>::max().overflowing_sub(-1) == (BoundedInt::<i64>::min(), true));
    assert!((-3_i128).overflowing_sub(-2) == (-1, false));
    assert!(BoundedInt::<i128>::max().overflowing_sub(-1) == (BoundedInt::<i128>::min(), true));
}

#[test]
fn test_overflowing_mul_unsigned_integers() {
    assert_eq!(2_u8.overflowing_mul(3), (6, false));
    assert_eq!(BoundedInt::<u8>::max().overflowing_mul(2), (BoundedInt::<u8>::max() - 1, true));
    assert_eq!(2_u16.overflowing_mul(3), (6, false));
    assert_eq!(BoundedInt::<u16>::max().overflowing_mul(2), (BoundedInt::<u16>::max() - 1, true));
    assert_eq!(2_u32.overflowing_mul(3), (6, false));
    assert_eq!(BoundedInt::<u32>::max().overflowing_mul(2), (BoundedInt::<u32>::max() - 1, true));
    assert_eq!(2_u64.overflowing_mul(3), (6, false));
    assert_eq!(BoundedInt::<u64>::max().overflowing_mul(2), (BoundedInt::<u64>::max() - 1, true));
    assert_eq!(2_u128.overflowing_mul(3), (6, false));
    assert_eq!(BoundedInt::<u128>::max().overflowing_mul(2), (BoundedInt::<u128>::max() - 1, true));
    assert_eq!(2_u256.overflowing_mul(3), (6, false));
    assert_eq!(BoundedInt::<u256>::max().overflowing_mul(2), (BoundedInt::<u256>::max() - 1, true));
}

// Wrapping tests

#[test]
fn tests_wrapping_add_unsigned_integers() {
    assert_eq!(1_u8.wrapping_add(2), 3);
    assert_eq!(BoundedInt::<u8>::max().wrapping_add(1), 0);
    assert_eq!(1_u16.wrapping_add(2), 3);
    assert_eq!(BoundedInt::<u16>::max().wrapping_add(1), 0);
    assert_eq!(1_u32.wrapping_add(2), 3);
    assert_eq!(BoundedInt::<u32>::max().wrapping_add(1), 0);
    assert_eq!(1_u64.wrapping_add(2), 3);
    assert_eq!(BoundedInt::<u64>::max().wrapping_add(1), 0);
    assert_eq!(1_u128.wrapping_add(2), 3);
    assert_eq!(BoundedInt::<u128>::max().wrapping_add(1), 0);
    assert_eq!(1_u256.wrapping_add(2), 3);
    assert_eq!(BoundedInt::<u256>::max().wrapping_add(1), 0);
}

#[test]
fn test_wrapping_add_positive_signed_integers() {
    assert!(1_i8.wrapping_add(2) == 3);
    assert!(BoundedInt::<i8>::max().wrapping_add(1) == -0x80);
    assert!(1_i16.wrapping_add(2) == 3);
    assert!(BoundedInt::<i16>::max().wrapping_add(1) == -0x8000);
    assert!(1_i32.wrapping_add(2) == 3);
    assert!(BoundedInt::<i32>::max().wrapping_add(1) == -0x80000000);
    assert!(1_i64.wrapping_add(2) == 3);
    assert!(BoundedInt::<i64>::max().wrapping_add(1) == -0x8000000000000000);
    assert!(1_i128.wrapping_add(2) == 3);
    assert!(BoundedInt::<i128>::max().wrapping_add(1) == -0x80000000000000000000000000000000);
}

#[test]
fn test_wrapping_add_negative_signed_integers() {
    assert!((-1_i8).wrapping_add(-2) == -3);
    assert!(BoundedInt::<i8>::min().wrapping_add(-1) == 0x7f);
    assert!((-1_i16).wrapping_add(-2) == -3);
    assert!(BoundedInt::<i16>::min().wrapping_add(-1) == 0x7fff);
    assert!((-1_i32).wrapping_add(-2) == -3);
    assert!(BoundedInt::<i32>::min().wrapping_add(-1) == 0x7fffffff);
    assert!((-1_i64).wrapping_add(-2) == -3);
    assert!(BoundedInt::<i64>::min().wrapping_add(-1) == 0x7fffffffffffffff);
    assert!((-1_i128).wrapping_add(-2) == -3);
    assert!(BoundedInt::<i128>::min().wrapping_add(-1) == 0x7fffffffffffffffffffffffffffffff);
}

#[test]
fn test_wrapping_sub_unsigned_integers() {
    assert_eq!(3_u8.wrapping_sub(2), 1);
    assert_eq!(0_u8.wrapping_sub(1), BoundedInt::<u8>::max());
    assert_eq!(3_u16.wrapping_sub(2), 1);
    assert_eq!(0_u16.wrapping_sub(1), BoundedInt::<u16>::max());
    assert_eq!(3_u32.wrapping_sub(2), 1);
    assert_eq!(0_u32.wrapping_sub(1), BoundedInt::<u32>::max());
    assert_eq!(3_u64.wrapping_sub(2), 1);
    assert_eq!(0_u64.wrapping_sub(1), BoundedInt::<u64>::max());
    assert_eq!(3_u128.wrapping_sub(2), 1);
    assert_eq!(0_u128.wrapping_sub(1), BoundedInt::<u128>::max());
    assert_eq!(3_u256.wrapping_sub(2), 1);
    assert_eq!(0_u256.wrapping_sub(1), BoundedInt::<u256>::max());
}

#[test]
fn test_wrapping_sub_positive_signed_integers() {
    assert!(3_i8.wrapping_sub(2) == 1);
    assert!(BoundedInt::<i8>::min().wrapping_sub(1) == BoundedInt::<i8>::max());
    assert!(3_i16.wrapping_sub(2) == 1);
    assert!(BoundedInt::<i16>::min().wrapping_sub(1) == BoundedInt::<i16>::max());
    assert!(3_i32.wrapping_sub(2) == 1);
    assert!(BoundedInt::<i32>::min().wrapping_sub(1) == BoundedInt::<i32>::max());
    assert!(3_i64.wrapping_sub(2) == 1);
    assert!(BoundedInt::<i64>::min().wrapping_sub(1) == BoundedInt::<i64>::max());
    assert!(3_i128.wrapping_sub(2) == 1);
    assert!(BoundedInt::<i128>::min().wrapping_sub(1) == BoundedInt::<i128>::max());
}

#[test]
fn test_wrapping_sub_negative_signed_integers() {
    assert!((-3_i8).wrapping_sub(-2) == -1);
    assert!(BoundedInt::<i8>::max().wrapping_sub(-1) == BoundedInt::<i8>::min());
    assert!((-3_i16).wrapping_sub(-2) == -1);
    assert!(BoundedInt::<i16>::max().wrapping_sub(-1) == BoundedInt::<i16>::min());
    assert!((-3_i32).wrapping_sub(-2) == -1);
    assert!(BoundedInt::<i32>::max().wrapping_sub(-1) == BoundedInt::<i32>::min());
    assert!((-3_i64).wrapping_sub(-2) == -1);
    assert!(BoundedInt::<i64>::max().wrapping_sub(-1) == BoundedInt::<i64>::min());
    assert!((-3_i128).wrapping_sub(-2) == -1);
    assert!(BoundedInt::<i128>::max().wrapping_sub(-1) == BoundedInt::<i128>::min());
}

#[test]
fn test_wrapping_mul_unsigned_integers() {
    assert_eq!(2_u8.wrapping_mul(3), 6);
    assert_eq!(BoundedInt::<u8>::max().wrapping_mul(2), BoundedInt::<u8>::max() - 1);
    assert_eq!(2_u16.wrapping_mul(3), 6);
    assert_eq!(BoundedInt::<u16>::max().wrapping_mul(2), BoundedInt::<u16>::max() - 1);
    assert_eq!(2_u32.wrapping_mul(3), 6);
    assert_eq!(BoundedInt::<u32>::max().wrapping_mul(2), BoundedInt::<u32>::max() - 1);
    assert_eq!(2_u64.wrapping_mul(3), 6);
    assert_eq!(BoundedInt::<u64>::max().wrapping_mul(2), BoundedInt::<u64>::max() - 1);
    assert_eq!(2_u128.wrapping_mul(3), 6);
    assert_eq!(BoundedInt::<u128>::max().wrapping_mul(2), BoundedInt::<u128>::max() - 1);
    assert_eq!(2_u256.wrapping_mul(3), 6);
    assert_eq!(BoundedInt::<u256>::max().wrapping_mul(2), BoundedInt::<u256>::max() - 1);
}
