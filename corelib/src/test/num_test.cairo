use crate::num::traits::{
    BitSize, Bounded, CheckedAdd, CheckedMul, CheckedSub, OverflowingAdd, OverflowingMul,
    OverflowingSub, Pow, SaturatingAdd, SaturatingMul, SaturatingSub, WrappingAdd, WrappingMul,
    WrappingSub,
};


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
    assert_eq!(Bounded::<u8>::MAX.overflowing_add(1), (0, true));
    assert_eq!(1_u16.overflowing_add(2), (3, false));
    assert_eq!(Bounded::<u16>::MAX.overflowing_add(1), (0, true));
    assert_eq!(1_u32.overflowing_add(2), (3, false));
    assert_eq!(Bounded::<u32>::MAX.overflowing_add(1), (0, true));
    assert_eq!(1_u64.overflowing_add(2), (3, false));
    assert_eq!(Bounded::<u64>::MAX.overflowing_add(1), (0, true));
    assert_eq!(1_u128.overflowing_add(2), (3, false));
    assert_eq!(Bounded::<u128>::MAX.overflowing_add(1), (0, true));
    assert_eq!(1_u256.overflowing_add(2), (3, false));
    assert_eq!(Bounded::<u256>::MAX.overflowing_add(1), (0, true));
}

#[test]
fn test_overflowing_add_positive_signed_integers() {
    assert!(1_i8.overflowing_add(2) == (3, false));
    assert!(Bounded::<i8>::MAX.overflowing_add(1) == (-0x80, true));
    assert!(1_i16.overflowing_add(2) == (3, false));
    assert!(Bounded::<i16>::MAX.overflowing_add(1) == (-0x8000, true));
    assert!(1_i32.overflowing_add(2) == (3, false));
    assert!(Bounded::<i32>::MAX.overflowing_add(1) == (-0x80000000, true));
    assert!(1_i64.overflowing_add(2) == (3, false));
    assert!(Bounded::<i64>::MAX.overflowing_add(1) == (-0x8000000000000000, true));
    assert!(1_i128.overflowing_add(2) == (3, false));
    assert!(Bounded::<i128>::MAX.overflowing_add(1) == (-0x80000000000000000000000000000000, true));
}

#[test]
fn test_overflowing_add_negative_signed_integers() {
    assert!((-1_i8).overflowing_add(-2) == (-3, false));
    assert!(Bounded::<i8>::MIN.overflowing_add(-1) == (0x7f, true));
    assert!((-1_i16).overflowing_add(-2) == (-3, false));
    assert!(Bounded::<i16>::MIN.overflowing_add(-1) == (0x7fff, true));
    assert!((-1_i32).overflowing_add(-2) == (-3, false));
    assert!(Bounded::<i32>::MIN.overflowing_add(-1) == (0x7fffffff, true));
    assert!((-1_i64).overflowing_add(-2) == (-3, false));
    assert!(Bounded::<i64>::MIN.overflowing_add(-1) == (0x7fffffffffffffff, true));
    assert!((-1_i128).overflowing_add(-2) == (-3, false));
    assert!(Bounded::<i128>::MIN.overflowing_add(-1) == (0x7fffffffffffffffffffffffffffffff, true));
}

fn test_overflowing_sub_unsigned_integers() {
    assert_eq!(3_u8.overflowing_sub(2), (1, false));
    assert_eq!(0_u8.overflowing_sub(1), (Bounded::<u8>::MAX, true));
    assert_eq!(3_u16.overflowing_sub(2), (1, false));
    assert_eq!(0_u16.overflowing_sub(1), (Bounded::<u16>::MAX, true));
    assert_eq!(3_u32.overflowing_sub(2), (1, false));
    assert_eq!(0_u32.overflowing_sub(1), (Bounded::<u32>::MAX, true));
    assert_eq!(3_u64.overflowing_sub(2), (1, false));
    assert_eq!(0_u64.overflowing_sub(1), (Bounded::<u64>::MAX, true));
    assert_eq!(3_u128.overflowing_sub(2), (1, false));
    assert_eq!(0_u128.overflowing_sub(1), (Bounded::<u128>::MAX, true));
    assert_eq!(3_u256.overflowing_sub(2), (1, false));
    assert_eq!(0_u256.overflowing_sub(1), (Bounded::<u256>::MAX, true));
}

#[test]
fn test_overflowing_sub_positive_signed_integers() {
    assert!(3_i8.overflowing_sub(2) == (1, false));
    assert!(Bounded::<i8>::MIN.overflowing_sub(1) == (Bounded::<i8>::MAX, true));
    assert!(3_i16.overflowing_sub(2) == (1, false));
    assert!(Bounded::<i16>::MIN.overflowing_sub(1) == (Bounded::<i16>::MAX, true));
    assert!(3_i32.overflowing_sub(2) == (1, false));
    assert!(Bounded::<i32>::MIN.overflowing_sub(1) == (Bounded::<i32>::MAX, true));
    assert!(3_i64.overflowing_sub(2) == (1, false));
    assert!(Bounded::<i64>::MIN.overflowing_sub(1) == (Bounded::<i64>::MAX, true));
    assert!(3_i128.overflowing_sub(2) == (1, false));
    assert!(Bounded::<i128>::MIN.overflowing_sub(1) == (Bounded::<i128>::MAX, true));
}

#[test]
fn test_overflowing_sub_negative_signed_integers() {
    assert!((-3_i8).overflowing_sub(-2) == (-1, false));
    assert!(Bounded::<i8>::MAX.overflowing_sub(-1) == (Bounded::<i8>::MIN, true));
    assert!((-3_i16).overflowing_sub(-2) == (-1, false));
    assert!(Bounded::<i16>::MAX.overflowing_sub(-1) == (Bounded::<i16>::MIN, true));
    assert!((-3_i32).overflowing_sub(-2) == (-1, false));
    assert!(Bounded::<i32>::MAX.overflowing_sub(-1) == (Bounded::<i32>::MIN, true));
    assert!((-3_i64).overflowing_sub(-2) == (-1, false));
    assert!(Bounded::<i64>::MAX.overflowing_sub(-1) == (Bounded::<i64>::MIN, true));
    assert!((-3_i128).overflowing_sub(-2) == (-1, false));
    assert!(Bounded::<i128>::MAX.overflowing_sub(-1) == (Bounded::<i128>::MIN, true));
}

#[test]
fn test_overflowing_mul_unsigned_integers() {
    assert_eq!(2_u8.overflowing_mul(3), (6, false));
    assert_eq!(Bounded::<u8>::MAX.overflowing_mul(2), (Bounded::<u8>::MAX - 1, true));
    assert_eq!(2_u16.overflowing_mul(3), (6, false));
    assert_eq!(Bounded::<u16>::MAX.overflowing_mul(2), (Bounded::<u16>::MAX - 1, true));
    assert_eq!(2_u32.overflowing_mul(3), (6, false));
    assert_eq!(Bounded::<u32>::MAX.overflowing_mul(2), (Bounded::<u32>::MAX - 1, true));
    assert_eq!(2_u64.overflowing_mul(3), (6, false));
    assert_eq!(Bounded::<u64>::MAX.overflowing_mul(2), (Bounded::<u64>::MAX - 1, true));
    assert_eq!(2_u128.overflowing_mul(3), (6, false));
    assert_eq!(Bounded::<u128>::MAX.overflowing_mul(2), (Bounded::<u128>::MAX - 1, true));
    assert_eq!(2_u256.overflowing_mul(3), (6, false));
    assert_eq!(Bounded::<u256>::MAX.overflowing_mul(2), (Bounded::<u256>::MAX - 1, true));
}

// Wrapping tests

#[test]
fn tests_wrapping_add_unsigned_integers() {
    assert_eq!(1_u8.wrapping_add(2), 3);
    assert_eq!(Bounded::<u8>::MAX.wrapping_add(1), 0);
    assert_eq!(1_u16.wrapping_add(2), 3);
    assert_eq!(Bounded::<u16>::MAX.wrapping_add(1), 0);
    assert_eq!(1_u32.wrapping_add(2), 3);
    assert_eq!(Bounded::<u32>::MAX.wrapping_add(1), 0);
    assert_eq!(1_u64.wrapping_add(2), 3);
    assert_eq!(Bounded::<u64>::MAX.wrapping_add(1), 0);
    assert_eq!(1_u128.wrapping_add(2), 3);
    assert_eq!(Bounded::<u128>::MAX.wrapping_add(1), 0);
    assert_eq!(1_u256.wrapping_add(2), 3);
    assert_eq!(Bounded::<u256>::MAX.wrapping_add(1), 0);
}

#[test]
fn test_wrapping_add_positive_signed_integers() {
    assert!(1_i8.wrapping_add(2) == 3);
    assert!(Bounded::<i8>::MAX.wrapping_add(1) == -0x80);
    assert!(1_i16.wrapping_add(2) == 3);
    assert!(Bounded::<i16>::MAX.wrapping_add(1) == -0x8000);
    assert!(1_i32.wrapping_add(2) == 3);
    assert!(Bounded::<i32>::MAX.wrapping_add(1) == -0x80000000);
    assert!(1_i64.wrapping_add(2) == 3);
    assert!(Bounded::<i64>::MAX.wrapping_add(1) == -0x8000000000000000);
    assert!(1_i128.wrapping_add(2) == 3);
    assert!(Bounded::<i128>::MAX.wrapping_add(1) == -0x80000000000000000000000000000000);
}

#[test]
fn test_wrapping_add_negative_signed_integers() {
    assert!((-1_i8).wrapping_add(-2) == -3);
    assert!(Bounded::<i8>::MIN.wrapping_add(-1) == 0x7f);
    assert!((-1_i16).wrapping_add(-2) == -3);
    assert!(Bounded::<i16>::MIN.wrapping_add(-1) == 0x7fff);
    assert!((-1_i32).wrapping_add(-2) == -3);
    assert!(Bounded::<i32>::MIN.wrapping_add(-1) == 0x7fffffff);
    assert!((-1_i64).wrapping_add(-2) == -3);
    assert!(Bounded::<i64>::MIN.wrapping_add(-1) == 0x7fffffffffffffff);
    assert!((-1_i128).wrapping_add(-2) == -3);
    assert!(Bounded::<i128>::MIN.wrapping_add(-1) == 0x7fffffffffffffffffffffffffffffff);
}

#[test]
fn test_wrapping_sub_unsigned_integers() {
    assert_eq!(3_u8.wrapping_sub(2), 1);
    assert_eq!(0_u8.wrapping_sub(1), Bounded::<u8>::MAX);
    assert_eq!(3_u16.wrapping_sub(2), 1);
    assert_eq!(0_u16.wrapping_sub(1), Bounded::<u16>::MAX);
    assert_eq!(3_u32.wrapping_sub(2), 1);
    assert_eq!(0_u32.wrapping_sub(1), Bounded::<u32>::MAX);
    assert_eq!(3_u64.wrapping_sub(2), 1);
    assert_eq!(0_u64.wrapping_sub(1), Bounded::<u64>::MAX);
    assert_eq!(3_u128.wrapping_sub(2), 1);
    assert_eq!(0_u128.wrapping_sub(1), Bounded::<u128>::MAX);
    assert_eq!(3_u256.wrapping_sub(2), 1);
    assert_eq!(0_u256.wrapping_sub(1), Bounded::<u256>::MAX);
}

#[test]
fn test_wrapping_sub_positive_signed_integers() {
    assert!(3_i8.wrapping_sub(2) == 1);
    assert!(Bounded::<i8>::MIN.wrapping_sub(1) == Bounded::<i8>::MAX);
    assert!(3_i16.wrapping_sub(2) == 1);
    assert!(Bounded::<i16>::MIN.wrapping_sub(1) == Bounded::<i16>::MAX);
    assert!(3_i32.wrapping_sub(2) == 1);
    assert!(Bounded::<i32>::MIN.wrapping_sub(1) == Bounded::<i32>::MAX);
    assert!(3_i64.wrapping_sub(2) == 1);
    assert!(Bounded::<i64>::MIN.wrapping_sub(1) == Bounded::<i64>::MAX);
    assert!(3_i128.wrapping_sub(2) == 1);
    assert!(Bounded::<i128>::MIN.wrapping_sub(1) == Bounded::<i128>::MAX);
}

#[test]
fn test_wrapping_sub_negative_signed_integers() {
    assert!((-3_i8).wrapping_sub(-2) == -1);
    assert!(Bounded::<i8>::MAX.wrapping_sub(-1) == Bounded::<i8>::MIN);
    assert!((-3_i16).wrapping_sub(-2) == -1);
    assert!(Bounded::<i16>::MAX.wrapping_sub(-1) == Bounded::<i16>::MIN);
    assert!((-3_i32).wrapping_sub(-2) == -1);
    assert!(Bounded::<i32>::MAX.wrapping_sub(-1) == Bounded::<i32>::MIN);
    assert!((-3_i64).wrapping_sub(-2) == -1);
    assert!(Bounded::<i64>::MAX.wrapping_sub(-1) == Bounded::<i64>::MIN);
    assert!((-3_i128).wrapping_sub(-2) == -1);
    assert!(Bounded::<i128>::MAX.wrapping_sub(-1) == Bounded::<i128>::MIN);
}

#[test]
fn test_wrapping_mul_unsigned_integers() {
    assert_eq!(2_u8.wrapping_mul(3), 6);
    assert_eq!(Bounded::<u8>::MAX.wrapping_mul(2), Bounded::<u8>::MAX - 1);
    assert_eq!(2_u16.wrapping_mul(3), 6);
    assert_eq!(Bounded::<u16>::MAX.wrapping_mul(2), Bounded::<u16>::MAX - 1);
    assert_eq!(2_u32.wrapping_mul(3), 6);
    assert_eq!(Bounded::<u32>::MAX.wrapping_mul(2), Bounded::<u32>::MAX - 1);
    assert_eq!(2_u64.wrapping_mul(3), 6);
    assert_eq!(Bounded::<u64>::MAX.wrapping_mul(2), Bounded::<u64>::MAX - 1);
    assert_eq!(2_u128.wrapping_mul(3), 6);
    assert_eq!(Bounded::<u128>::MAX.wrapping_mul(2), Bounded::<u128>::MAX - 1);
    assert_eq!(2_u256.wrapping_mul(3), 6);
    assert_eq!(Bounded::<u256>::MAX.wrapping_mul(2), Bounded::<u256>::MAX - 1);
}

// Checked tests
#[test]
fn test_checked_add_unsigned_integers() {
    assert_eq!(1_u8.checked_add(2), Some(3));
    assert!(Bounded::<u8>::MAX.checked_add(1).is_none());
    assert_eq!(1_u16.checked_add(2), Some(3));
    assert!(Bounded::<u16>::MAX.checked_add(1).is_none());
    assert_eq!(1_u32.checked_add(2), Some(3));
    assert!(Bounded::<u32>::MAX.checked_add(1).is_none());
    assert_eq!(1_u64.checked_add(2), Some(3));
    assert!(Bounded::<u64>::MAX.checked_add(1).is_none());
    assert_eq!(1_u128.checked_add(2), Some(3));
    assert!(Bounded::<u128>::MAX.checked_add(1).is_none());
    assert_eq!(1_u256.checked_add(2), Some(3));
    assert!(Bounded::<u256>::MAX.checked_add(1).is_none());
}

#[test]
fn test_checked_add_positive_signed_integers() {
    assert_eq!(1_i8.checked_add(2), Some(3));
    assert!(Bounded::<i8>::MAX.checked_add(1).is_none());
    assert_eq!(1_i16.checked_add(2), Some(3));
    assert!(Bounded::<i16>::MAX.checked_add(1).is_none());
    assert_eq!(1_i32.checked_add(2), Some(3));
    assert!(Bounded::<i32>::MAX.checked_add(1).is_none());
    assert_eq!(1_i64.checked_add(2), Some(3));
    assert!(Bounded::<i64>::MAX.checked_add(1).is_none());
    assert_eq!(1_i128.checked_add(2), Some(3));
    assert!(Bounded::<i128>::MAX.checked_add(1).is_none());
}

#[test]
fn test_checked_add_negative_signed_integers() {
    assert_eq!((-1_i8).checked_add(-2), Some(-3));
    assert!(Bounded::<i8>::MIN.checked_add(-1).is_none());
    assert_eq!((-1_i16).checked_add(-2), Some(-3));
    assert!(Bounded::<i16>::MIN.checked_add(-1).is_none());
    assert_eq!((-1_i32).checked_add(-2), Some(-3));
    assert!(Bounded::<i32>::MIN.checked_add(-1).is_none());
    assert_eq!((-1_i64).checked_add(-2), Some(-3));
    assert!(Bounded::<i64>::MIN.checked_add(-1).is_none());
    assert_eq!((-1_i128).checked_add(-2), Some(-3));
    assert!(Bounded::<i128>::MIN.checked_add(-1).is_none());
}

#[test]
fn test_checked_sub_unsigned_integers() {
    assert_eq!(3_u8.checked_sub(2), Some(1));
    assert!(0_u8.checked_sub(1).is_none());
    assert_eq!(3_u16.checked_sub(2), Some(1));
    assert!(0_u16.checked_sub(1).is_none());
    assert_eq!(3_u32.checked_sub(2), Some(1));
    assert!(0_u32.checked_sub(1).is_none());
    assert_eq!(3_u64.checked_sub(2), Some(1));
    assert!(0_u64.checked_sub(1).is_none());
    assert_eq!(3_u128.checked_sub(2), Some(1));
    assert!(0_u128.checked_sub(1).is_none());
    assert_eq!(3_u256.checked_sub(2), Some(1));
    assert!(0_u256.checked_sub(1).is_none());
}

#[test]
fn test_checked_sub_positive_signed_integers() {
    assert_eq!(3_i8.checked_sub(2), Some(1));
    assert!(Bounded::<i8>::MIN.checked_sub(1).is_none());
    assert_eq!(3_i16.checked_sub(2), Some(1));
    assert!(Bounded::<i16>::MIN.checked_sub(1).is_none());
    assert_eq!(3_i32.checked_sub(2), Some(1));
    assert!(Bounded::<i32>::MIN.checked_sub(1).is_none());
    assert_eq!(3_i64.checked_sub(2), Some(1));
    assert!(Bounded::<i64>::MIN.checked_sub(1).is_none());
    assert_eq!(3_i128.checked_sub(2), Some(1));
    assert!(Bounded::<i128>::MIN.checked_sub(1).is_none());
}

#[test]
fn test_checked_sub_negative_signed_integers() {
    assert_eq!((-3_i8).checked_sub(-2), Some(-1));
    assert!(Bounded::<i8>::MAX.checked_sub(-1).is_none());
    assert_eq!((-3_i16).checked_sub(-2), Some(-1));
    assert!(Bounded::<i16>::MAX.checked_sub(-1).is_none());
    assert_eq!((-3_i32).checked_sub(-2), Some(-1));
    assert!(Bounded::<i32>::MAX.checked_sub(-1).is_none());
    assert_eq!((-3_i64).checked_sub(-2), Some(-1));
    assert!(Bounded::<i64>::MAX.checked_sub(-1).is_none());
    assert_eq!((-3_i128).checked_sub(-2), Some(-1));
    assert!(Bounded::<i128>::MAX.checked_sub(-1).is_none());
}

#[test]
fn test_checked_mul_unsigned_integers() {
    assert_eq!(2_u8.checked_mul(3), Some(6));
    assert!(Bounded::<u8>::MAX.checked_mul(2).is_none());
    assert_eq!(2_u16.checked_mul(3), Some(6));
    assert!(Bounded::<u16>::MAX.checked_mul(2).is_none());
    assert_eq!(2_u32.checked_mul(3), Some(6));
    assert!(Bounded::<u32>::MAX.checked_mul(2).is_none());
    assert_eq!(2_u64.checked_mul(3), Some(6));
    assert!(Bounded::<u64>::MAX.checked_mul(2).is_none());
    assert_eq!(2_u128.checked_mul(3), Some(6));
    assert!(Bounded::<u128>::MAX.checked_mul(2).is_none());
    assert_eq!(2_u256.checked_mul(3), Some(6));
    assert!(Bounded::<u256>::MAX.checked_mul(2).is_none());
}

#[test]
fn test_saturating_add_unsigned_integers() {
    assert_eq!(1_u8.saturating_add(2), 3);
    assert_eq!(Bounded::<u8>::MAX.saturating_add(1), Bounded::<u8>::MAX);
    assert_eq!(1_u16.saturating_add(2), 3);
    assert_eq!(Bounded::<u16>::MAX.saturating_add(1), Bounded::<u16>::MAX);
    assert_eq!(1_u32.saturating_add(2), 3);
    assert_eq!(Bounded::<u32>::MAX.saturating_add(1), Bounded::<u32>::MAX);
    assert_eq!(1_u64.saturating_add(2), 3);
    assert_eq!(Bounded::<u64>::MAX.saturating_add(1), Bounded::<u64>::MAX);
    assert_eq!(1_u128.saturating_add(2), 3);
    assert_eq!(Bounded::<u128>::MAX.saturating_add(1), Bounded::<u128>::MAX);
    assert_eq!(1_u256.saturating_add(2), 3);
    assert_eq!(Bounded::<u256>::MAX.saturating_add(1), Bounded::<u256>::MAX);
}

#[test]
fn test_saturating_add_signed_integers() {
    assert_eq!(1_i8.saturating_add(2), 3);
    assert_eq!(Bounded::<i8>::MAX.saturating_add(1), Bounded::<i8>::MAX);
    assert_eq!(1_i16.saturating_add(2), 3);
    assert_eq!(Bounded::<i16>::MAX.saturating_add(1), Bounded::<i16>::MAX);
    assert_eq!(1_i32.saturating_add(2), 3);
    assert_eq!(Bounded::<i32>::MAX.saturating_add(1), Bounded::<i32>::MAX);
    assert_eq!(1_i64.saturating_add(2), 3);
    assert_eq!(Bounded::<i64>::MAX.saturating_add(1), Bounded::<i64>::MAX);
    assert_eq!(1_i128.saturating_add(2), 3);
    assert_eq!(Bounded::<i128>::MAX.saturating_add(1), Bounded::<i128>::MAX);
}

#[test]
fn test_saturating_add_signed_negative_integers() {
    assert_eq!(Bounded::<i8>::MIN.saturating_add(-1), Bounded::<i8>::MIN);
    assert_eq!((-1_i8).saturating_add(-2), -3);
    assert_eq!(Bounded::<i16>::MIN.saturating_add(-1), Bounded::<i16>::MIN);
    assert_eq!((-1_i16).saturating_add(-2), -3);
    assert_eq!(Bounded::<i32>::MIN.saturating_add(-1), Bounded::<i32>::MIN);
    assert_eq!((-1_i32).saturating_add(-2), -3);
    assert_eq!(Bounded::<i64>::MIN.saturating_add(-1), Bounded::<i64>::MIN);
    assert_eq!((-1_i64).saturating_add(-2), -3);
    assert_eq!(Bounded::<i128>::MIN.saturating_add(-1), Bounded::<i128>::MIN);
    assert_eq!((-1_i128).saturating_add(-2), -3);
}

#[test]
fn test_saturating_sub_unsigned_integers() {
    assert_eq!(3_u8.saturating_sub(2), 1);
    assert_eq!(0_u8.saturating_sub(1), 0);
    assert_eq!(3_u16.saturating_sub(2), 1);
    assert_eq!(0_u16.saturating_sub(1), 0);
    assert_eq!(3_u32.saturating_sub(2), 1);
    assert_eq!(0_u32.saturating_sub(1), 0);
    assert_eq!(3_u64.saturating_sub(2), 1);
    assert_eq!(0_u64.saturating_sub(1), 0);
    assert_eq!(3_u128.saturating_sub(2), 1);
    assert_eq!(0_u128.saturating_sub(1), 0);
    assert_eq!(3_u256.saturating_sub(2), 1);
    assert_eq!(0_u256.saturating_sub(1), 0);
}

#[test]
fn test_saturating_sub_signed_integers() {
    assert_eq!(3_i8.saturating_sub(2), 1);
    assert_eq!(Bounded::<i8>::MIN.saturating_sub(1), Bounded::<i8>::MIN);
    assert_eq!(3_i16.saturating_sub(2), 1);
    assert_eq!(Bounded::<i16>::MIN.saturating_sub(1), Bounded::<i16>::MIN);
    assert_eq!(3_i32.saturating_sub(2), 1);
    assert_eq!(Bounded::<i32>::MIN.saturating_sub(1), Bounded::<i32>::MIN);
    assert_eq!(3_i64.saturating_sub(2), 1);
    assert_eq!(Bounded::<i64>::MIN.saturating_sub(1), Bounded::<i64>::MIN);
    assert_eq!(3_i128.saturating_sub(2), 1);
    assert_eq!(Bounded::<i128>::MIN.saturating_sub(1), Bounded::<i128>::MIN);
}

#[test]
fn test_saturating_sub_signed_negative_integers() {
    assert_eq!(1_i8.saturating_sub(-2), 3);
    assert_eq!(Bounded::<i8>::MAX.saturating_sub(-1), Bounded::<i8>::MAX);
    assert_eq!(1_i16.saturating_sub(-2), 3);
    assert_eq!(Bounded::<i16>::MAX.saturating_sub(-1), Bounded::<i16>::MAX);
    assert_eq!(1_i32.saturating_sub(-2), 3);
    assert_eq!(Bounded::<i32>::MAX.saturating_sub(-1), Bounded::<i32>::MAX);
    assert_eq!(1_i64.saturating_sub(-2), 3);
    assert_eq!(Bounded::<i64>::MAX.saturating_sub(-1), Bounded::<i64>::MAX);
    assert_eq!(1_i128.saturating_sub(-2), 3);
    assert_eq!(Bounded::<i128>::MAX.saturating_sub(-1), Bounded::<i128>::MAX);
}

#[test]
fn test_saturating_mul_unsigned_integers() {
    assert_eq!(2_u8.saturating_mul(3), 6);
    assert_eq!(Bounded::<u8>::MAX.saturating_mul(2), Bounded::<u8>::MAX);
    assert_eq!(2_u16.saturating_mul(3), 6);
    assert_eq!(Bounded::<u16>::MAX.saturating_mul(2), Bounded::<u16>::MAX);
    assert_eq!(2_u32.saturating_mul(3), 6);
    assert_eq!(Bounded::<u32>::MAX.saturating_mul(2), Bounded::<u32>::MAX);
    assert_eq!(2_u64.saturating_mul(3), 6);
    assert_eq!(Bounded::<u64>::MAX.saturating_mul(2), Bounded::<u64>::MAX);
    assert_eq!(2_u128.saturating_mul(3), 6);
    assert_eq!(Bounded::<u128>::MAX.saturating_mul(2), Bounded::<u128>::MAX);
    assert_eq!(2_u256.saturating_mul(3), 6);
    assert_eq!(Bounded::<u256>::MAX.saturating_mul(2), Bounded::<u256>::MAX);
}

#[test]
fn test_pow() {
    assert_eq!((-2_i8).pow(0), 1);
    assert_eq!((-2_i8).pow(1), -2);
    assert_eq!((-2_i8).pow(2), 4);
    assert_eq!((-2_i8).pow(3), -8);
    assert_eq!((-2_i8).pow(4), 16);
    assert_eq!((-2_i8).pow(5), -32);
    assert_eq!((-2_i8).pow(6), 64);

    assert_eq!(0.pow(0), 1);
    assert_eq!(0.pow(1), 0);
    assert_eq!(0.pow(2), 0);

    assert_eq!(2.pow(0), 0b1);
    assert_eq!(2.pow(1), 0b10);
    assert_eq!(2.pow(2), 0b100);
    assert_eq!(2.pow(3), 0b1000);
    assert_eq!(2.pow(4), 0b10000);
    assert_eq!(2.pow(5), 0b100000);
    assert_eq!(2.pow(6), 0b1000000);
    assert_eq!(2.pow(7), 0b10000000);
    assert_eq!(2.pow(8), 0b100000000);
    assert_eq!(2.pow(9), 0b1000000000);
    assert_eq!(2.pow(10), 0b10000000000);
}
