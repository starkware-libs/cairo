use core::num::traits::BitSize;
use core::num::traits::OverflowingAdd;
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
