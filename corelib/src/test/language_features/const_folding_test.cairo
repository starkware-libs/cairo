use crate::integer::i8_diff;
use crate::num::traits::{WrappingAdd, WrappingMul, WrappingSub};

/// Helper to prevent const folding.
#[inline(never)]
fn noop<T>(t: T) -> T {
    t
}


#[test]
fn test_add() {
    assert!(WrappingAdd::wrapping_add(1_u8, 2) == WrappingAdd::wrapping_add(noop(1), noop(2)));
    assert!(
        WrappingAdd::wrapping_add(100_u8, 200) == WrappingAdd::wrapping_add(noop(100), noop(200)),
    );
    assert!(WrappingAdd::wrapping_add(1_i8, 2) == WrappingAdd::wrapping_add(noop(1), noop(2)));
    assert!(
        WrappingAdd::wrapping_add(100_i8, 50) == WrappingAdd::wrapping_add(noop(100), noop(50)),
    );
    assert!(
        WrappingAdd::wrapping_add(-100_i8, -50) == WrappingAdd::wrapping_add(noop(-100), noop(-50)),
    );
}

#[test]
fn test_sub() {
    assert!(WrappingSub::wrapping_sub(1_u8, 2) == WrappingSub::wrapping_sub(noop(1), noop(2)));
    assert!(WrappingSub::wrapping_sub(2_u8, 1) == WrappingSub::wrapping_sub(noop(2), noop(1)));
    assert!(WrappingSub::wrapping_sub(1_i8, 2) == WrappingSub::wrapping_sub(noop(1), noop(2)));
    assert!(
        WrappingSub::wrapping_sub(100_i8, -50) == WrappingSub::wrapping_sub(noop(100), noop(-50)),
    );
    assert!(
        WrappingSub::wrapping_sub(-100_i8, 50) == WrappingSub::wrapping_sub(noop(-100), noop(50)),
    );
}

#[test]
fn test_diff() {
    assert!(i8_diff(1, 2) == i8_diff(noop(1), noop(2)));
    assert!(i8_diff(2, 1) == i8_diff(noop(2), noop(1)));
}

#[test]
fn test_mul() {
    assert!(WrappingMul::wrapping_mul(1_u8, 2) == WrappingMul::wrapping_mul(noop(1), noop(2)));
    assert!(
        WrappingMul::wrapping_mul(100_u8, 200) == WrappingMul::wrapping_mul(noop(100), noop(200)),
    );
}
