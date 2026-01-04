use core::num::traits::ops::overflowing::{OverflowingAdd, OverflowingMul, OverflowingSub};

pub fn is_overflow_add<T, +OverflowingAdd<T>, +Drop<T>>(x: T, y: T) -> bool {
    let (_, does_overflow) = x.overflowing_add(y);
    does_overflow
}

pub fn is_overflow_mul<T, +OverflowingMul<T>, +Drop<T>>(x: T, y: T) -> bool {
    let (_, does_overflow) = x.overflowing_mul(y);
    does_overflow
}

pub fn is_overflow_sub<T, +OverflowingSub<T>, +Drop<T>>(x: T, y: T) -> bool {
    let (_, does_overflow) = x.overflowing_sub(y);
    does_overflow
}
