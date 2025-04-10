use crate::integer::SignedIntegerResult;

/// Helper trait for calling the libfuncs per signed int type.
trait SignedIntegerHelper<T> {
    /// The wrapper for the addition libfunc.
    fn add(lhs: T, rhs: T) -> SignedIntegerResult<T> nopanic;

    /// The wrapper for the subtraction libfunc.
    fn sub(lhs: T, rhs: T) -> SignedIntegerResult<T> nopanic;
}

impl SignedIntegerHelperI8 of SignedIntegerHelper<i8> {
    fn add(lhs: i8, rhs: i8) -> SignedIntegerResult<i8> nopanic {
        crate::integer::i8_overflowing_add_impl(lhs, rhs)
    }

    fn sub(lhs: i8, rhs: i8) -> SignedIntegerResult<i8> nopanic {
        crate::integer::i8_overflowing_sub_impl(lhs, rhs)
    }
}

impl SignedIntegerHelperI16 of SignedIntegerHelper<i16> {
    fn add(lhs: i16, rhs: i16) -> SignedIntegerResult<i16> nopanic {
        crate::integer::i16_overflowing_add_impl(lhs, rhs)
    }

    fn sub(lhs: i16, rhs: i16) -> SignedIntegerResult<i16> nopanic {
        crate::integer::i16_overflowing_sub_impl(lhs, rhs)
    }
}

impl SignedIntegerHelperI32 of SignedIntegerHelper<i32> {
    fn add(lhs: i32, rhs: i32) -> SignedIntegerResult<i32> nopanic {
        crate::integer::i32_overflowing_add_impl(lhs, rhs)
    }

    fn sub(lhs: i32, rhs: i32) -> SignedIntegerResult<i32> nopanic {
        crate::integer::i32_overflowing_sub_impl(lhs, rhs)
    }
}

impl SignedIntegerHelperI64 of SignedIntegerHelper<i64> {
    fn add(lhs: i64, rhs: i64) -> SignedIntegerResult<i64> nopanic {
        crate::integer::i64_overflowing_add_impl(lhs, rhs)
    }

    fn sub(lhs: i64, rhs: i64) -> SignedIntegerResult<i64> nopanic {
        crate::integer::i64_overflowing_sub_impl(lhs, rhs)
    }
}

impl SignedIntegerHelperI128 of SignedIntegerHelper<i128> {
    fn add(lhs: i128, rhs: i128) -> SignedIntegerResult<i128> nopanic {
        crate::integer::i128_overflowing_add_impl(lhs, rhs)
    }

    fn sub(lhs: i128, rhs: i128) -> SignedIntegerResult<i128> nopanic {
        crate::integer::i128_overflowing_sub_impl(lhs, rhs)
    }
}

/// Impl for `CheckedAdd` based on `SignedIntegerHelper`.
pub impl CheckedAddImpl<
    T, impl H: SignedIntegerHelper<T>, +Drop<T>,
> of crate::num::traits::CheckedAdd<T> {
    fn checked_add(self: T, v: T) -> Option<T> {
        as_checked(H::add(self, v))
    }
}

/// Impl for `CheckedSub` based on `SignedIntegerHelper`.
pub impl CheckedSubImpl<
    T, impl H: SignedIntegerHelper<T>, +Drop<T>,
> of crate::num::traits::CheckedSub<T> {
    fn checked_sub(self: T, v: T) -> Option<T> {
        as_checked(H::sub(self, v))
    }
}

/// Converts `SignedIntegerResult` to an `Some` if in range and to `None`
/// otherwise.
fn as_checked<T, +Drop<T>>(result: SignedIntegerResult<T>) -> Option<T> {
    match result {
        SignedIntegerResult::InRange(result) => Some(result),
        SignedIntegerResult::Underflow(_) | SignedIntegerResult::Overflow(_) => None,
    }
}

/// Impl for `SaturatingAdd` based on `SignedIntegerHelper`.
pub impl SaturatingAddImpl<
    T, impl H: SignedIntegerHelper<T>, +Drop<T>, +crate::num::traits::Bounded<T>,
> of crate::num::traits::SaturatingAdd<T> {
    fn saturating_add(self: T, other: T) -> T {
        as_saturating(H::add(self, other))
    }
}

/// Impl for `SaturatingSub` based on `SignedIntegerHelper`.
pub impl SaturatingSubImpl<
    T, impl H: SignedIntegerHelper<T>, +Drop<T>, +crate::num::traits::Bounded<T>,
> of crate::num::traits::SaturatingSub<T> {
    fn saturating_sub(self: T, other: T) -> T {
        as_saturating(H::sub(self, other))
    }
}

/// Converts `SignedIntegerResult` to a saturated value.
fn as_saturating<T, +Drop<T>, impl B: crate::num::traits::Bounded<T>>(
    result: SignedIntegerResult<T>,
) -> T {
    match result {
        SignedIntegerResult::InRange(result) => result,
        SignedIntegerResult::Underflow(_) => B::MIN,
        SignedIntegerResult::Overflow(_) => B::MAX,
    }
}

/// Impl for `OverflowingAdd` based on `SignedIntegerHelper`.
pub impl OverflowingAddImpl<
    T, impl H: SignedIntegerHelper<T>, +Drop<T>,
> of crate::num::traits::OverflowingAdd<T> {
    fn overflowing_add(self: T, v: T) -> (T, bool) {
        as_overflowing(H::add(self, v))
    }
}

/// Impl for `OverflowingSub` based on `SignedIntegerHelper`.
pub impl OverflowingSubImpl<
    T, impl H: SignedIntegerHelper<T>, +Drop<T>,
> of crate::num::traits::OverflowingSub<T> {
    fn overflowing_sub(self: T, v: T) -> (T, bool) {
        as_overflowing(H::sub(self, v))
    }
}

/// Converts `SignedIntegerResult` to a tuple of the result and a boolean indicating overflow.
fn as_overflowing<T>(result: SignedIntegerResult<T>) -> (T, bool) {
    match result {
        SignedIntegerResult::InRange(result) => (result, false),
        SignedIntegerResult::Underflow(result) |
        SignedIntegerResult::Overflow(result) => (result, true),
    }
}

/// Impl for `WrappingAdd` based on `SignedIntegerHelper`.
pub impl WrappingAddImpl<
    T, impl H: SignedIntegerHelper<T>, +Drop<T>,
> of crate::num::traits::WrappingAdd<T> {
    fn wrapping_add(self: T, v: T) -> T {
        as_wrapping(H::add(self, v))
    }
}

/// Impl for `WrappingSub` based on `SignedIntegerHelper`.
pub impl WrappingSubImpl<
    T, impl H: SignedIntegerHelper<T>, +Drop<T>,
> of crate::num::traits::WrappingSub<T> {
    fn wrapping_sub(self: T, v: T) -> T {
        as_wrapping(H::sub(self, v))
    }
}

/// Converts `SignedIntegerResult` to a wrapping value.
fn as_wrapping<T>(result: SignedIntegerResult<T>) -> T {
    match result {
        SignedIntegerResult::InRange(result) | SignedIntegerResult::Underflow(result) |
        SignedIntegerResult::Overflow(result) => result,
    }
}
