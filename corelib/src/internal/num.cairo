use crate::integer::SignedIntegerResult;
use crate::internal::OptionRev;
#[feature("bounded-int-utils")]
use crate::internal::bounded_int::{
    AddHelper, SubHelper, TrimMaxHelper, TrimMinHelper, UnitInt, add, sub, trim_max, trim_min,
    upcast,
};

/// Returns `Ok(t + 1)`, or `Err(core::num::traits::Bounded<T>::MIN)` if out of range.
///
/// Designed to be equivalent to `*_overflowing_add(t, 1)`.
fn uint_inc<
    T,
    impl Bounded: crate::num::traits::Bounded<T>,
    impl Trim: TrimMaxHelper<T>,
    impl Add: AddHelper<Trim::Target, UnitInt<1>>,
>(
    t: T,
) -> Result<T, T> {
    if let OptionRev::Some(trimmed) = trim_max(t) {
        Ok(upcast(add::<_, _, Add>(trimmed, 1)))
    } else {
        Err(Bounded::MIN)
    }
}

/// Returns `Ok(t - 1)`, or `Err(core::num::traits::Bounded<T>::MAX)` if out of range.
///
/// Designed to be equivalent to `*_overflowing_sub(t, 1)`.
fn uint_dec<
    T,
    impl Bounded: crate::num::traits::Bounded<T>,
    impl Trim: TrimMinHelper<T>,
    impl Sub: SubHelper<Trim::Target, UnitInt<1>>,
>(
    t: T,
) -> Result<T, T> {
    if let OptionRev::Some(trimmed) = trim_min(t) {
        Ok(upcast(sub::<_, _, Sub>(trimmed, 1)))
    } else {
        Err(Bounded::MAX)
    }
}

fn u8_inc(value: u8) -> Result<u8, u8> {
    uint_inc(value)
}

fn u8_dec(value: u8) -> Result<u8, u8> {
    uint_dec(value)
}

fn u16_inc(value: u16) -> Result<u16, u16> {
    uint_inc(value)
}

fn u16_dec(value: u16) -> Result<u16, u16> {
    uint_dec(value)
}

fn u32_inc(value: u32) -> Result<u32, u32> {
    uint_inc(value)
}

fn u32_dec(value: u32) -> Result<u32, u32> {
    uint_dec(value)
}

fn u64_inc(value: u64) -> Result<u64, u64> {
    uint_inc(value)
}

fn u64_dec(value: u64) -> Result<u64, u64> {
    uint_dec(value)
}

fn u128_inc(value: u128) -> Result<u128, u128> {
    uint_inc(value)
}

fn u128_dec(value: u128) -> Result<u128, u128> {
    uint_dec(value)
}

/// Returns `SignedIntegerResult::InRange(t + 1)`, or
/// `SignedIntegerResult::Overflow(core::num::traits::Bounded<T>::MIN)` if out of range.
///
/// Designed to be equivalent to `*_overflowing_add_impl(t, 1)`.
fn sint_inc<
    T,
    impl Bounded: crate::num::traits::Bounded<T>,
    impl Trim: TrimMaxHelper<T>,
    impl Add: AddHelper<Trim::Target, UnitInt<1>>,
>(
    t: T,
) -> SignedIntegerResult<T> {
    if let OptionRev::Some(trimmed) = trim_max(t) {
        SignedIntegerResult::InRange(upcast(add::<_, _, Add>(trimmed, 1)))
    } else {
        SignedIntegerResult::Overflow(Bounded::MIN)
    }
}

/// Returns `SignedIntegerResult::InRange(t - 1)`, or
/// `SignedIntegerResult::Underflow(core::num::traits::Bounded<T>::MAX)` if out of range.
///
/// Designed to be equivalent to `*_overflowing_sub_impl(t, 1)`.
fn sint_dec<
    T,
    impl Bounded: crate::num::traits::Bounded<T>,
    impl Trim: TrimMinHelper<T>,
    impl Sub: SubHelper<Trim::Target, UnitInt<1>>,
>(
    t: T,
) -> SignedIntegerResult<T> {
    if let OptionRev::Some(trimmed) = trim_min(t) {
        SignedIntegerResult::InRange(upcast(sub::<_, _, Sub>(trimmed, 1)))
    } else {
        SignedIntegerResult::Underflow(Bounded::MAX)
    }
}

fn i8_inc(value: i8) -> SignedIntegerResult<i8> {
    sint_inc(value)
}

fn i8_dec(value: i8) -> SignedIntegerResult<i8> {
    sint_dec(value)
}

fn i16_inc(value: i16) -> SignedIntegerResult<i16> {
    sint_inc(value)
}

fn i16_dec(value: i16) -> SignedIntegerResult<i16> {
    sint_dec(value)
}

fn i32_inc(value: i32) -> SignedIntegerResult<i32> {
    sint_inc(value)
}

fn i32_dec(value: i32) -> SignedIntegerResult<i32> {
    sint_dec(value)
}

fn i64_inc(value: i64) -> SignedIntegerResult<i64> {
    sint_inc(value)
}

fn i64_dec(value: i64) -> SignedIntegerResult<i64> {
    sint_dec(value)
}

fn i128_inc(value: i128) -> SignedIntegerResult<i128> {
    sint_inc(value)
}

fn i128_dec(value: i128) -> SignedIntegerResult<i128> {
    sint_dec(value)
}
