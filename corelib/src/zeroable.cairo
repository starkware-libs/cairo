//! Types and traits for handling non-zero values and zero checking operations.
//!
//! This module provides the [`NonZero`] wrapper type which guarantees that a value is never
//! zero.
//! The [`Zeroable`] trait is meant for internal use only. The public-facing equivalent is the
//! [`Zero`] trait.
//!
//! [`Zero`]: core::num::traits::zero::Zero

/// A trait for types that have a concept of zero and can be compared to zero.
///
/// This trait is useful for numeric types or any type that has an additive identity element.
pub(crate) trait Zeroable<T> {
    /// Returns the additive identity element of `self`, 0.
    ///
    /// This method should return a value that, when added to any other value of type `T`,
    /// does not change that value.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(Zeroable::<i32>::zero() == 0);
    /// ```
    #[must_use]
    fn zero() -> T;

    /// Returns whether `self` is equal to 0, the additive identity element.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(0.is_zero());
    /// assert!(!5.is_zero());
    /// ```
    #[must_use]
    fn is_zero(self: T) -> bool;

    /// Returns whether `self` is not equal to 0, the additive identity element.
    ///
    /// This method is the logical inverse of `is_zero()`.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(5.is_non_zero());
    /// assert!(!0.is_non_zero());
    /// ```
    #[must_use]
    fn is_non_zero(self: T) -> bool;
}

/// Provides an implementation of the `Zeroable` trait for types that implement `Zero`.
pub(crate) mod zero_based {
    /// Implements `Zeroable` for any type that implements `Zero`, `Drop`, and `Copy`.
    pub(crate) impl ZeroableImpl<
        T, impl ZeroImpl: crate::num::traits::Zero<T>, +Drop<T>, +Copy<T>,
    > of super::Zeroable<T> {
        fn zero() -> T {
            ZeroImpl::zero()
        }

        #[inline]
        fn is_zero(self: T) -> bool {
            ZeroImpl::is_zero(@self)
        }

        #[inline]
        fn is_non_zero(self: T) -> bool {
            ZeroImpl::is_non_zero(@self)
        }
    }
}

pub(crate) impl Felt252Zeroable = zero_based::ZeroableImpl<felt252>;


/// A wrapper type for non-zero values of type T.
///
/// This type guarantees that the wrapped value is never zero.
pub extern type NonZero<T>;

impl NonZeroCopy<T> of Copy<NonZero<T>>;
impl NonZeroDrop<T> of Drop<NonZero<T>>;

pub(crate) mod non_zero_neg {
    // Generic implementation for types that don't support bounded integer optimization
    pub impl Impl<T, +Neg<T>, +TryInto<T, NonZero<T>>> of Neg<NonZero<T>> {
        fn neg(a: NonZero<T>) -> NonZero<T> {
            let value: T = a.into();
            let negated: T = -value;
            negated.try_into().unwrap()
        }
    }

    // Optimized implementation for signed integer types using bounded integers
    // This uses NegateHelper directly on bounded integers, which is more efficient
    // than the generic implementation that goes through Neg<T>
    #[feature("bounded-int-utils")]
    mod optimized_signed {
        use crate::internal::bounded_int::{self, upcast};

        pub impl I8Impl of super::super::Neg<super::super::NonZero<i8>> {
            fn neg(a: super::super::NonZero<i8>) -> super::super::NonZero<i8> {
                let value: i8 = a.into();
                // Use bounded integer negation directly, which is optimized
                let core::internal::OptionRev::Some(bounded) = bounded_int::trim_min(value) else {
                    // Minimum value case: use standard negation (will panic, but consistent with Neg<i8>)
                    return (-value).try_into().unwrap();
                };
                let negated = bounded_int::NegateHelper::negate(bounded);
                upcast(negated).try_into().unwrap()
            }
        }

        pub impl I16Impl of super::super::Neg<super::super::NonZero<i16>> {
            fn neg(a: super::super::NonZero<i16>) -> super::super::NonZero<i16> {
                let value: i16 = a.into();
                let core::internal::OptionRev::Some(bounded) = bounded_int::trim_min(value) else {
                    return (-value).try_into().unwrap();
                };
                let negated = bounded_int::NegateHelper::negate(bounded);
                upcast(negated).try_into().unwrap()
            }
        }

        pub impl I32Impl of super::super::Neg<super::super::NonZero<i32>> {
            fn neg(a: super::super::NonZero<i32>) -> super::super::NonZero<i32> {
                let value: i32 = a.into();
                let core::internal::OptionRev::Some(bounded) = bounded_int::trim_min(value) else {
                    return (-value).try_into().unwrap();
                };
                let negated = bounded_int::NegateHelper::negate(bounded);
                upcast(negated).try_into().unwrap()
            }
        }

        pub impl I64Impl of super::super::Neg<super::super::NonZero<i64>> {
            fn neg(a: super::super::NonZero<i64>) -> super::super::NonZero<i64> {
                let value: i64 = a.into();
                let core::internal::OptionRev::Some(bounded) = bounded_int::trim_min(value) else {
                    return (-value).try_into().unwrap();
                };
                let negated = bounded_int::NegateHelper::negate(bounded);
                upcast(negated).try_into().unwrap()
            }
        }

        pub impl I128Impl of super::super::Neg<super::super::NonZero<i128>> {
            fn neg(a: super::super::NonZero<i128>) -> super::super::NonZero<i128> {
                let value: i128 = a.into();
                let core::internal::OptionRev::Some(bounded) = bounded_int::trim_min(value) else {
                    return (-value).try_into().unwrap();
                };
                let negated = bounded_int::NegateHelper::negate(bounded);
                upcast(negated).try_into().unwrap()
            }
        }
    }
}

// Use optimized implementation for signed integer types
#[feature("bounded-int-utils")]
impl NonZeroI8Neg = non_zero_neg::optimized_signed::I8Impl;
#[feature("bounded-int-utils")]
impl NonZeroI16Neg = non_zero_neg::optimized_signed::I16Impl;
#[feature("bounded-int-utils")]
impl NonZeroI32Neg = non_zero_neg::optimized_signed::I32Impl;
#[feature("bounded-int-utils")]
impl NonZeroI64Neg = non_zero_neg::optimized_signed::I64Impl;
#[feature("bounded-int-utils")]
impl NonZeroI128Neg = non_zero_neg::optimized_signed::I128Impl;
// Use generic implementation for felt252 (no bounded integer optimization available)
impl NonZeroFelt252Neg = non_zero_neg::Impl<felt252>;

/// Represents the result of checking whether a value is zero.
pub(crate) enum IsZeroResult<T> {
    /// Indicates that the value is zero.
    Zero,
    /// Indicates that the value is non-zero, wrapping it in a `NonZero<T>`.
    NonZero: NonZero<T>,
}

/// Unwraps a `NonZero<T>` to retrieve the underlying value of type `T`.
extern const fn unwrap_non_zero<T>(a: NonZero<T>) -> T nopanic;

pub(crate) impl NonZeroIntoImpl<T> of Into<NonZero<T>, T> {
    const fn into(self: NonZero<T>) -> T nopanic {
        unwrap_non_zero(self)
    }
}

impl IsZeroResultIntoBool<T, +Drop<T>> of Into<IsZeroResult<T>, bool> {
    fn into(self: IsZeroResult<T>) -> bool {
        match self {
            IsZeroResult::Zero => true,
            IsZeroResult::NonZero(_) => false,
        }
    }
}

impl NonZeroPartialEq<T, +PartialEq<T>, +Copy<T>, +Drop<T>> of PartialEq<NonZero<T>> {
    #[inline]
    fn eq(lhs: @NonZero<T>, rhs: @NonZero<T>) -> bool {
        let lhs: T = (*lhs).into();
        let rhs: T = (*rhs).into();
        lhs == rhs
    }

    #[inline]
    fn ne(lhs: @NonZero<T>, rhs: @NonZero<T>) -> bool {
        let lhs: T = (*lhs).into();
        let rhs: T = (*rhs).into();
        lhs != rhs
    }
}

impl NonZeroSerde<T, +Serde<T>, +Copy<T>, +Drop<T>, +TryInto<T, NonZero<T>>> of Serde<NonZero<T>> {
    fn serialize(self: @NonZero<T>, ref output: Array<felt252>) {
        let value: T = (*self).into();
        value.serialize(ref output);
    }

    fn deserialize(ref serialized: Span<felt252>) -> Option<NonZero<T>> {
        Serde::<T>::deserialize(ref serialized)?.try_into()
    }
}
