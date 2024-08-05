// === Zeroable ===

/// A trait for types that have a concept of zero and can be compared to zero.
///
/// This trait is useful for numeric types or any type that has an additive identity element.
pub(crate) trait Zeroable<T> {
    /// Returns the additive identity element of Self, 0.
    ///
    /// This method should return a value that, when added to any other value of type T,
    /// does not change that value.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Zeroable::<i32>::zero(), 0);
    /// ```
    #[must_use]
    fn zero() -> T;

    /// Returns whether self is equal to 0, the additive identity element.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(0.is_zero());
    /// assert!(!5.is_zero());
    /// ```
    #[must_use]
    fn is_zero(self: T) -> bool;

    /// Returns whether self is not equal to 0, the additive identity element.
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
        T, impl ZeroImpl: core::num::traits::Zero<T>, +Drop<T>, +Copy<T>
    > of super::Zeroable<T> {
        /// Returns the zero value for the type.
        fn zero() -> T {
            ZeroImpl::zero()
        }

        /// Checks if the value is zero.
        #[inline(always)]
        fn is_zero(self: T) -> bool {
            ZeroImpl::is_zero(@self)
        }

        /// Checks if the value is non-zero.
        #[inline(always)]
        fn is_non_zero(self: T) -> bool {
            ZeroImpl::is_non_zero(@self)
        }
    }
}

pub(crate) impl Felt252Zeroable = zero_based::ZeroableImpl<felt252>;

// === NonZero ===

/// A wrapper type for non-zero values of type T.
///
/// This type guarantees that the wrapped value is never zero.
#[derive(Copy, Drop)]
pub extern type NonZero<T>;

/// Represents the result of checking whether a value is zero.
pub(crate) enum IsZeroResult<T> {
    /// Indicates that the value is zero.
    Zero,
    /// Indicates that the value is non-zero, wrapping it in a NonZero<T>.
    NonZero: NonZero<T>,
}

/// Unwraps a NonZero<T> to retrieve the underlying value of type T.
extern fn unwrap_non_zero<T>(a: NonZero<T>) -> T nopanic;

/// Implements the `Into` trait for converting NonZero<T> to T.
pub(crate) impl NonZeroIntoImpl<T> of Into<NonZero<T>, T> {
    /// Converts a NonZero<T> to T.
    fn into(self: NonZero<T>) -> T nopanic {
        unwrap_non_zero(self)
    }
}

/// Implements the `Into` trait for converting IsZeroResult<T> to bool.
impl IsZeroResultIntoBool<T, +Drop<T>> of Into<IsZeroResult<T>, bool> {
    /// Converts an IsZeroResult<T> to a boolean.
    ///
    /// Returns true if the result is Zero, false otherwise.
    fn into(self: IsZeroResult<T>) -> bool {
        match self {
            IsZeroResult::Zero => true,
            IsZeroResult::NonZero(_) => false,
        }
    }
}

/// Implements the `PartialEq` trait for NonZero<T>.
impl NonZeroPartialEq<T, +PartialEq<T>, +Copy<T>, +Drop<T>> of PartialEq<NonZero<T>> {
    /// Checks if two NonZero<T> values are equal.
    #[inline(always)]
    fn eq(lhs: @NonZero<T>, rhs: @NonZero<T>) -> bool {
        let lhs: T = (*lhs).into();
        let rhs: T = (*rhs).into();
        lhs == rhs
    }

    /// Checks if two NonZero<T> values are not equal.
    #[inline(always)]
    fn ne(lhs: @NonZero<T>, rhs: @NonZero<T>) -> bool {
        let lhs: T = (*lhs).into();
        let rhs: T = (*rhs).into();
        lhs != rhs
    }
}

/// Implements the `Serde` trait for NonZero<T>.
impl NonZeroSerde<T, +Serde<T>, +Copy<T>, +Drop<T>, +TryInto<T, NonZero<T>>> of Serde<NonZero<T>> {
    /// Serializes a NonZero<T> value.
    fn serialize(self: @NonZero<T>, ref output: Array<felt252>) {
        let value: T = (*self).into();
        value.serialize(ref output);
    }

    /// Deserializes a NonZero<T> value.
    ///
    /// Returns None if deserialization fails or if the deserialized value is zero.
    fn deserialize(ref serialized: Span<felt252>) -> Option<NonZero<T>> {
        Serde::<T>::deserialize(ref serialized)?.try_into()
    }
}
