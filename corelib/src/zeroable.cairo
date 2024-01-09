// === Zeroable ===

pub(crate) trait Zeroable<T> {
    /// Returns the additive identity element of Self, 0.
    #[must_use]
    fn zero() -> T;
    /// Returns whether self is equal to 0, the additive identity element.
    #[must_use]
    fn is_zero(self: T) -> bool;
    /// Returns whether self is not equal to 0, the additive identity element.
    #[must_use]
    fn is_non_zero(self: T) -> bool;
}

pub(crate) mod zero_based {
    pub(crate) impl ZeroableImpl<
        T, impl ZeroImpl: core::num::traits::Zero<T>, +Drop<T>, +Copy<T>
    > of super::Zeroable<T> {
        fn zero() -> T {
            ZeroImpl::zero()
        }
        #[inline(always)]
        fn is_zero(self: T) -> bool {
            ZeroImpl::is_zero(@self)
        }
        #[inline(always)]
        fn is_non_zero(self: T) -> bool {
            ZeroImpl::is_non_zero(@self)
        }
    }
}

pub(crate) impl Felt252Zeroable = zero_based::ZeroableImpl<felt252>;

// === NonZero ===

#[derive(Copy, Drop)]
pub extern type NonZero<T>;

pub(crate) enum IsZeroResult<T> {
    Zero,
    NonZero: NonZero<T>,
}
extern fn unwrap_non_zero<T>(a: NonZero<T>) -> T nopanic;

pub(crate) impl NonZeroIntoImpl<T> of Into<NonZero<T>, T> {
    fn into(self: NonZero<T>) -> T nopanic {
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
