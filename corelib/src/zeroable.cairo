// === Zeroable ===

trait Zeroable<T> {
    /// Returns the additive identity element of Self, 0.
    fn zero() -> T;
    /// Returns whether self is equal to 0, the additive identity element.
    fn is_zero(self: T) -> bool;
    /// Returns whether self is not equal to 0, the additive identity element.
    fn is_non_zero(self: T) -> bool;
}

impl Felt252Zeroable of Zeroable<felt252> {
    fn zero() -> felt252 {
        0
    }
    #[inline(always)]
    fn is_zero(self: felt252) -> bool {
        self == 0
    }
    #[inline(always)]
    fn is_non_zero(self: felt252) -> bool {
        !self.is_zero()
    }
}

// === NonZero ===

#[derive(Copy, Drop)]
extern type NonZero<T>;

enum IsZeroResult<T> {
    Zero,
    NonZero: NonZero<T>,
}
extern fn unwrap_non_zero<T>(a: NonZero<T>) -> T nopanic;

impl NonZeroIntoImpl<T> of Into<NonZero<T>, T> {
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
