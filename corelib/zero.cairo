trait Zero<T> {
    /// Returns the additive identity element of Self, 0.
    fn zero() -> T;
    /// Returns true if self is equal to the additive identity.
    fn is_zero(self: T) -> bool;
}

impl FeltZero of Zero::<felt> {
    /// Returns the additive identity element of Self, 0.
    fn zero() -> felt {
        0
    }

    /// Returns true if self is equal to the additive identity.
    #[inline(always)]
    fn is_zero(self: felt) -> bool {
        self == 0
    }
}
