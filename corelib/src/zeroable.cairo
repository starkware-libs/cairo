trait Zeroable<T> {
    /// Returns the additive identity element of Self, 0.
    fn zero() -> T;
    /// Returns whether self is equal to 0, the additive identity element.
    fn is_zero(self: T) -> bool;
}

impl FeltZeroable of Zeroable::<felt> {
    fn zero() -> felt {
        0
    }

    #[inline(always)]
    fn is_zero(self: felt) -> bool {
        self == 0
    }
}
