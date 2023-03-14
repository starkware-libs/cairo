trait Zeroable<T> {
    /// Returns the additive identity element of Self, 0.
    fn zero() -> T;
    /// Returns whether self is equal to 0, the additive identity element.
    fn is_zero(self: T) -> bool;
    /// Returns whether self is not equal to 0, the additive identity element.
    fn is_non_zero(self: T) -> bool;
}

impl Felt252Zeroable of Zeroable::<felt252> {
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
