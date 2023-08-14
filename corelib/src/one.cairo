// === One ===

trait One<T> {
    /// Returns the multiplicative identity element of Self, 1.
    fn one() -> T;
    /// Returns whether self is equal to 1, the multiplicative identity element.
    fn is_one(self: T) -> bool;
    /// Returns whether self is not equal to 1, the multiplicative identity element.
    fn is_non_one(self: T) -> bool;
}

impl Felt252One of One<felt252> {
    fn one() -> felt252 {
        1
    }
    #[inline(always)]
    fn is_one(self: felt252) -> bool {
        self == 1
    }
    #[inline(always)]
    fn is_non_one(self: felt252) -> bool {
        !self.is_one()
    }
}
