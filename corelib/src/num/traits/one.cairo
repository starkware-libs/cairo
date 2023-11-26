/// Defines a multiplicative identity element for `T`.
pub trait One<T> {
    /// Returns the multiplicative identity element of `T`, `1`.
    fn one() -> T;
    /// Returns `true` if `self` is equal to the multiplicative identity.
    fn is_one(self: @T) -> bool;
    /// Returns `false` if `self` is equal to the multiplicative identity.
    fn is_non_one(self: @T) -> bool;
}
