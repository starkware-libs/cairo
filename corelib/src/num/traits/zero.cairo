/// Defines an additive identity element for `T`.
pub trait Zero<T> {
    /// Returns the additive identity element of `T`, `0`.
    fn zero() -> T;
    /// Returns `true` if `self` is equal to the additive identity.
    fn is_zero(self: @T) -> bool;
    /// Returns `false` if `self` is equal to the additive identity.
    fn is_non_zero(self: @T) -> bool;
}
