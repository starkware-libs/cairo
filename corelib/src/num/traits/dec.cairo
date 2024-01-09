/// Trait used to increment a number.
pub trait Dec<T> {
    /// Returns the number decremented by 1 if an overflow did not occur.
    fn dec(self: T) -> Option<T>;
}
