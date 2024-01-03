/// Trait used to increment a number.
pub trait Inc<T> {
    /// Returns the number incremented by 1 if an overflow did not occur.
    fn inc(self: T) -> Option<T>;
}
