/// Performs addition with a flag for overflow.
pub trait OverflowingAdd<T> {
    /// Returns a tuple of the sum along with a boolean indicating whether an arithmetic overflow
    /// would occur.
    /// If an overflow would have occurred then the wrapped value is returned.
    fn overflowing_add(self: T, v: T) -> (T, bool);
}
