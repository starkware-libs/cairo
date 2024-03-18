/// Performs addition with a flag for overflow.
pub trait OverflowingAdd<T> {
    /// Returns a tuple of the sum along with a boolean indicating whether an arithmetic overflow
    /// would occur.
    /// If an overflow would have occurred then the wrapped value is returned.
    fn overflowing_add(self: T, v: T) -> (T, bool);
}

/// Performs subtraction with a flag for overflow.
pub trait OverflowingSub<T> {
    /// Returns a tuple of the difference along with a boolean indicating whether an arithmetic
    /// overflow would occur.
    /// If an overflow would have occurred then the wrapped value is returned.
    fn overflowing_sub(self: T, v: T) -> (T, bool);
}

/// Performs multiplication with a flag for overflow.
pub trait OverflowingMul<T> {
    /// Returns a tuple of the product along with a boolean indicating whether an arithmetic
    /// overflow would occur.
    /// If an overflow would have occurred then the wrapped value is returned.
    fn overflowing_mul(self: T, v: T) -> (T, bool);
}
