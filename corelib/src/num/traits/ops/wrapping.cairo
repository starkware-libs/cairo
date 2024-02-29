/// Performs addition that wraps around on overflow.
pub trait WrappingAdd<T> {
    /// Wrapping (modular) addition. Computes `self + other`, wrapping around at the boundary of the
    /// type.
    fn wrapping_add(self: T, v: T) -> T;
}

/// Performs subtraction that wraps around on overflow.
pub trait WrappingSub<T> {
    /// Wrapping (modular) subtraction. Computes `self - other`, wrapping around at the boundary of
    /// the type.
    fn wrapping_sub(self: T, v: T) -> T;
}

/// Performs multiplication that wraps around on overflow.
pub trait WrappingMul<T> {
    /// Wrapping (modular) multiplication. Computes `self * other`, wrapping around at the boundary
    /// of the type.
    fn wrapping_mul(self: T, v: T) -> T;
}
