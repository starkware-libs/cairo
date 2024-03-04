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

pub(crate) mod overflow_based {
    pub(crate) impl TWrappingAdd<
        T, +core::num::traits::OverflowingAdd<T>
    > of core::num::traits::WrappingAdd<T> {
        fn wrapping_add(self: T, v: T) -> T {
            let (result, _) = self.overflowing_add(v);
            result
        }
    }

    pub(crate) impl TWrappingSub<
        T, +core::num::traits::OverflowingSub<T>
    > of core::num::traits::WrappingSub<T> {
        fn wrapping_sub(self: T, v: T) -> T {
            let (result, _) = self.overflowing_sub(v);
            result
        }
    }

    pub(crate) impl TWrappingMul<
        T, +core::num::traits::OverflowingMul<T>
    > of core::num::traits::WrappingMul<T> {
        fn wrapping_mul(self: T, v: T) -> T {
            let (result, _) = self.overflowing_mul(v);
            result
        }
    }
}
