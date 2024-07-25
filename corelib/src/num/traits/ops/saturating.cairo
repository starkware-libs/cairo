/// Performs addition that saturates at the numeric bounds instead of overflowing.
pub trait SaturatingAdd<T> {
    /// Saturating addition. Computes `self + other`, saturating at the relevant high or low
    /// boundary of the type.
    fn saturating_add(self: T, other: T) -> T;
}

/// Performs subtraction that saturates at the numeric bounds instead of overflowing.
pub trait SaturatingSub<T> {
    /// Saturating subtraction. Computes `self - other`, saturating at the relevant high or low
    /// boundary of the type.
    fn saturating_sub(self: T, other: T) -> T;
}

/// Performs multiplication that saturates at the numeric bounds instead of overflowing.
pub trait SaturatingMul<T> {
    /// Saturating multiplication. Computes `self * other`, saturating at the relevant high or low
    /// boundary of the type.
    fn saturating_mul(self: T, other: T) -> T;
}

pub(crate) mod overflow_based {
    pub(crate) impl TSaturatingAdd<
        T, +Drop<T>, +core::num::traits::OverflowingAdd<T>, +core::num::traits::Bounded<T>
    > of core::num::traits::SaturatingAdd<T> {
        fn saturating_add(self: T, other: T) -> T {
            let (result, overflow) = self.overflowing_add(other);
            match overflow {
                true => core::num::traits::Bounded::MAX,
                false => result,
            }
        }
    }

    pub(crate) impl TSaturatingSub<
        T, +Drop<T>, +core::num::traits::OverflowingSub<T>, +core::num::traits::Bounded<T>
    > of core::num::traits::SaturatingSub<T> {
        fn saturating_sub(self: T, other: T) -> T {
            let (result, overflow) = self.overflowing_sub(other);
            match overflow {
                true => core::num::traits::Bounded::MIN,
                false => result,
            }
        }
    }

    pub(crate) impl TSaturatingMul<
        T, +Drop<T>, +core::num::traits::OverflowingMul<T>, +core::num::traits::Bounded<T>
    > of core::num::traits::SaturatingMul<T> {
        fn saturating_mul(self: T, other: T) -> T {
            let (result, overflow) = self.overflowing_mul(other);
            match overflow {
                true => core::num::traits::Bounded::MAX,
                false => result,
            }
        }
    }
}
