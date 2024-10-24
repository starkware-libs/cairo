/// Performs addition that returns `None` instead of wrapping around on
/// overflow.
pub trait CheckedAdd<T> {
    /// Adds two numbers, checking for overflow. If overflow happens, `None` is
    /// returned.
    fn checked_add(self: T, v: T) -> Option<T>;
}

/// Performs subtraction that returns `None` instead of wrapping around on underflow.
pub trait CheckedSub<T> {
    /// Subtracts two numbers, checking for underflow. If underflow happens,
    /// `None` is returned.
    fn checked_sub(self: T, v: T) -> Option<T>;
}

/// Performs multiplication that returns `None` instead of wrapping around on underflow or
/// overflow.
pub trait CheckedMul<T> {
    /// Multiplies two numbers, checking for underflow or overflow. If underflow
    /// or overflow happens, `None` is returned.
    fn checked_mul(self: T, v: T) -> Option<T>;
}

pub(crate) mod overflow_based {
    pub(crate) impl TCheckedAdd<
        T, +Drop<T>, +crate::num::traits::OverflowingAdd<T>
    > of crate::num::traits::CheckedAdd<T> {
        fn checked_add(self: T, v: T) -> Option<T> {
            let (result, overflow) = self.overflowing_add(v);
            match overflow {
                true => Option::None,
                false => Option::Some(result),
            }
        }
    }

    pub(crate) impl TCheckedSub<
        T, +Drop<T>, +crate::num::traits::OverflowingSub<T>
    > of crate::num::traits::CheckedSub<T> {
        fn checked_sub(self: T, v: T) -> Option<T> {
            let (result, overflow) = self.overflowing_sub(v);
            match overflow {
                true => Option::None,
                false => Option::Some(result),
            }
        }
    }

    pub(crate) impl TCheckedMul<
        T, +Drop<T>, +crate::num::traits::OverflowingMul<T>
    > of crate::num::traits::CheckedMul<T> {
        fn checked_mul(self: T, v: T) -> Option<T> {
            let (result, overflow) = self.overflowing_mul(v);
            match overflow {
                true => Option::None,
                false => Option::Some(result),
            }
        }
    }
}
