//! Trait and implementations for raising a value to a power.
//!
//! This module provides efficient exponentiation operations for numeric types using
//! the square-and-multiply algorithm, which achieves logarithmic time complexity O(log n).

/// Raises a value to the power of `exp`.
///
/// Note that `0⁰` (`pow(0, 0)`) returns `1`. Mathematically this is undefined.
///
/// # Panics
///
/// Panics if the result of the exponentiation operation overflows the output type.
///
/// # Examples
///
/// ```
/// use core::num::traits::Pow;
///
/// assert!(2_i8.pow(4_usize) == 16_i8);
/// assert!(6_u8.pow(3_usize) == 216_u8);
/// assert!(0_u8.pow(0_usize) == 1_u8);
/// ```
pub trait Pow<Base, Exp> {
    /// The type of the result of the power calculation.
    type Output;

    /// Returns `self` to the power `exp`.
    fn pow(self: Base, exp: Exp) -> Self::Output;
}

// TODO(gil): Use a macro for it instead of copy paste.
// Not using a trait for the implementation to allow `fn` to be `const`.

impl PowFelt252 of Pow<felt252, usize> {
    type Output = felt252;

    const fn pow(self: felt252, exp: usize) -> felt252 {
        let (tail_exp, head_exp) = DivRem::div_rem(exp, 2);
        let tail_result = if tail_exp == 0 {
            1
        } else {
            Self::pow(self * self, tail_exp)
        };
        if head_exp == 0 {
            tail_result
        } else {
            tail_result * self
        }
    }
}

impl PowI8 of Pow<i8, usize> {
    type Output = i8;

    const fn pow(self: i8, exp: usize) -> i8 {
        let (tail_exp, head_exp) = DivRem::div_rem(exp, 2);
        let tail_result = if tail_exp == 0 {
            1
        } else {
            Self::pow(self * self, tail_exp)
        };
        if head_exp == 0 {
            tail_result
        } else {
            tail_result * self
        }
    }
}

impl PowU8 of Pow<u8, usize> {
    type Output = u8;

    const fn pow(self: u8, exp: usize) -> u8 {
        let (tail_exp, head_exp) = DivRem::div_rem(exp, 2);
        let tail_result = if tail_exp == 0 {
            1
        } else {
            Self::pow(self * self, tail_exp)
        };
        if head_exp == 0 {
            tail_result
        } else {
            tail_result * self
        }
    }
}

impl PowI16 of Pow<i16, usize> {
    type Output = i16;

    const fn pow(self: i16, exp: usize) -> i16 {
        let (tail_exp, head_exp) = DivRem::div_rem(exp, 2);
        let tail_result = if tail_exp == 0 {
            1
        } else {
            Self::pow(self * self, tail_exp)
        };
        if head_exp == 0 {
            tail_result
        } else {
            tail_result * self
        }
    }
}

impl PowU16 of Pow<u16, usize> {
    type Output = u16;

    const fn pow(self: u16, exp: usize) -> u16 {
        let (tail_exp, head_exp) = DivRem::div_rem(exp, 2);
        let tail_result = if tail_exp == 0 {
            1
        } else {
            Self::pow(self * self, tail_exp)
        };
        if head_exp == 0 {
            tail_result
        } else {
            tail_result * self
        }
    }
}

impl PowI32 of Pow<i32, usize> {
    type Output = i32;

    const fn pow(self: i32, exp: usize) -> i32 {
        let (tail_exp, head_exp) = DivRem::div_rem(exp, 2);
        let tail_result = if tail_exp == 0 {
            1
        } else {
            Self::pow(self * self, tail_exp)
        };
        if head_exp == 0 {
            tail_result
        } else {
            tail_result * self
        }
    }
}

impl PowU32 of Pow<u32, usize> {
    type Output = u32;

    const fn pow(self: u32, exp: usize) -> u32 {
        let (tail_exp, head_exp) = DivRem::div_rem(exp, 2);
        let tail_result = if tail_exp == 0 {
            1
        } else {
            Self::pow(self * self, tail_exp)
        };
        if head_exp == 0 {
            tail_result
        } else {
            tail_result * self
        }
    }
}

impl PowI64 of Pow<i64, usize> {
    type Output = i64;

    const fn pow(self: i64, exp: usize) -> i64 {
        let (tail_exp, head_exp) = DivRem::div_rem(exp, 2);
        let tail_result = if tail_exp == 0 {
            1
        } else {
            Self::pow(self * self, tail_exp)
        };
        if head_exp == 0 {
            tail_result
        } else {
            tail_result * self
        }
    }
}

impl PowU64 of Pow<u64, usize> {
    type Output = u64;

    const fn pow(self: u64, exp: usize) -> u64 {
        let (tail_exp, head_exp) = DivRem::div_rem(exp, 2);
        let tail_result = if tail_exp == 0 {
            1
        } else {
            Self::pow(self * self, tail_exp)
        };
        if head_exp == 0 {
            tail_result
        } else {
            tail_result * self
        }
    }
}

impl PowI128 of Pow<i128, usize> {
    type Output = i128;

    const fn pow(self: i128, exp: usize) -> i128 {
        let (tail_exp, head_exp) = DivRem::div_rem(exp, 2);
        let tail_result = if tail_exp == 0 {
            1
        } else {
            Self::pow(self * self, tail_exp)
        };
        if head_exp == 0 {
            tail_result
        } else {
            tail_result * self
        }
    }
}

impl PowU128 of Pow<u128, usize> {
    type Output = u128;

    const fn pow(self: u128, exp: usize) -> u128 {
        let (tail_exp, head_exp) = DivRem::div_rem(exp, 2);
        let tail_result = if tail_exp == 0 {
            1
        } else {
            Self::pow(self * self, tail_exp)
        };
        if head_exp == 0 {
            tail_result
        } else {
            tail_result * self
        }
    }
}

impl PowU256 of Pow<u256, usize> {
    type Output = u256;

    const fn pow(self: u256, exp: usize) -> u256 {
        let (tail_exp, head_exp) = DivRem::div_rem(exp, 2);
        let tail_result = if tail_exp == 0 {
            1
        } else {
            Self::pow(self * self, tail_exp)
        };
        if head_exp == 0 {
            tail_result
        } else {
            tail_result * self
        }
    }
}

