// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (utils/src/math.cairo)

use core::integer::u512_safe_div_rem_by_u256;
use core::num::traits::WideMul;
use core::traits::{BitAnd, BitXor, Into};

/// Returns the average of two unsigned integers. The result is rounded down.
pub fn average<
    T, +Unsigned<T>, +Add<T>, +Div<T>, +BitAnd<T>, +BitXor<T>, +Into<u8, T>, +Copy<T>, +Drop<T>,
>(
    a: T, b: T,
) -> T {
    // (a + b) / 2 can overflow.
    (a & b) + (a ^ b) / 2_u8.into()
}

/// A trait to represent unsigned integers.
pub trait Unsigned<T>;

impl U8Unsigned of Unsigned<u8>;
impl U16Unsigned of Unsigned<u16>;
impl U32Unsigned of Unsigned<u32>;
impl U64Unsigned of Unsigned<u64>;
impl U128Unsigned of Unsigned<u128>;
impl U256Unsigned of Unsigned<u256>;

#[derive(Drop, Copy, Debug)]
pub enum Rounding {
    Floor, // Toward negative infinity
    Ceil, // Toward positive infinity
    Trunc, // Toward zero
    Expand // Away from zero
}

/// Returns the quotient of x * y / denominator and rounds up or down depending on `rounding`.
/// Uses `wide_mul` and `u512_safe_div_rem_by_u256` for precision.
///
/// Requirements:
///
/// - `denominator` must not be zero.
/// - The quotient must not overflow u256.
pub fn u256_mul_div(x: u256, y: u256, denominator: u256, rounding: Rounding) -> u256 {
    let (q, r) = _raw_u256_mul_div(x, y, denominator);

    let is_rounded_up = match rounding {
        Rounding::Ceil => 1,
        Rounding::Expand => 1,
        Rounding::Trunc => 0,
        Rounding::Floor => 0,
    };

    let has_remainder = if r > 0 {
        1
    } else {
        0
    };

    q + (is_rounded_up & has_remainder)
}

/// Returns the quotient and remainder of x * y / denominator.
/// Uses `wide_mul` and `u512_safe_div_rem_by_u256` for precision.
///
/// Requirements:
///
/// - `denominator` must not be zero.
/// - The quotient must not overflow u256.
fn _raw_u256_mul_div(x: u256, y: u256, denominator: u256) -> (u256, u256) {
    let denominator = denominator.try_into().expect('mul_div division by zero');
    let p = x.wide_mul(y);
    let (q, r) = u512_safe_div_rem_by_u256(p, denominator);
    let q = q.try_into().expect('mul_div quotient > u256');
    (q, r)
}
