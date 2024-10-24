/// A trait defining minimum and maximum bounds for numeric types.
/// Only supports types that can have a constant value.
///
/// Example:
/// ```
/// Bounded::<MyType>::MAX;
/// ```
pub trait Bounded<T> {
    /// The minimum allowable value.
    const MIN: T;
    /// The maximum allowable value.
    const MAX: T;
}

impl BoundedU8 of Bounded<u8> {
    const MIN: u8 = 0x0;
    const MAX: u8 = 0xff;
}

impl BoundedU16 of Bounded<u16> {
    const MIN: u16 = 0x0;
    const MAX: u16 = 0xffff;
}

impl BoundedU32 of Bounded<u32> {
    const MIN: u32 = 0x0;
    const MAX: u32 = 0xffffffff;
}

impl BoundedU64 of Bounded<u64> {
    const MIN: u64 = 0x0;
    const MAX: u64 = 0xffffffffffffffff;
}

impl BoundedU128 of Bounded<u128> {
    const MIN: u128 = 0x0;
    const MAX: u128 = 0xffffffffffffffffffffffffffffffff;
}

impl BoundedU256 of Bounded<u256> {
    const MIN: u256 = 0x0;
    const MAX: u256 = 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff;
}

// Implementations for signed integer types
impl BoundedI8 of Bounded<i8> {
    const MIN: i8 = -0x80;
    const MAX: i8 = 0x7f;
}

impl BoundedI16 of Bounded<i16> {
    const MIN: i16 = -0x8000;
    const MAX: i16 = 0x7fff;
}

impl BoundedI32 of Bounded<i32> {
    const MIN: i32 = -0x80000000;
    const MAX: i32 = 0x7fffffff;
}

impl BoundedI64 of Bounded<i64> {
    const MIN: i64 = -0x8000000000000000;
    const MAX: i64 = 0x7fffffffffffffff;
}

impl BoundedI128 of Bounded<i128> {
    const MIN: i128 = -0x80000000000000000000000000000000;
    const MAX: i128 = 0x7fffffffffffffffffffffffffffffff;
}
