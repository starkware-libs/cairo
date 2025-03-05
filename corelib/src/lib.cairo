//! Main entrypoint for the Cairo core library.

pub mod traits;
use serde::Serde;
#[feature("deprecated-index-traits")]
#[feature("deprecated-op-assign-traits")]
#[allow(unused_imports)]
use traits::{
    Add, AddEq, BitAnd, BitNot, BitOr, BitXor, Copy, Default, Destruct, Div, DivEq, DivRem, Drop,
    Felt252DictValue, Index, IndexView, Into, Mul, MulEq, Neg, Not, PanicDestruct, PartialEq,
    PartialOrd, Rem, RemEq, Sub, SubEq, TryInto, TupleSize0Copy, TupleSize0Drop,
};

/// `usize` is an alias for `u32` type.
pub type usize = u32;

/// `bool` enum representing either `false` or `true`.
#[derive(Copy, Drop, Default)]
pub enum bool {
    #[default]
    False,
    True,
}

impl BoolSerde of Serde<bool> {
    fn serialize(self: @bool, ref output: Array<felt252>) {
        if *self {
            1_felt252
        } else {
            0_felt252
        }.serialize(ref output);
    }

    fn deserialize(ref serialized: Span<felt252>) -> Option<bool> {
        Some(*serialized.pop_front()? != 0)
    }
}

extern fn bool_and_impl(lhs: bool, rhs: bool) -> (bool,) implicits() nopanic;
impl BoolBitAnd of BitAnd<bool> {
    #[inline]
    fn bitand(lhs: bool, rhs: bool) -> bool {
        let (r,) = bool_and_impl(lhs, rhs);
        r
    }
}

extern fn bool_or_impl(lhs: bool, rhs: bool) -> (bool,) implicits() nopanic;
impl BoolBitOr of BitOr<bool> {
    #[inline]
    fn bitor(lhs: bool, rhs: bool) -> bool {
        let (r,) = bool_or_impl(lhs, rhs);
        r
    }
}

extern fn bool_not_impl(a: bool) -> (bool,) implicits() nopanic;
#[inline]
impl BoolNot of Not<bool> {
    #[inline]
    fn not(a: bool) -> bool implicits() nopanic {
        let (r,) = bool_not_impl(a);
        r
    }
}

extern fn bool_xor_impl(lhs: bool, rhs: bool) -> (bool,) implicits() nopanic;
impl BoolBitXor of BitXor<bool> {
    #[inline]
    fn bitxor(lhs: bool, rhs: bool) -> bool {
        let (r,) = bool_xor_impl(lhs, rhs);
        r
    }
}

impl BoolPartialEq of PartialEq<bool> {
    #[inline]
    fn eq(lhs: @bool, rhs: @bool) -> bool {
        match lhs {
            false => !*rhs,
            true => *rhs,
        }
    }

    #[inline]
    fn ne(lhs: @bool, rhs: @bool) -> bool {
        match lhs {
            false => *rhs,
            true => !*rhs,
        }
    }
}

impl BoolFelt252DictValue of Felt252DictValue<bool> {
    #[inline]
    fn zero_default() -> bool nopanic {
        false
    }
}

extern fn bool_to_felt252(a: bool) -> felt252 implicits() nopanic;
impl BoolIntoFelt252 of Into<bool, felt252> {
    #[inline]
    fn into(self: bool) -> felt252 implicits() nopanic {
        bool_to_felt252(self)
    }
}

pub mod boolean;

pub mod circuit;

/// General purpose implicits.
pub extern type RangeCheck;
pub extern type SegmentArena;

mod felt_252;
#[allow(unused_imports)]
use felt_252::{Felt252One, Felt252Zero};

/// `felt252` is the basic field element used in Cairo.
///
/// It corresponds to an integer in the range 0 ≤ x < P where P is
/// a very large prime number currently equal to 2^251 + 17⋅2^192 + 1.
///
/// Any operation that uses `felt252` will be computed modulo P.
pub extern type felt252;

impl felt252Copy of Copy<felt252>;
impl felt252Drop of Drop<felt252>;

extern fn felt252_const<const value: felt252>() -> felt252 nopanic;

impl Felt252Serde of Serde<felt252> {
    fn serialize(self: @felt252, ref output: Array<felt252>) {
        output.append(*self);
    }

    fn deserialize(ref serialized: Span<felt252>) -> Option<felt252> {
        let mut snapshot = serialized.snapshot;
        match crate::array::array_snapshot_pop_front(ref snapshot) {
            Some(x) => {
                serialized = Span { snapshot };
                Some(*x.unbox())
            },
            None => {
                serialized = Span { snapshot };
                None
            },
        }
    }
}

extern fn felt252_add(lhs: felt252, rhs: felt252) -> felt252 nopanic;

impl Felt252Add of Add<felt252> {
    #[inline]
    fn add(lhs: felt252, rhs: felt252) -> felt252 {
        felt252_add(lhs, rhs)
    }
}

impl Felt252AddEq of AddEq<felt252> {
    #[inline]
    fn add_eq(ref self: felt252, other: felt252) {
        self = Add::add(self, other);
    }
}

extern fn felt252_sub(lhs: felt252, rhs: felt252) -> felt252 nopanic;

impl Felt252Sub of Sub<felt252> {
    #[inline]
    fn sub(lhs: felt252, rhs: felt252) -> felt252 {
        felt252_sub(lhs, rhs)
    }
}

impl Felt252SubEq of SubEq<felt252> {
    #[inline]
    fn sub_eq(ref self: felt252, other: felt252) {
        self = Sub::sub(self, other);
    }
}

extern fn felt252_mul(lhs: felt252, rhs: felt252) -> felt252 nopanic;

impl Felt252Mul of Mul<felt252> {
    #[inline]
    fn mul(lhs: felt252, rhs: felt252) -> felt252 {
        felt252_mul(lhs, rhs)
    }
}

impl Felt252MulEq of MulEq<felt252> {
    #[inline]
    fn mul_eq(ref self: felt252, other: felt252) {
        self = Mul::mul(self, other);
    }
}

impl Felt252Neg of Neg<felt252> {
    #[inline]
    fn neg(a: felt252) -> felt252 {
        a * -1
    }
}

/// Performs division on `felt252` values in Cairo's finite field.
/// Unlike regular integer division, `felt252` division returns a field element n that satisfies
/// the equation: n * rhs ≡ lhs (mod P), where P is the `felt252` prime.
///
/// # Examples
///
/// ```
/// use core::felt252_div;
///
/// // Division with 0 remainder works the same way as integer division.
/// assert!(felt252_div(4, 2) == 2);
///
/// // Division with non 0 remainder returns a field element n where n * 3 ≡ 4 (mod P)
/// assert!(felt252_div(4, 3) ==
/// 1206167596222043737899107594365023368541035738443865566657697352045290673495);
///
/// ```
pub extern fn felt252_div(lhs: felt252, rhs: NonZero<felt252>) -> felt252 nopanic;

impl Felt252PartialEq of PartialEq<felt252> {
    #[inline]
    fn eq(lhs: @felt252, rhs: @felt252) -> bool {
        match *lhs - *rhs {
            0 => true,
            _ => false,
        }
    }
}

extern fn felt252_is_zero(lhs: felt252) -> zeroable::IsZeroResult<felt252> nopanic;

impl Felt252TryIntoNonZero of TryInto<felt252, NonZero<felt252>> {
    fn try_into(self: felt252) -> Option<NonZero<felt252>> {
        match felt252_is_zero(self) {
            zeroable::IsZeroResult::Zero => None,
            zeroable::IsZeroResult::NonZero(x) => Some(x),
        }
    }
}

impl Felt252Default of Default<felt252> {
    #[inline]
    fn default() -> felt252 nopanic {
        0
    }
}

impl Felt252Felt252DictValue of Felt252DictValue<felt252> {
    #[inline]
    fn zero_default() -> felt252 nopanic {
        0
    }
}

// TODO(spapini): Constraint using Copy and Drop traits.
extern fn dup<T>(obj: T) -> (T, T) nopanic;
extern fn drop<T>(obj: T) nopanic;

pub mod blake;

pub mod box;
#[allow(unused_imports)]
use box::{Box, BoxTrait};

pub mod nullable;
#[allow(unused_imports)]
use nullable::{Nullable, NullableTrait, match_nullable, null, nullable_from_box};

pub mod array;
#[allow(unused_imports)]
use array::{Array, ArrayTrait, Span, SpanTrait};

pub mod dict;
#[allow(unused_imports)]
use dict::{
    Felt252Dict, Felt252DictTrait, SquashedFelt252Dict, felt252_dict_new, felt252_dict_squash,
};

pub mod result;
#[allow(unused_imports)]
use result::{Result, ResultTrait};

pub mod option;
#[allow(unused_imports)]
use option::{Option, OptionTrait};

pub mod clone;
#[allow(unused_imports)]
use clone::Clone;

pub mod ec;
#[allow(unused_imports)]
use ec::{EcOp, EcPoint, EcState};

pub mod ecdsa;

#[feature("corelib-internal-use")]
pub mod integer;
#[allow(unused_imports)]
use integer::{
    Bitwise, Felt252IntoU256, Felt252TryIntoU128, Felt252TryIntoU16, Felt252TryIntoU32,
    Felt252TryIntoU64, Felt252TryIntoU8, I128IntoFelt252, I16IntoFelt252, I32IntoFelt252,
    I64IntoFelt252, I8IntoFelt252, NumericLiteral, U128IntoFelt252, U16IntoFelt252, U32IntoFelt252,
    U64IntoFelt252, U8IntoFelt252, i128, i16, i32, i64, i8, u128, u128_is_zero, u16, u256, u32, u64,
    u8,
};
#[feature("corelib-internal-use")]
#[deprecated(feature: "corelib-internal-use", note: "Use `core::num::traits::Sqrt` instead")]
#[allow(unused_imports)]
use integer::{u128_sqrt, u256_sqrt};

pub mod cmp;

pub mod gas;

#[feature("corelib-internal-use")]
pub mod math;

pub mod num;

pub mod ops;
#[allow(unused_imports)]
use gas::{BuiltinCosts, GasBuiltin, get_builtin_costs};

pub mod panics;
#[allow(unused_imports)]
use panics::{Panic, PanicResult, panic};

pub enum never {}

/// Panics with the given `felt252` as error message.
///
/// # Examples
///
/// ```
/// use core::panic_with_felt252;
///
/// panic_with_felt252('error message');
/// ```
#[inline(never)]
pub fn panic_with_felt252(err_code: felt252) -> never {
    panic(array![err_code])
}

/// Panics with the given const argument `felt252` as error message.
///
/// # Examples
///
/// ```
/// use core::panic_with_const_felt252;
///
/// panic_with_const_felt252::<'error message'>();
/// ```
#[inline(never)]
pub fn panic_with_const_felt252<const ERR_CODE: felt252>() -> never {
    panic(array![ERR_CODE])
}

/// Panics if `cond` is false with the given `felt252` as error message.
///
/// # Examples
///
/// ```
/// assert(false, 'error message');
/// ```
#[inline]
pub const fn assert(cond: bool, err_code: felt252) {
    if !cond {
        panic_with_felt252(err_code)
    }
}

pub mod hash;

pub mod keccak;

pub mod pedersen;

pub mod qm31;

pub mod serde;

pub mod sha256;
#[allow(unused_imports)]
use pedersen::Pedersen;

pub mod poseidon;
#[allow(unused_imports)]
use poseidon::Poseidon;

pub mod debug;

pub mod fmt;

#[feature("corelib-internal-use")]
#[deprecated(feature: "corelib-internal-use", note: "Use `starknet` directly")]
pub mod starknet;
#[allow(unused_imports)]
#[feature("corelib-internal-use")]
use starknet::System;

pub mod internal;

pub mod zeroable;
#[allow(unused_imports)]
use zeroable::{NonZero, Zeroable};

pub mod bytes_31;
#[allow(unused_imports)]
use bytes_31::{
    Bytes31IndexView, Bytes31IntoFelt252, Bytes31Trait, Felt252TryIntoBytes31, bytes31,
    bytes31_const,
};

pub mod byte_array;
#[allow(unused_imports)]
use byte_array::{ByteArray, ByteArrayIndexView, ByteArrayStringLiteral, ByteArrayTrait};

pub mod string;
#[allow(unused_imports)]
use string::StringLiteral;

pub mod iter;

pub mod metaprogramming;

#[allow(unused_imports)]
mod prelude;

#[cfg(test)]
mod test;

pub mod testing;

pub mod to_byte_array;
