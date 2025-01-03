//! Main entrypoint for the Cairo core library.

pub mod traits;
#[feature("deprecated-index-traits")]
#[feature("deprecated-op-assign-traits")]
#[allow(unused_imports)]
use traits::{
    Add, AddEq, BitAnd, BitNot, BitOr, BitXor, Copy, Div, DivEq, DivRem, Drop, Mul, MulEq,
    PartialEq, PartialOrd, Rem, RemEq, Sub, SubEq, TupleSize0Copy, TupleSize0Drop, Not, Neg, Into,
    TryInto, Index, IndexView, Destruct, Default, Felt252DictValue, PanicDestruct,
};
use serde::Serde;

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
        Option::Some(*serialized.pop_front()? != 0)
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
#[derive(Copy, Drop)]
pub extern type felt252;

extern fn felt252_const<const value: felt252>() -> felt252 nopanic;

impl Felt252Serde of Serde<felt252> {
    fn serialize(self: @felt252, ref output: Array<felt252>) {
        output.append(*self);
    }

    fn deserialize(ref serialized: Span<felt252>) -> Option<felt252> {
        let mut snapshot = serialized.snapshot;
        match crate::array::array_snapshot_pop_front(ref snapshot) {
            Option::Some(x) => {
                serialized = Span { snapshot };
                Option::Some(*x.unbox())
            },
            Option::None => {
                serialized = Span { snapshot };
                Option::None
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

/// Performs a division on `felt252` values.
///
/// # Examples
///
/// ```
/// use core::felt252_div;
///
/// assert!(felt252_div(4, 2) == 2);
/// ```
///
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
            zeroable::IsZeroResult::Zero => Option::None,
            zeroable::IsZeroResult::NonZero(x) => Option::Some(x),
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

pub mod box;
#[allow(unused_imports)]
use box::{Box, BoxTrait};

pub mod nullable;
#[allow(unused_imports)]
use nullable::{Nullable, NullableTrait, match_nullable, null, nullable_from_box};

pub mod array;
#[allow(unused_imports)]
use array::{Array, ArrayTrait};

#[allow(unused_imports)]
use array::{Span, SpanTrait};

pub mod dict;
#[allow(unused_imports)]
use dict::{
    Felt252Dict, SquashedFelt252Dict, felt252_dict_new, felt252_dict_squash, Felt252DictTrait,
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
    i8, I8IntoFelt252, i16, I16IntoFelt252, i32, I32IntoFelt252, i64, I64IntoFelt252, i128,
    I128IntoFelt252, NumericLiteral, u128, u128_is_zero, u8, u16, u32, u64, u256, Felt252TryIntoU8,
    U8IntoFelt252, Felt252TryIntoU16, U16IntoFelt252, Felt252TryIntoU32, U32IntoFelt252,
    Felt252TryIntoU64, U64IntoFelt252, Felt252TryIntoU128, U128IntoFelt252, Felt252IntoU256,
    Bitwise,
};
#[feature("corelib-internal-use")]
#[deprecated(feature: "corelib-internal-use", note: "Use `core::num::traits::Sqrt` instead")]
#[allow(unused_imports)]
use integer::{u128_sqrt, u256_sqrt};

#[feature("corelib-internal-use")]
pub mod math;

pub mod num;

pub mod ops;

pub mod cmp;

pub mod gas;
#[allow(unused_imports)]
use gas::{BuiltinCosts, GasBuiltin, get_builtin_costs};

pub mod panics;
#[allow(unused_imports)]
use panics::{panic, Panic, PanicResult};

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
#[inline]
pub fn panic_with_felt252(err_code: felt252) -> never {
    panic(array![err_code])
}

/// Panics if `cond` is false with the given `felt252` as error message.
///
/// # Examples
///
/// ```
/// assert(false, 'error message');
/// ```
#[inline]
pub fn assert(cond: bool, err_code: felt252) {
    if !cond {
        panic_with_felt252(err_code)
    }
}

pub mod serde;

pub mod hash;

pub mod keccak;

pub mod sha256;

pub mod pedersen;
#[allow(unused_imports)]
use pedersen::Pedersen;

pub mod poseidon;
#[allow(unused_imports)]
use poseidon::Poseidon;

pub mod debug;

pub mod fmt;

#[feature("corelib-internal-use")]
pub mod starknet;
#[allow(unused_imports)]
use starknet::System;

pub mod internal;

pub mod zeroable;
#[allow(unused_imports)]
use zeroable::{Zeroable, NonZero};

pub mod bytes_31;
#[allow(unused_imports)]
use bytes_31::{
    bytes31, bytes31_const, Bytes31IndexView, Bytes31IntoFelt252, Bytes31Trait,
    Felt252TryIntoBytes31,
};

pub mod byte_array;
#[allow(unused_imports)]
use byte_array::{ByteArray, ByteArrayIndexView, ByteArrayStringLiteral, ByteArrayTrait};

pub mod string;
#[allow(unused_imports)]
use string::StringLiteral;

pub mod to_byte_array;

#[cfg(test)]
mod test;

pub mod testing;

pub mod metaprogramming;

#[allow(unused_imports)]
mod prelude;

pub mod iter;
