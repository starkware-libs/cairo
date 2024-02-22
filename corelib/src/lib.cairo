pub mod traits;
use traits::{
    Add, AddEq, BitAnd, BitNot, BitOr, BitXor, Copy, Div, DivEq, DivRem, Drop, Mul, MulEq,
    PartialEq, PartialOrd, Rem, RemEq, Sub, SubEq, TupleSize0Copy, TupleSize0Drop, Not, Neg, Into,
    TryInto, Index, IndexView, Destruct, Default, Felt252DictValue, PanicDestruct
};
use serde::Serde;

pub type usize = u32;

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
    #[inline(always)]
    fn bitand(lhs: bool, rhs: bool) -> bool {
        let (r,) = bool_and_impl(lhs, rhs);
        r
    }
}

extern fn bool_or_impl(lhs: bool, rhs: bool) -> (bool,) implicits() nopanic;
impl BoolBitOr of BitOr<bool> {
    #[inline(always)]
    fn bitor(lhs: bool, rhs: bool) -> bool {
        let (r,) = bool_or_impl(lhs, rhs);
        r
    }
}

extern fn bool_not_impl(a: bool) -> (bool,) implicits() nopanic;
#[inline(always)]
impl BoolNot of Not<bool> {
    #[inline(always)]
    fn not(a: bool) -> bool implicits() nopanic {
        let (r,) = bool_not_impl(a);
        r
    }
}

extern fn bool_xor_impl(lhs: bool, rhs: bool) -> (bool,) implicits() nopanic;
impl BoolBitXor of BitXor<bool> {
    #[inline(always)]
    fn bitxor(lhs: bool, rhs: bool) -> bool {
        let (r,) = bool_xor_impl(lhs, rhs);
        r
    }
}

impl BoolPartialEq of PartialEq<bool> {
    #[inline(always)]
    fn eq(lhs: @bool, rhs: @bool) -> bool {
        match lhs {
            false => !*rhs,
            true => *rhs,
        }
    }
    #[inline(always)]
    fn ne(lhs: @bool, rhs: @bool) -> bool {
        match lhs {
            false => *rhs,
            true => !*rhs,
        }
    }
}

/// Default values for felt252_dict values.
impl BoolFelt252DictValue of Felt252DictValue<bool> {
    #[inline(always)]
    fn zero_default() -> bool nopanic {
        false
    }
}

extern fn bool_to_felt252(a: bool) -> felt252 implicits() nopanic;
impl BoolIntoFelt252 of Into<bool, felt252> {
    #[inline(always)]
    fn into(self: bool) -> felt252 implicits() nopanic {
        bool_to_felt252(self)
    }
}
pub mod boolean;

// General purpose implicits.
pub extern type RangeCheck;
pub extern type SegmentArena;

// felt252.
mod felt_252;
use felt_252::{Felt252One, Felt252Zero};

#[derive(Copy, Drop)]
pub extern type felt252;
extern fn felt252_const<const value: felt252>() -> felt252 nopanic;

impl Felt252Serde of Serde<felt252> {
    fn serialize(self: @felt252, ref output: Array<felt252>) {
        output.append(*self);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<felt252> {
        let mut snapshot = serialized.snapshot;
        match core::array::array_snapshot_pop_front(ref snapshot) {
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

impl Felt252Add of Add<felt252> {
    #[inline(always)]
    fn add(lhs: felt252, rhs: felt252) -> felt252 {
        felt252_add(lhs, rhs)
    }
}
impl Felt252AddEq of AddEq<felt252> {
    #[inline(always)]
    fn add_eq(ref self: felt252, other: felt252) {
        self = Add::add(self, other);
    }
}

extern fn felt252_add(lhs: felt252, rhs: felt252) -> felt252 nopanic;
impl Felt252Sub of Sub<felt252> {
    #[inline(always)]
    fn sub(lhs: felt252, rhs: felt252) -> felt252 {
        felt252_sub(lhs, rhs)
    }
}
impl Felt252SubEq of SubEq<felt252> {
    #[inline(always)]
    fn sub_eq(ref self: felt252, other: felt252) {
        self = Sub::sub(self, other);
    }
}

extern fn felt252_sub(lhs: felt252, rhs: felt252) -> felt252 nopanic;
impl Felt252Mul of Mul<felt252> {
    #[inline(always)]
    fn mul(lhs: felt252, rhs: felt252) -> felt252 {
        felt252_mul(lhs, rhs)
    }
}
impl Felt252MulEq of MulEq<felt252> {
    #[inline(always)]
    fn mul_eq(ref self: felt252, other: felt252) {
        self = Mul::mul(self, other);
    }
}

extern fn felt252_mul(lhs: felt252, rhs: felt252) -> felt252 nopanic;

impl Felt252Neg of Neg<felt252> {
    #[inline(always)]
    fn neg(a: felt252) -> felt252 {
        a * -1
    }
}

pub extern fn felt252_div(lhs: felt252, rhs: NonZero<felt252>) -> felt252 nopanic;

impl Felt252PartialEq of PartialEq<felt252> {
    #[inline(always)]
    fn eq(lhs: @felt252, rhs: @felt252) -> bool {
        match *lhs - *rhs {
            0 => true,
            _ => false,
        }
    }
    #[inline(always)]
    fn ne(lhs: @felt252, rhs: @felt252) -> bool {
        !(*lhs == *rhs)
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
    #[inline(always)]
    fn default() -> felt252 nopanic {
        0
    }
}

impl Felt252Felt252DictValue of Felt252DictValue<felt252> {
    #[inline(always)]
    fn zero_default() -> felt252 nopanic {
        0
    }
}

// TODO(spapini): Constraint using Copy and Drop traits.
extern fn dup<T>(obj: T) -> (T, T) nopanic;
extern fn drop<T>(obj: T) nopanic;

// Boxes.
pub mod box;
use box::{Box, BoxTrait};

// Nullable
pub mod nullable;
use nullable::{Nullable, NullableTrait, match_nullable, null, nullable_from_box};

// Array.
pub mod array;
use array::{Array, ArrayTrait};

// Span.
use array::{Span, SpanTrait};

// Dictionary.
pub mod dict;
use dict::{
    Felt252Dict, SquashedFelt252Dict, felt252_dict_new, felt252_dict_squash, Felt252DictTrait
};

// Result.
pub mod result;
use result::{Result, ResultTrait};

// Option.
pub mod option;
use option::{Option, OptionTrait};

// Clone.
pub mod clone;
use clone::Clone;

// EC.
pub mod ec;
use ec::{EcOp, EcPoint, EcState};

pub mod ecdsa;

// Integer.
pub mod integer;
use integer::{
    i8, I8IntoFelt252, i16, I16IntoFelt252, i32, I32IntoFelt252, i64, I64IntoFelt252, i128,
    I128IntoFelt252, NumericLiteral, u128, u128_sqrt, u128_is_zero, u8, u16, u32, u64, u256,
    u256_sqrt, Felt252TryIntoU8, U8IntoFelt252, Felt252TryIntoU16, U16IntoFelt252,
    Felt252TryIntoU32, U32IntoFelt252, Felt252TryIntoU64, U64IntoFelt252, Felt252TryIntoU128,
    U128IntoFelt252, Felt252IntoU256, Bitwise
};

// Math.
pub mod math;

// Num.
pub mod num;

// Cmp.
pub mod cmp;

// Gas.
pub mod gas;
use gas::{BuiltinCosts, GasBuiltin, get_builtin_costs};


// Panics.
pub mod panics;
use panics::{panic, Panic, PanicResult};

pub enum never {}

#[inline(always)]
pub fn panic_with_felt252(err_code: felt252) -> never {
    panic(array![err_code])
}

#[inline(always)]
pub fn assert(cond: bool, err_code: felt252) {
    if !cond {
        panic_with_felt252(err_code)
    }
}

// Serialization and Deserialization.
pub mod serde;

// Hash functions.
pub mod hash;

pub mod keccak;

// Pedersen
pub mod pedersen;
use pedersen::Pedersen;

// Poseidon
pub mod poseidon;
use poseidon::Poseidon;

// Debug.
pub mod debug;

pub mod fmt;

// Starknet
pub mod starknet;
use starknet::System;

// Internals.
pub mod internal;

// Zeroable.
pub mod zeroable;
use zeroable::{Zeroable, NonZero};

// bytes31.
pub mod bytes_31;
use bytes_31::{
    bytes31, bytes31_const, Bytes31IndexView, Bytes31IntoFelt252, Bytes31Trait,
    Felt252TryIntoBytes31
};

// BytesArray.
pub mod byte_array;
use byte_array::{ByteArray, ByteArrayIndexView, ByteArrayStringLiteral, ByteArrayTrait};

// String.
pub mod string;
use string::StringLiteral;

// to_byte_array.
pub mod to_byte_array;

#[cfg(test)]
mod test;

// Module for testing only.
pub mod testing;

// Metaprogramming.
pub mod metaprogramming;

// Preludes.
mod prelude;
