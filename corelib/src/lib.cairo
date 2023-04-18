mod traits;
use traits::Add;
use traits::AddEq;
use traits::BitAnd;
use traits::BitOr;
use traits::BitXor;
use traits::Copy;
use traits::Div;
use traits::DivEq;
use traits::Drop;
use traits::Mul;
use traits::MulEq;
use traits::PartialEq;
use traits::PartialOrd;
use traits::Rem;
use traits::RemEq;
use traits::Sub;
use traits::SubEq;
use traits::TupleSize0Copy;
use traits::TupleSize0Drop;
use traits::TupleSize0PartialEq;
use traits::TupleSize1Copy;
use traits::TupleSize1Drop;
use traits::TupleSize1PartialEq;
use traits::TupleSize2Copy;
use traits::TupleSize2Drop;
use traits::TupleSize3Copy;
use traits::TupleSize3Drop;
use traits::TupleSize4Copy;
use traits::TupleSize4Drop;
use traits::Not;
use traits::Neg;
use traits::Into;
use traits::TryInto;
use traits::Index;
use traits::IndexView;
use traits::Destruct;
use traits::Default;
use traits::Felt252DictValue;

#[derive(Copy, Drop)]
enum bool {
    False: (),
    True: (),
}

extern fn bool_and_impl(lhs: bool, rhs: bool) -> (bool, ) implicits() nopanic;
impl BoolBitAnd of BitAnd<bool> {
    #[inline(always)]
    fn bitand(lhs: bool, rhs: bool) -> bool {
        let (r, ) = bool_and_impl(lhs, rhs);
        r
    }
}

extern fn bool_or_impl(lhs: bool, rhs: bool) -> (bool, ) implicits() nopanic;
impl BoolBitOr of BitOr<bool> {
    #[inline(always)]
    fn bitor(lhs: bool, rhs: bool) -> bool {
        let (r, ) = bool_or_impl(lhs, rhs);
        r
    }
}

extern fn bool_not_impl(a: bool) -> (bool, ) implicits() nopanic;
#[inline(always)]
impl BoolNot of Not<bool> {
    #[inline(always)]
    fn not(a: bool) -> bool implicits() nopanic {
        let (r, ) = bool_not_impl(a);
        r
    }
}

extern fn bool_xor_impl(lhs: bool, rhs: bool) -> (bool, ) implicits() nopanic;
impl BoolBitXor of BitXor<bool> {
    #[inline(always)]
    fn bitxor(lhs: bool, rhs: bool) -> bool {
        let (r, ) = bool_xor_impl(lhs, rhs);
        r
    }
}

extern fn bool_eq(lhs: bool, rhs: bool) -> bool implicits() nopanic;
impl BoolPartialEq of PartialEq<bool> {
    #[inline(always)]
    fn eq(lhs: bool, rhs: bool) -> bool {
        bool_eq(lhs, rhs)
    }
    #[inline(always)]
    fn ne(lhs: bool, rhs: bool) -> bool {
        !(lhs == rhs)
    }
}

extern fn bool_to_felt252(a: bool) -> felt252 implicits() nopanic;

// General purpose implicits.
extern type RangeCheck;
extern type SegmentArena;

// felt252.
#[derive(Copy, Drop)]
extern type felt252;
extern fn felt252_const<const value: felt252>() -> felt252 nopanic;

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
        a * felt252_const::<-1>()
    }
}

extern fn felt252_div(lhs: felt252, rhs: NonZero<felt252>) -> felt252 nopanic;

impl Felt252PartialEq of PartialEq<felt252> {
    #[inline(always)]
    fn eq(lhs: felt252, rhs: felt252) -> bool {
        match lhs - rhs {
            0 => bool::True(()),
            _ => bool::False(()),
        }
    }
    #[inline(always)]
    fn ne(lhs: felt252, rhs: felt252) -> bool {
        !(lhs == rhs)
    }
}

extern fn felt252_is_zero(lhs: felt252) -> zeroable::IsZeroResult<felt252> nopanic;

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
mod box;
use box::Box;
use box::BoxTrait;

// Nullable
mod nullable;
use nullable::Nullable;
use nullable::match_nullable;
use nullable::null;
use nullable::nullable_from_box;

// Arrays.
mod array;
use array::Array;
use array::ArrayTrait;
type usize = u32;

// Span.
use array::Span;


// Dictionary.
mod dict;
use dict::Felt252Dict;
use dict::SquashedFelt252Dict;
use dict::felt252_dict_new;
use dict::felt252_dict_write;
use dict::felt252_dict_read;
use dict::felt252_dict_squash;
use dict::Felt252DictTrait;

// Result.
mod result;
use result::Result;

// Option.
mod option;
use option::Option;

// Clone.
mod clone;
use clone::Clone;

// EC.
mod ec;
use ec::EcOp;
use ec::EcPoint;
use ec::EcState;

mod ecdsa;

// Integer.
mod integer;
use integer::NumericLiteral;
use integer::u128;
use integer::u128_const;
use integer::u128_sqrt;
use integer::u128_is_zero;
use integer::u8;
use integer::u8_const;
use integer::u16;
use integer::u16_const;
use integer::u32;
use integer::u32_const;
use integer::u64;
use integer::u64_const;
use integer::u256;
use integer::Felt252TryIntoU8;
use integer::U8IntoFelt252;
use integer::Felt252TryIntoU16;
use integer::U16IntoFelt252;
use integer::Felt252TryIntoU32;
use integer::U32IntoFelt252;
use integer::Felt252TryIntoU64;
use integer::U64IntoFelt252;
use integer::Felt252TryIntoU128;
use integer::U128IntoFelt252;
use integer::U16TryIntoU8;
use integer::U32TryIntoU16;
use integer::U64TryIntoU32;
use integer::U128TryIntoU64;
use integer::Felt252IntoU256;
use integer::Bitwise;

// Gas.
mod gas;
use gas::BuiltinCosts;
use gas::GasBuiltin;
use gas::get_builtin_costs;


// Panics.
enum PanicResult<T> {
    Ok: T,
    Err: Array<felt252>,
}
enum never {}
extern fn panic(data: Array<felt252>) -> never;

#[inline(always)]
fn panic_with_felt252(err_code: felt252) -> never {
    let mut data = ArrayTrait::new();
    data.append(err_code);
    panic(data)
}

fn assert(cond: bool, err_code: felt252) {
    if !cond {
        panic_with_felt252(err_code)
    }
}

// Serialization and Deserialization.
mod serde;

// Hash functions.
mod hash;
use hash::pedersen;
use hash::Pedersen;

// Poseidon
mod poseidon;
use poseidon::Poseidon;

// Debug.
mod debug;

// Starknet
mod starknet;
use starknet::System;

// Internals.
mod internal;

// Zeroable.
mod zeroable;
use zeroable::Zeroable;
use zeroable::NonZero;

#[cfg(test)]
mod test;

// Module for testing only.
mod testing;
