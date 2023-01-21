mod traits;
use traits::Copy;
use traits::Drop;

#[derive(Copy, Drop)]
enum bool { False: (), True: (), }

extern fn bool_and_impl(a: bool, b: bool) -> (bool,) implicits() nopanic;
#[inline(always)]
fn bool_and(a: bool, b: bool) -> bool implicits() nopanic {
    let (r,) = bool_and_impl(a, b);
    r
}

extern fn bool_or_impl(a: bool, b: bool) -> (bool,) implicits() nopanic;
#[inline(always)]
fn bool_or(a: bool, b: bool) -> bool implicits() nopanic {
    let (r,) = bool_or_impl(a, b);
    r
}

extern fn bool_not_impl(a: bool) -> (bool,) implicits() nopanic;
#[inline(always)]
fn bool_not(a: bool) -> bool implicits() nopanic {
    let (r,) = bool_not_impl(a);
    r
}

extern fn bool_xor_impl(a: bool, b: bool) -> (bool,) implicits() nopanic;
#[inline(always)]
fn bool_xor(a: bool, b: bool) -> bool implicits() nopanic {
    let (r,) = bool_xor_impl(a, b);
    r
}

extern fn bool_eq(a: bool, b: bool) -> bool implicits() nopanic;

#[inline(always)]
fn bool_ne(a: bool, b: bool) -> bool implicits() nopanic {
    !(a == b)
}

// Felt.
extern type RangeCheck;

#[derive(Copy, Drop)]
extern type felt;
extern fn felt_const<value>() -> felt nopanic;

// TODO(spapini): Make unnamed.
impl FeltCopy of Copy::<felt>;
impl FeltDrop of Drop::<felt>;

extern fn felt_add(a: felt, b: felt) -> felt nopanic;
extern fn felt_sub(a: felt, b: felt) -> felt nopanic;
extern fn felt_mul(a: felt, b: felt) -> felt nopanic;
#[inline(always)]
fn felt_neg(a: felt) -> felt nopanic {
    a * felt_const::<-1>()
}

extern type NonZero<T>;
// TODO(spapini): Add generic impls for NonZero for Copy, Drop.
enum JumpNzResult<T> { Zero: (), NonZero: NonZero::<T>, }
extern fn unwrap_nz<T>(a: NonZero::<T>) -> T nopanic;

impl NonZeroFeltCopy of Copy::<NonZero::<felt>>;
impl NonZeroFeltDrop of Drop::<NonZero::<felt>>;
extern fn felt_div(a: felt, b: NonZero::<felt>) -> felt nopanic;

// TODO(orizi): Change to extern when added.
#[inline(always)]
fn felt_eq(a: felt, b: felt) -> bool nopanic {
    match a - b {
        0 => bool::True(()),
        _ => bool::False(()),
    }
}
#[inline(always)]
fn felt_ne(a: felt, b: felt) -> bool nopanic {
    !(a == b)
}

#[inline(always)]
fn felt_lt(a: felt, b: felt) -> bool implicits(RangeCheck) {
    u256_from_felt(a) < u256_from_felt(b)
}

#[inline(always)]
fn felt_gt(a: felt, b: felt) -> bool implicits(RangeCheck) {
    b < a
}

#[inline(always)]
fn felt_le(a: felt, b: felt) -> bool implicits(RangeCheck) {
    !(b < a)
}

#[inline(always)]
fn felt_ge(a: felt, b: felt) -> bool implicits(RangeCheck) {
    !(a < b)
}

extern fn felt_jump_nz(a: felt) -> JumpNzResult::<felt> nopanic;

// TODO(spapini): Constraint using Copy and Drop traits.
extern fn dup<T>(obj: T) -> (T, T) nopanic;
extern fn drop<T>(obj: T) nopanic;

// Boxes.
mod box;
use box::Box;
use box::into_box;
use box::unbox;

// Nullable
mod nullable;
use nullable::FromNullableResult;
use nullable::Nullable;
use nullable::null;
use nullable::into_nullable;
use nullable::from_nullable;

// Arrays.
mod array;
use array::Array;
use array::array_new;
use array::array_append;
use array::array_pop_front;
use array::array_get;
use array::array_at;
use array::array_len;

// Dictionary.
mod dict;
use dict::DictFeltTo;
use dict::SquashedDictFeltTo;
use dict::dict_felt_to_new;
use dict::dict_felt_to_write;
use dict::dict_felt_to_read;
use dict::dict_felt_to_squash;

// Result.
mod result;
use result::Result;

// Option.
mod option;
use option::Option;
use option::option_is_none;
use option::option_is_some;
use option::option_unwrap;

// EC.
mod ec;
use ec::EcOp;
use ec::EcPoint;
use ec::EcState;
use ec::ec_mul;
use ec::ec_neg;
use ec::ec_point_from_x;
use ec::ec_point_new;
use ec::ec_point_try_new;
use ec::ec_point_unwrap;
use ec::ec_state_add_mul;
use ec::ec_state_add;
use ec::ec_state_finalize_nonzero;
use ec::ec_state_finalize;
use ec::ec_state_init;

// Integer.
mod integer;
use integer::u128;
use integer::u128_const;
use integer::u128_from_felt;
use integer::u128_try_from_felt;
use integer::u128_to_felt;
use integer::u128_add;
use integer::u128_sub;
use integer::u128_mul;
use integer::u128_as_non_zero;
use integer::u128_div;
use integer::u128_mod;
use integer::u128_lt;
use integer::u128_le;
use integer::u128_gt;
use integer::u128_ge;
use integer::u128_eq;
use integer::u128_ne;
use integer::u128_and;
use integer::u128_or;
use integer::u128_xor;
use integer::u128_jump_nz;
use integer::u8;
use integer::u8_const;
use integer::u8_add;
use integer::u8_sub;
use integer::u8_eq;
use integer::u8_ne;
use integer::u8_lt;
use integer::u8_le;
use integer::u8_gt;
use integer::u8_ge;
use integer::u256;
use integer::u256_add;
use integer::u256_sub;
use integer::u256_mul;
use integer::u256_eq;
use integer::u256_ne;
use integer::u256_lt;
use integer::u256_le;
use integer::u256_gt;
use integer::u256_ge;
use integer::u256_and;
use integer::u256_or;
use integer::u256_xor;
use integer::u256_from_felt;
use integer::Bitwise;

// Gas.
mod gas;
use gas::BuiltinCosts;
use gas::GasBuiltin;
use gas::get_builtin_costs;
use gas::get_gas;
use gas::get_gas_all;

// Panics.
enum PanicResult<T> { Ok: T, Err: Array::<felt>, }
enum never { }
extern fn panic(data: Array::<felt>) -> never;

fn assert(cond: bool, err_code: felt) {
    if !cond {
        let mut data = array_new::<felt>();
        array_append::<felt>(ref data, err_code);
        panic(data);
    }
}

// Serialization and Deserialization.
mod serde;

// Hash functions.
mod hash;
use hash::pedersen;
use hash::Pedersen;

// Debug.
mod debug;

// StarkNet
mod starknet;
use starknet::System;
use starknet::ContractAddress;

#[cfg(test)]
mod test;
