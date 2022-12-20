mod traits;
use traits::Copy;
use traits::Drop;

enum bool { False: (), True: (), }
// TODO(spapini): Make unnamed.
impl BoolCopy of Copy::<bool>;
impl BoolDrop of Drop::<bool>;

// TODO(dorimedini): Once we can differentiate between the value-bool and the branch-bool, just do:
// extern func bool_and(a: bool, b: bool) -> bool implicits() nopanic;
// (this will also require renaming the libfunc from "bool_and_impl" back to "bool_and").
extern func bool_and_impl(ref a: bool, b: bool) implicits() nopanic;
func bool_and(mut a: bool, b: bool) -> bool implicits() nopanic {
    bool_and_impl(a, b);
    a
}

// TODO(orizi): Change to extern when added.
func bool_or(a: bool, b: bool) -> bool implicits() nopanic {
    match a {
        bool::False(x) => b,
        bool::True(x) => bool::True(()),
    }
}

// TODO(dorimedini): Once we can differentiate between the value-bool and the branch-bool, just do:
// extern func bool_not(a: bool) -> bool implicits() nopanic;
// (this will also require renaming the libfunc from "bool_not_impl" back to "bool_not").
extern func bool_not_impl(ref a: bool) implicits() nopanic;
func bool_not(mut a: bool) -> bool implicits() nopanic {
    bool_not_impl(a);
    a
}

// TODO(orizi): Change to extern when added.
func bool_xor(a: bool, b: bool) -> bool implicits() nopanic {
    match a {
        bool::False(x) => b,
        bool::True(x) => bool_not(b),
    }
}

// TODO(orizi): Change to extern when added.
func bool_eq(a: bool, b: bool) -> bool implicits() nopanic {
    match a {
        bool::False(x) => bool_not(b),
        bool::True(x) => b,
    }
}
func bool_ne(a: bool, b: bool) -> bool implicits() nopanic {
    !(a == b)
}

extern type RangeCheck;

extern type felt;
extern func felt_const<value>() -> felt nopanic;

// TODO(spapini): Make unnamed.
impl FeltCopy of Copy::<felt>;
impl FeltDrop of Drop::<felt>;

extern func felt_add(a: felt, b: felt) -> felt nopanic;
extern func felt_sub(a: felt, b: felt) -> felt nopanic;
extern func felt_mul(a: felt, b: felt) -> felt nopanic;
extern func felt_neg(a: felt) -> felt nopanic;

extern type NonZero<T>;
// TODO(spapini): Add generic impls for NonZero for Copy, Drop.
enum JumpNzResult<T> { Zero: (), NonZero: NonZero::<T>, }
extern func unwrap_nz<T>(a: NonZero::<T>) -> T nopanic;

impl NonZeroFeltCopy of Copy::<NonZero::<felt>>;
impl NonZeroFeltDrop of Drop::<NonZero::<felt>>;
extern func felt_div(a: felt, b: NonZero::<felt>) -> felt nopanic;

// TODO(orizi): Change to extern when added.
func felt_eq(a: felt, b: felt) -> bool nopanic {
    match a - b {
        0 => bool::True(()),
        _ => bool::False(()),
    }
}
func felt_ne(a: felt, b: felt) -> bool nopanic {
    !(a == b)
}

// TODO(orizi): Change to extern when added.
func felt_lt(a: felt, b: felt) -> bool implicits(RangeCheck) {
    u128_lt(u128_from_felt(a), u128_from_felt(b))
}

func felt_gt(a: felt, b: felt) -> bool implicits(RangeCheck) {
    felt_lt(b, a)
}

// TODO(orizi): Change to extern when added.
func felt_le(a: felt, b: felt) -> bool implicits(RangeCheck) {
    bool_not(felt_gt(a, b))
}

func felt_ge(a: felt, b: felt) -> bool implicits(RangeCheck) {
    felt_le(b, a)
}

extern func felt_jump_nz(a: felt) -> JumpNzResult::<felt> nopanic;

// TODO(spapini): Constraint using Copy and Drop traits.
extern func dup<T>(obj: T) -> (T, T) nopanic;
extern func drop<T>(obj: T) nopanic;

// Boxes.
mod box;
use box::Box;
use box::into_box;
use box::unbox;

// Arrays.
mod array;
use array::Array;
use array::array_new;
use array::array_append;
use array::array_at;
use array::array_len;

// Result.
mod result;
use result::Result;

// Option.
mod option;
use option::Option;

// Integer.
mod integer;
use integer::u128;
use integer::u128_const;
use integer::u128_from_felt;
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
use integer::u128_jump_nz;
use integer::u256;
use integer::u256_add;
use integer::u256_sub;
use integer::u256_mul;
use integer::u256_eq;
use integer::u256_ne;
use integer::u256_from_felt;

// Gas.
mod gas;
use gas::BuiltinCosts;
use gas::GasBuiltin;
use gas::get_gas;
use gas::get_gas_all;

// Panics.
enum PanicResult<T> { Ok: T, Err: Array::<felt>, }
enum never { }
extern func panic(data: Array::<felt>) -> never;

func assert(cond: bool, err_code: felt) {
    if !cond {
        let mut data = array_new::<felt>();
        array_append::<felt>(data, err_code);
        panic(data);
    }
}


// Hash functions.
mod hash;
use hash::pedersen;
use hash::Pedersen;

// StarkNet
mod starknet;
use starknet::System;

mod test;
