mod traits;
use traits::Copy;
use traits::Drop;

enum bool { False: (), True: (), }
// TODO(spapini): Make unnamed.
impl BoolCopy of Copy::<bool>;
impl BoolDrop of Drop::<bool>;

// TODO(orizi): Change to extern when added.
func bool_and(a: bool, b: bool) -> bool implicits() nopanic {
    match a {
        bool::False(x) => bool::False(()),
        bool::True(x) => b,
    }
}

// TODO(orizi): Change to extern when added.
func bool_or(a: bool, b: bool) -> bool implicits() nopanic {
    match a {
        bool::False(x) => b,
        bool::True(x) => bool::True(()),
    }
}

// TODO(orizi): Change to extern when added.
func bool_not(a: bool) -> bool implicits() nopanic {
    match a {
        bool::False(x) => bool::True(()),
        bool::True(x) => bool::False(()),
    }
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

extern type RangeCheck;

extern type felt;
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

extern func felt_div(a: felt, b: NonZero::<felt>) -> felt nopanic;

// TODO(orizi): Change to extern when added.
func felt_eq(a: felt, b: felt) -> bool {
    match a - b {
        0 => bool::True(()),
        _ => bool::False(()),
    }
}

// TODO(orizi): Change to extern when added.
func felt_lt(a: felt, b: felt) -> bool implicits(rc: RangeCheck) {
    uint128_lt(uint128_from_felt(a), uint128_from_felt(b))
}

func felt_gt(a: felt, b: felt) -> bool implicits(rc: RangeCheck) {
    felt_lt(b, a)
}

// TODO(orizi): Change to extern when added.
func felt_le(a: felt, b: felt) -> bool implicits(rc: RangeCheck) {
    bool_not(felt_gt(a, b))
}

func felt_ge(a: felt, b: felt) -> bool implicits(rc: RangeCheck) {
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

// Result.
mod result;
use result::Result;

// Option.
mod option;
use option::Option;

// Integer.
mod integer;
use integer::uint128;
use integer::uint128_from_felt;
use integer::uint128_to_felt;
use integer::uint128_add;
use integer::uint128_sub;
use integer::uint128_mul;
use integer::uint128_div;
use integer::uint128_mod;
use integer::uint128_divmod;
use integer::uint128_lt;
use integer::uint128_le;
use integer::uint128_gt;
use integer::uint128_ge;
use integer::uint128_eq;

use integer::uint128_jump_nz;

// Gas.
mod gas;
use gas::GasBuiltin;
use gas::get_gas;

// Panics.
enum PanicResult<T> { Ok: T, Err: Array::<felt>, }
enum never { }
extern func panic(data: Array::<felt>) -> never;

func assert(cond: bool, err_code: felt) {
    if cond {
    } else {
        let data = array_new::<felt>();
        array_append::<felt>(data, err_code);
        panic(data);
    }
}


// Hash functions.
mod hash;
use hash::pedersen;
