mod traits;
use traits::Copy;
use traits::Drop;

enum bool { False: (), True: (), }
// TODO(spapini): Make unnamed.
impl BoolCopy of Copy::<bool>;
impl BoolDrop of Drop::<bool>;

extern func bool_and(a: bool, b: bool) -> bool nopanic;
extern func bool_or(a: bool, b: bool) -> bool nopanic;
extern func bool_not(a: bool) -> bool nopanic;

extern type RangeCheck;

extern type felt;
// TODO(spapini): Make unnamed.
impl FeltCopy of Copy::<felt>;
impl FeltDrop of Drop::<felt>;

extern func felt_add(a: felt, b: felt) -> felt nopanic;
extern func felt_sub(a: felt, b: felt) -> felt nopanic;
extern func felt_mul(a: felt, b: felt) -> felt nopanic;

extern type NonZero<T>;
// TODO(spapini): Add generic impls for NonZero for Copy, Drop.
enum JumpNzResult<T> { Zero: (), NonZero: NonZero::<T>, }
extern func unwrap_nz<T>(a: NonZero::<T>) -> T nopanic;

extern func felt_div(a: felt, b: NonZero::<felt>) -> felt nopanic;

// TODO(orizi): Consider removing and replacing with `jump_nz(a - b)`.
extern func felt_eq(a: felt, b: felt) -> bool nopanic;
extern func felt_le(a: felt, b: felt) -> bool implicits (rc: RangeCheck) nopanic;
extern func felt_ge(a: felt, b: felt) -> bool implicits (rc: RangeCheck) nopanic;
extern func felt_lt(a: felt, b: felt) -> bool implicits (rc: RangeCheck) nopanic;
extern func felt_gt(a: felt, b: felt) -> bool implicits (rc: RangeCheck) nopanic;

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
use integer::uint128_lt;

use integer::uint128_jump_nz;

// Gas.
mod gas;
use gas::GasBuiltin;
use gas::get_gas;

// Panics.
enum PanicResult<T> { Ok: T, Err: Array::<felt>, }
extern func panic(data: Array::<felt>);

// Hash functions.
mod hash;
use hash::pedersen;
