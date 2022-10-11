enum bool {
    True: (),
    False: (),
}
extern func bool_and(a: bool, b: bool) -> bool;
extern func bool_or(a: bool, b: bool) -> bool;
extern func bool_not(a: bool) -> bool;

extern type RangeCheck;

extern type felt;
extern func felt_add(a: felt, b: felt) -> felt;
extern func felt_sub(a: felt, b: felt) -> felt;
extern func felt_mul(a: felt, b: felt) -> felt;

extern type NonZero < T >;
extern func unwrap_nz< T > (a: NonZero::<T>) -> T;
extern func felt_div(a: felt, b: NonZero::<felt>) -> felt;

extern func felt_eq(a: felt, b: felt) -> bool;
// TODO(spapini): Get RangeCheck implicit.
extern func felt_le(a: felt, b: felt) -> (bool);
extern func felt_ge(a: felt, b: felt) -> (bool);
extern func felt_lt(a: felt, b: felt) -> (bool);
extern func felt_gt(a: felt, b: felt) -> (bool);

extern func dup < T >(obj: T) -> (T, T);
extern func drop < T >(obj: T);

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
