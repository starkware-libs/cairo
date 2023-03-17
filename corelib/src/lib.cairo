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
use traits::Not;
use traits::Neg;
use traits::Into;
use traits::TryInto;

#[derive(Copy, Drop)]
enum bool {
    False: (),
    True: (),
}

extern fn bool_and_impl(a: bool, b: bool) -> (bool, ) implicits() nopanic;
impl BoolBitAnd of BitAnd::<bool> {
    #[inline(always)]
    fn bitand(a: bool, b: bool) -> bool {
        let (r, ) = bool_and_impl(a, b);
        r
    }
}

extern fn bool_or_impl(a: bool, b: bool) -> (bool, ) implicits() nopanic;
impl BoolBitOr of BitOr::<bool> {
    #[inline(always)]
    fn bitor(a: bool, b: bool) -> bool {
        let (r, ) = bool_or_impl(a, b);
        r
    }
}

extern fn bool_not_impl(a: bool) -> (bool, ) implicits() nopanic;
#[inline(always)]
impl BoolNot of Not::<bool> {
    #[inline(always)]
    fn not(a: bool) -> bool implicits() nopanic {
        let (r, ) = bool_not_impl(a);
        r
    }
}

extern fn bool_xor_impl(a: bool, b: bool) -> (bool, ) implicits() nopanic;
impl BoolBitXor of BitXor::<bool> {
    #[inline(always)]
    fn bitxor(a: bool, b: bool) -> bool {
        let (r, ) = bool_xor_impl(a, b);
        r
    }
}

impl BoolPartialEq of PartialEq::<bool> {
    #[inline(always)]
    fn eq(a: bool, b: bool) -> bool {
        bool_to_felt252(a) == bool_to_felt252(b)
    }
    #[inline(always)]
    fn ne(a: bool, b: bool) -> bool {
        !(a == b)
    }
}

extern fn bool_to_felt252(a: bool) -> felt252 implicits() nopanic;

// General purpose implicits.
extern type RangeCheck;
extern type SegmentArena;

// felt252.
#[derive(Copy, Drop)]
extern type felt252;
extern fn felt252_const<const value>() -> felt252 nopanic;

impl Felt252Add of Add::<felt252> {
    #[inline(always)]
    fn add(a: felt252, b: felt252) -> felt252 {
        felt252_add(a, b)
    }
}
impl Felt252AddEq of AddEq::<felt252> {
    #[inline(always)]
    fn add_eq(ref self: felt252, other: felt252) {
        self = Add::add(self, other);
    }
}

extern fn felt252_add(a: felt252, b: felt252) -> felt252 nopanic;
impl Felt252Sub of Sub::<felt252> {
    #[inline(always)]
    fn sub(a: felt252, b: felt252) -> felt252 {
        felt252_sub(a, b)
    }
}
impl Felt252SubEq of SubEq::<felt252> {
    #[inline(always)]
    fn sub_eq(ref self: felt252, other: felt252) {
        self = Sub::sub(self, other);
    }
}

extern fn felt252_sub(a: felt252, b: felt252) -> felt252 nopanic;
impl Felt252Mul of Mul::<felt252> {
    #[inline(always)]
    fn mul(a: felt252, b: felt252) -> felt252 {
        felt252_mul(a, b)
    }
}
impl Felt252MulEq of MulEq::<felt252> {
    #[inline(always)]
    fn mul_eq(ref self: felt252, other: felt252) {
        self = Mul::mul(self, other);
    }
}

extern fn felt252_mul(a: felt252, b: felt252) -> felt252 nopanic;

impl Felt252Neg of Neg::<felt252> {
    #[inline(always)]
    fn neg(a: felt252) -> felt252 {
        a * felt252_const::<-1>()
    }
}

extern type NonZero<T>;
impl NonZeroTCopy<T, impl TCopy: Copy::<T>> of Copy::<NonZero::<T>>;
impl NonZeroTDrop<T, impl TDrop: Drop::<T>> of Drop::<NonZero::<T>>;
enum IsZeroResult<T> {
    Zero: (),
    NonZero: NonZero<T>,
}
extern fn unwrap_non_zero<T>(a: NonZero<T>) -> T nopanic;

impl IsZeroResultIntoBool<T, impl TDrop: Drop::<T>> of Into::<IsZeroResult<T>, bool> {
    fn into(self: IsZeroResult<T>) -> bool {
        match self {
            IsZeroResult::Zero(()) => true,
            IsZeroResult::NonZero(_) => false,
        }
    }
}

extern fn felt252_div(a: felt252, b: NonZero<felt252>) -> felt252 nopanic;

impl Felt252PartialEq of PartialEq::<felt252> {
    #[inline(always)]
    fn eq(a: felt252, b: felt252) -> bool {
        match a - b {
            0 => bool::True(()),
            _ => bool::False(()),
        }
    }
    #[inline(always)]
    fn ne(a: felt252, b: felt252) -> bool {
        !(a == b)
    }
}

extern fn felt252_is_zero(a: felt252) -> IsZeroResult<felt252> nopanic;

// TODO(spapini): Constraint using Copy and Drop traits.
extern fn dup<T>(obj: T) -> (T, T) nopanic;
extern fn drop<T>(obj: T) nopanic;

// Boxes.
mod box;
use box::Box;
use box::BoxTrait;

// Nullable
mod nullable;
use nullable::FromNullableResult;
use nullable::Nullable;
use nullable::match_nullable;
use nullable::null;
use nullable::nullable_from_box;

// Arrays.
mod array;
use array::Array;
use array::ArrayTrait;
use array::ArrayImpl;
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
use dict::Felt252DictImpl;

// Result.
mod result;
use result::Result;

// Option.
mod option;
use option::Option;
use option::OptionCopy;
use option::OptionDrop;

// Clone.
mod clone;
use clone::Clone;
use clone::TCopyClone;

// EC.
mod ec;
use ec::EcOp;
use ec::EcPoint;
use ec::EcPointAdd;
use ec::EcPointSub;
use ec::EcState;
use ec::NonZeroEcPoint;
use ec::ec_mul;
use ec::ec_neg;
use ec::ec_point_from_x;
use ec::ec_point_from_x_nz;
use ec::ec_point_is_zero;
use ec::ec_point_new;
use ec::ec_point_new_nz;
use ec::ec_point_non_zero;
use ec::ec_point_try_new;
use ec::ec_point_try_new_nz;
use ec::ec_point_unwrap;
use ec::ec_point_zero;
use ec::ec_state_add_mul;
use ec::ec_state_add;
use ec::ec_state_finalize;
use ec::ec_state_init;
use ec::ec_state_try_finalize_nz;

mod ecdsa;

// Integer.
mod integer;
use integer::u128;
use integer::u128_const;
use integer::u128_sqrt;
use integer::U128Add;
use integer::U128Sub;
use integer::U128Mul;
use integer::U128Div;
use integer::U128Rem;
use integer::U128AddEq;
use integer::U128SubEq;
use integer::U128MulEq;
use integer::U128DivEq;
use integer::U128RemEq;
use integer::U128PartialOrd;
use integer::U128PartialEq;
use integer::U128BitAnd;
use integer::U128BitOr;
use integer::U128BitXor;
use integer::u128_is_zero;
use integer::u8;
use integer::u8_const;
use integer::U8Add;
use integer::U8Sub;
use integer::U8Mul;
use integer::U8Div;
use integer::U8Rem;
use integer::U8AddEq;
use integer::U8SubEq;
use integer::U8MulEq;
use integer::U8DivEq;
use integer::U8RemEq;
use integer::U8PartialEq;
use integer::U8PartialOrd;
use integer::u16;
use integer::u16_const;
use integer::U16Add;
use integer::U16Sub;
use integer::U16Mul;
use integer::U16Div;
use integer::U16Rem;
use integer::U16AddEq;
use integer::U16SubEq;
use integer::U16MulEq;
use integer::U16DivEq;
use integer::U16RemEq;
use integer::U16PartialEq;
use integer::U16PartialOrd;
use integer::u32;
use integer::u32_const;
use integer::U32Add;
use integer::U32Sub;
use integer::U32Mul;
use integer::U32Div;
use integer::U32Rem;
use integer::U32AddEq;
use integer::U32SubEq;
use integer::U32MulEq;
use integer::U32DivEq;
use integer::U32RemEq;
use integer::U32PartialEq;
use integer::U32PartialOrd;
use integer::u64;
use integer::u64_const;
use integer::U64Add;
use integer::U64Sub;
use integer::U64Mul;
use integer::U64Div;
use integer::U64Rem;
use integer::U64AddEq;
use integer::U64SubEq;
use integer::U64MulEq;
use integer::U64DivEq;
use integer::U64RemEq;
use integer::U64PartialEq;
use integer::U64PartialOrd;
use integer::u256;
use integer::U256Add;
use integer::U256Sub;
use integer::U256Mul;
use integer::U256AddEq;
use integer::U256SubEq;
use integer::U256MulEq;
use integer::U256PartialOrd;
use integer::U256PartialEq;
use integer::U256BitAnd;
use integer::U256BitOr;
use integer::U256BitXor;
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

fn assert(cond: bool, err_code: felt252) {
    if !cond {
        let mut data = ArrayTrait::new();
        data.append(err_code);
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

// Starknet
mod starknet;
use starknet::System;

// Internals.
mod internal;

mod zeroable;
use zeroable::Zeroable;

#[cfg(test)]
mod test;

// Module for testing only.
mod testing;

// Tuple Copy and Drop impls.
impl TupleSize0Copy of Copy::<()>;
impl TupleSize0Drop of Drop::<()>;

impl TupleSize1Copy<E0, impl E0Copy: Copy::<E0>> of Copy::<(E0, )>;
impl TupleSize1Drop<E0, impl E0Drop: Drop::<E0>> of Drop::<(E0, )>;

impl TupleSize2Copy<E0, E1, impl E0Copy: Copy::<E0>, impl E1Copy: Copy::<E1>> of Copy::<(E0, E1)>;
impl TupleSize2Drop<E0, E1, impl E0Drop: Drop::<E0>, impl E1Drop: Drop::<E1>> of Drop::<(E0, E1)>;

impl TupleSize3Copy<E0,
E1,
E2,
impl E0Copy: Copy::<E0>,
impl E1Copy: Copy::<E1>,
impl E2Copy: Copy::<E2>> of Copy::<(E0, E1, E2)>;
impl TupleSize3Drop<E0,
E1,
E2,
impl E0Drop: Drop::<E0>,
impl E1Drop: Drop::<E1>,
impl E2Drop: Drop::<E2>> of Drop::<(E0, E1, E2)>;

impl TupleSize4Copy<E0,
E1,
E2,
E3,
impl E0Copy: Copy::<E0>,
impl E1Copy: Copy::<E1>,
impl E2Copy: Copy::<E2>,
impl E3Copy: Copy::<E3>> of Copy::<(E0, E1, E2, E3)>;
impl TupleSize4Drop<E0,
E1,
E2,
E3,
impl E0Drop: Drop::<E0>,
impl E1Drop: Drop::<E1>,
impl E2Drop: Drop::<E2>,
impl E2Drop: Drop::<E3>> of Drop::<(E0, E1, E2, E3)>;
