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

extern fn bool_eq(a: bool, b: bool) -> bool implicits() nopanic;

impl BoolPartialEq of PartialEq::<bool> {
    #[inline(always)]
    fn eq(a: bool, b: bool) -> bool {
        bool_eq(a, b)
    }
    #[inline(always)]
    fn ne(a: bool, b: bool) -> bool {
        !(a == b)
    }
}

// Felt.
extern type RangeCheck;

#[derive(Copy, Drop)]
extern type felt;
extern fn felt_const<const value>() -> felt nopanic;

// TODO(spapini): Make unnamed.
impl FeltCopy of Copy::<felt>;
impl FeltDrop of Drop::<felt>;

impl FeltAdd of Add::<felt> {
    #[inline(always)]
    fn add(a: felt, b: felt) -> felt {
        felt_add(a, b)
    }
}
impl FeltAddEq of AddEq::<felt> {
    #[inline(always)]
    fn add_eq(ref self: felt, other: felt) {
        self = Add::add(self, other);
    }
}

extern fn felt_add(a: felt, b: felt) -> felt nopanic;
impl FeltSub of Sub::<felt> {
    #[inline(always)]
    fn sub(a: felt, b: felt) -> felt {
        felt_sub(a, b)
    }
}
impl FeltSubEq of SubEq::<felt> {
    #[inline(always)]
    fn sub_eq(ref self: felt, other: felt) {
        self = Sub::sub(self, other);
    }
}

extern fn felt_sub(a: felt, b: felt) -> felt nopanic;
impl FeltMul of Mul::<felt> {
    #[inline(always)]
    fn mul(a: felt, b: felt) -> felt {
        felt_mul(a, b)
    }
}
impl FeltMulEq of MulEq::<felt> {
    #[inline(always)]
    fn mul_eq(ref self: felt, other: felt) {
        self = Mul::mul(self, other);
    }
}

extern fn felt_mul(a: felt, b: felt) -> felt nopanic;

impl FeltNeg of Neg::<felt> {
    #[inline(always)]
    fn neg(a: felt) -> felt {
        a * felt_const::<-1>()
    }
}

extern type NonZero<T>;
// TODO(spapini): Add generic impls for NonZero for Copy, Drop.
enum IsZeroResult<T> {
    Zero: (),
    NonZero: NonZero::<T>,
}
extern fn unwrap_nz<T>(a: NonZero::<T>) -> T nopanic;

impl IsZeroResultIntoBool<T> of Into::<IsZeroResult::<T>, bool> {
    fn into(self: IsZeroResult::<T>) -> bool {
        match self {
            IsZeroResult::Zero(()) => true,
            IsZeroResult::NonZero(_) => false,
        }
    }
}

impl NonZeroFeltCopy of Copy::<NonZero::<felt>>;
impl NonZeroFeltDrop of Drop::<NonZero::<felt>>;
extern fn felt_div(a: felt, b: NonZero::<felt>) -> felt nopanic;

impl FeltPartialEq of PartialEq::<felt> {
    #[inline(always)]
    fn eq(a: felt, b: felt) -> bool {
        match a - b {
            0 => bool::True(()),
            _ => bool::False(()),
        }
    }
    #[inline(always)]
    fn ne(a: felt, b: felt) -> bool {
        !(a == b)
    }
}

impl PartialOrdFelt of PartialOrd::<felt> {
    #[inline(always)]
    fn le(a: felt, b: felt) -> bool {
        !(b < a)
    }
    #[inline(always)]
    fn ge(a: felt, b: felt) -> bool {
        !(a < b)
    }
    #[inline(always)]
    fn lt(a: felt, b: felt) -> bool {
        integer::u256_from_felt(a) < integer::u256_from_felt(b)
    }
    #[inline(always)]
    fn gt(a: felt, b: felt) -> bool {
        b < a
    }
}

extern fn felt_is_zero(a: felt) -> IsZeroResult::<felt> nopanic;

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
use array::ArrayTrait;
use array::ArrayImpl;
impl ArrayFeltDrop of Drop::<Array::<felt>>;
type usize = u32;

// Dictionary.
mod dict;
use dict::DictFeltTo;
use dict::SquashedDictFeltTo;
use dict::dict_felt_to_new;
use dict::dict_felt_to_write;
use dict::dict_felt_to_read;
use dict::dict_felt_to_squash;
use dict::DictFeltToTrait;
use dict::DictFeltToImpl;

// Result.
mod result;
use result::Result;

// Option.
mod option;
use option::Option;
use option::OptionUnitCopy;
use option::OptionUnitDrop;

// EC.
mod ec;
use ec::EcOp;
use ec::EcPoint;
use ec::EcPointAdd;
use ec::EcPointSub;
use ec::EcState;
use ec::NonZeroEcPoint;
use ec::NonZeroEcPointCopy;
use ec::OptionNonZeroEcPointCopy;
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
use integer::upcast;
use integer::U128Add;
use integer::U128Sub;
use integer::U128Mul;
use integer::U128Div;
use integer::U128Rem;
use integer::U128PartialOrd;
use integer::U128PartialEq;
use integer::U128BitAnd;
use integer::U128BitOr;
use integer::U128BitXor;
use integer::u128_is_zero;
use integer::u8;
use integer::u8_const;
use integer::U8Add;
use integer::U8Div;
use integer::U8PartialEq;
use integer::U8PartialOrd;
use integer::U8Rem;
use integer::U8Sub;
use integer::U8Mul;
use integer::u16;
use integer::u16_const;
use integer::U16Add;
use integer::U16Div;
use integer::U16PartialEq;
use integer::U16PartialOrd;
use integer::U16Rem;
use integer::U16Sub;
use integer::U16Mul;
use integer::u32;
use integer::u32_const;
use integer::U32Add;
use integer::U32Div;
use integer::U32PartialEq;
use integer::U32PartialOrd;
use integer::U32Rem;
use integer::U32Sub;
use integer::U32Mul;
use integer::u64;
use integer::u64_const;
use integer::U64Add;
use integer::U64Div;
use integer::U64PartialEq;
use integer::U64PartialOrd;
use integer::U64Rem;
use integer::U64Sub;
use integer::U64Mul;
use integer::u256;
use integer::U256Add;
use integer::U256Sub;
use integer::U256Mul;
use integer::U256PartialOrd;
use integer::U256PartialEq;
use integer::U256BitAnd;
use integer::U256BitOr;
use integer::U256BitXor;
use integer::FeltTryIntoU8;
use integer::U8IntoFelt;
use integer::FeltTryIntoU16;
use integer::U16IntoFelt;
use integer::FeltTryIntoU32;
use integer::U32IntoFelt;
use integer::FeltTryIntoU64;
use integer::U64IntoFelt;
use integer::FeltTryIntoU128;
use integer::U128IntoFelt;
use integer::U16TryIntoU8;
use integer::U32TryIntoU16;
use integer::U64TryIntoU32;
use integer::U128TryIntoU64;
use integer::FeltIntoU256;
use integer::Bitwise;

// Gas.
mod gas;
use gas::BuiltinCosts;
use gas::GasBuiltin;
use gas::get_builtin_costs;
use gas::get_gas;
use gas::get_gas_all;

// Panics.
enum PanicResult<T> {
    Ok: T,
    Err: Array::<felt>,
}
enum never {}
extern fn panic(data: Array::<felt>) -> never;

fn assert(cond: bool, err_code: felt) {
    if !cond {
        let mut data = ArrayTrait::new();
        data.append(err_code);
        panic(data);
    }
}

// Serialization and Deserialization.
mod serde;
mod starknet_serde;

// Hash functions.
mod hash;
use hash::pedersen;
use hash::Pedersen;

// Debug.
mod debug;

// Starknet
mod starknet;
use starknet::System;
use starknet::ContractAddress;

// Internals.
mod internal;

mod zeroable;
use zeroable::Zeroable;

#[cfg(test)]
mod test;
