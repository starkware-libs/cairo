use super::signed128::Sint128Type;
use super::{
    IntConstLibfunc, IntEqualLibfunc, IntFromFelt252Libfunc, IntMulTraits, IntToFelt252Libfunc,
    IntTraits, IntType, IntWideMulLibfunc,
};
use crate::define_libfunc_hierarchy;
use crate::extensions::is_zero::{IsZeroLibfunc, IsZeroTraits};
use crate::extensions::{GenericLibfunc, NamedType};
use crate::ids::GenericTypeId;

/// Trait for implementing signed integers.
pub trait SintTraits: IntTraits {}

define_libfunc_hierarchy! {
    pub enum SintLibfunc<TSintTraits: SintTraits + IntMulTraits + IsZeroTraits> {
        Const(IntConstLibfunc<TSintTraits>),
        Equal(IntEqualLibfunc<TSintTraits>),
        ToFelt252(IntToFelt252Libfunc<TSintTraits>),
        FromFelt252(IntFromFelt252Libfunc<TSintTraits>),
        IsZero(IsZeroLibfunc<TSintTraits>),
        WideMul(IntWideMulLibfunc<TSintTraits>),
    }, SintConcrete
}

#[derive(Default)]
pub struct Sint8Traits;

impl SintTraits for Sint8Traits {}

impl IntTraits for Sint8Traits {
    type IntType = i8;
    const GENERIC_TYPE_ID: GenericTypeId = GenericTypeId::new_inline("i8");
    const IS_SMALL: bool = true;
    const CONST: &'static str = "i8_const";
    const EQUAL: &'static str = "i8_eq";
    const TO_FELT252: &'static str = "i8_to_felt252";
    const TRY_FROM_FELT252: &'static str = "i8_try_from_felt252";
}

impl IntMulTraits for Sint8Traits {
    const WIDE_MUL: &'static str = "i8_wide_mul";
    const WIDE_MUL_RES_TYPE_ID: GenericTypeId = <Sint16Type as NamedType>::ID;
}

impl IsZeroTraits for Sint8Traits {
    const IS_ZERO: &'static str = "i8_is_zero";
    const GENERIC_TYPE_ID: GenericTypeId = <Sint8Type as NamedType>::ID;
}

/// Type for i8.
pub type Sint8Type = IntType<Sint8Traits>;
pub type Sint8Libfunc = SintLibfunc<Sint8Traits>;
pub type Sint8Concrete = <Sint8Libfunc as GenericLibfunc>::Concrete;

#[derive(Default)]
pub struct Sint16Traits;

impl SintTraits for Sint16Traits {}

impl IntTraits for Sint16Traits {
    type IntType = i16;
    const GENERIC_TYPE_ID: GenericTypeId = GenericTypeId::new_inline("i16");
    const IS_SMALL: bool = true;
    const CONST: &'static str = "i16_const";
    const EQUAL: &'static str = "i16_eq";
    const TO_FELT252: &'static str = "i16_to_felt252";
    const TRY_FROM_FELT252: &'static str = "i16_try_from_felt252";
}

impl IntMulTraits for Sint16Traits {
    const WIDE_MUL: &'static str = "i16_wide_mul";
    const WIDE_MUL_RES_TYPE_ID: GenericTypeId = <Sint32Type as NamedType>::ID;
}

impl IsZeroTraits for Sint16Traits {
    const IS_ZERO: &'static str = "i16_is_zero";
    const GENERIC_TYPE_ID: GenericTypeId = <Sint16Type as NamedType>::ID;
}

/// Type for i16.
pub type Sint16Type = IntType<Sint16Traits>;
pub type Sint16Libfunc = SintLibfunc<Sint16Traits>;
pub type Sint16Concrete = <Sint16Libfunc as GenericLibfunc>::Concrete;

#[derive(Default)]
pub struct Sint32Traits;

impl SintTraits for Sint32Traits {}

impl IntTraits for Sint32Traits {
    type IntType = i32;
    const GENERIC_TYPE_ID: GenericTypeId = GenericTypeId::new_inline("i32");
    const IS_SMALL: bool = true;
    const CONST: &'static str = "i32_const";
    const EQUAL: &'static str = "i32_eq";
    const TO_FELT252: &'static str = "i32_to_felt252";
    const TRY_FROM_FELT252: &'static str = "i32_try_from_felt252";
}

impl IntMulTraits for Sint32Traits {
    const WIDE_MUL: &'static str = "i32_wide_mul";
    const WIDE_MUL_RES_TYPE_ID: GenericTypeId = <Sint64Type as NamedType>::ID;
}

impl IsZeroTraits for Sint32Traits {
    const IS_ZERO: &'static str = "i32_is_zero";
    const GENERIC_TYPE_ID: GenericTypeId = <Sint32Type as NamedType>::ID;
}

/// Type for i32.
pub type Sint32Type = IntType<Sint32Traits>;
pub type Sint32Libfunc = SintLibfunc<Sint32Traits>;
pub type Sint32Concrete = <Sint32Libfunc as GenericLibfunc>::Concrete;

#[derive(Default)]
pub struct Sint64Traits;

impl SintTraits for Sint64Traits {}

impl IntTraits for Sint64Traits {
    type IntType = i64;
    const GENERIC_TYPE_ID: GenericTypeId = GenericTypeId::new_inline("i64");
    const IS_SMALL: bool = true;
    const CONST: &'static str = "i64_const";
    const EQUAL: &'static str = "i64_eq";
    const TO_FELT252: &'static str = "i64_to_felt252";
    const TRY_FROM_FELT252: &'static str = "i64_try_from_felt252";
}

impl IntMulTraits for Sint64Traits {
    const WIDE_MUL: &'static str = "i64_wide_mul";
    const WIDE_MUL_RES_TYPE_ID: GenericTypeId = <Sint128Type as NamedType>::ID;
}

impl IsZeroTraits for Sint64Traits {
    const IS_ZERO: &'static str = "i64_is_zero";
    const GENERIC_TYPE_ID: GenericTypeId = <Sint64Type as NamedType>::ID;
}

/// Type for i64.
pub type Sint64Type = IntType<Sint64Traits>;
pub type Sint64Libfunc = SintLibfunc<Sint64Traits>;
pub type Sint64Concrete = <Sint64Libfunc as GenericLibfunc>::Concrete;
