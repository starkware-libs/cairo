use super::signed::{SintDiffLibfunc, SintOperationLibfunc, SintTraits};
use super::unsigned128::Uint128Type;
use super::{
    IntConstLibfunc, IntEqualLibfunc, IntFromFelt252Libfunc, IntToFelt252Libfunc, IntTraits,
    IntType,
};
use crate::define_libfunc_hierarchy;
use crate::extensions::is_zero::{IsZeroLibfunc, IsZeroTraits};
use crate::extensions::NamedType;
use crate::ids::GenericTypeId;

/// Type for i128.
pub type Sint128Type = IntType<Sint128Traits>;

define_libfunc_hierarchy! {
    pub enum Sint128Libfunc {
        Equal(IntEqualLibfunc<Sint128Traits>),
        Const(IntConstLibfunc<Sint128Traits>),
        ToFelt252(IntToFelt252Libfunc<Sint128Traits>),
        FromFelt252(IntFromFelt252Libfunc<Sint128Traits>),
        Operation(SintOperationLibfunc<Sint128Traits>),
        Diff(SintDiffLibfunc<Sint128Traits>),
        IsZero(IsZeroLibfunc<Sint128Traits>),
    }, Sint128Concrete
}

#[derive(Default)]
pub struct Sint128Traits;

impl SintTraits for Sint128Traits {
    const OVERFLOWING_ADD: &'static str = "i128_overflowing_add_impl";
    const OVERFLOWING_SUB: &'static str = "i128_overflowing_sub_impl";
    const DIFF: &'static str = "i128_diff";
    const UNSIGNED_INT_TYPE: GenericTypeId = <Uint128Type as NamedType>::ID;
}

impl IntTraits for Sint128Traits {
    type IntType = i128;
    const GENERIC_TYPE_ID: GenericTypeId = GenericTypeId::new_inline("i128");
    const IS_SMALL: bool = false;
    const CONST: &'static str = "i128_const";
    const EQUAL: &'static str = "i128_eq";
    const TO_FELT252: &'static str = "i128_to_felt252";
    const TRY_FROM_FELT252: &'static str = "i128_try_from_felt252";
}

impl IsZeroTraits for Sint128Traits {
    const IS_ZERO: &'static str = "i128_is_zero";
    const GENERIC_TYPE_ID: GenericTypeId = <Sint128Type as NamedType>::ID;
}
