use std::marker::PhantomData;

use super::signed128::Sint128Type;
use super::unsigned::{Uint16Type, Uint32Type, Uint64Type, Uint8Type};
use super::{
    IntConstLibfunc, IntEqualLibfunc, IntFromFelt252Libfunc, IntMulTraits,
    IntOperationConcreteLibfunc, IntOperator, IntToFelt252Libfunc, IntTraits, IntType,
    IntWideMulLibfunc,
};
use crate::define_libfunc_hierarchy;
use crate::extensions::is_zero::{IsZeroLibfunc, IsZeroTraits};
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::range_check::RangeCheckType;
use crate::extensions::{
    GenericLibfunc, NamedType, NoGenericArgsGenericLibfunc, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::{GenericLibfuncId, GenericTypeId};
use crate::program::GenericArg;

/// Trait for implementing signed integers.
pub trait SintTraits: IntTraits {
    /// The generic libfunc id for addition.
    const OVERFLOWING_ADD: &'static str;
    /// The generic libfunc id for subtraction.
    const OVERFLOWING_SUB: &'static str;
    /// The generic libfunc id for difference of signed integers, logically equivalent to
    /// substraction of unsigned integers.
    const DIFF: &'static str;
    /// The generic type id of the equivalent unsigned integer type.
    const UNSIGNED_INT_TYPE: GenericTypeId;
}

define_libfunc_hierarchy! {
    pub enum SintLibfunc<TSintTraits: SintTraits + IntMulTraits + IsZeroTraits> {
        Const(IntConstLibfunc<TSintTraits>),
        Equal(IntEqualLibfunc<TSintTraits>),
        ToFelt252(IntToFelt252Libfunc<TSintTraits>),
        FromFelt252(IntFromFelt252Libfunc<TSintTraits>),
        Operation(SintOperationLibfunc<TSintTraits>),
        Diff(SintDiffLibfunc<TSintTraits>),
        IsZero(IsZeroLibfunc<TSintTraits>),
        WideMul(IntWideMulLibfunc<TSintTraits>),
    }, SintConcrete
}

/// Libfunc for integer operations.
pub struct SintOperationLibfunc<TSintTraits: SintTraits> {
    pub operator: IntOperator,
    _phantom: PhantomData<TSintTraits>,
}
impl<TSintTraits: SintTraits> SintOperationLibfunc<TSintTraits> {
    const OVERFLOWING_ADD: &'static str = TSintTraits::OVERFLOWING_ADD;
    const OVERFLOWING_SUB: &'static str = TSintTraits::OVERFLOWING_SUB;
    fn new(operator: IntOperator) -> Option<Self> {
        Some(Self { operator, _phantom: PhantomData::default() })
    }
}
impl<TSintTraits: SintTraits> GenericLibfunc for SintOperationLibfunc<TSintTraits> {
    type Concrete = IntOperationConcreteLibfunc;

    fn supported_ids() -> Vec<GenericLibfuncId> {
        vec![
            GenericLibfuncId::from(Self::OVERFLOWING_ADD),
            GenericLibfuncId::from(Self::OVERFLOWING_SUB),
        ]
    }

    fn by_id(id: &GenericLibfuncId) -> Option<Self> {
        match id.0.as_str() {
            id if id == Self::OVERFLOWING_ADD => Self::new(IntOperator::OverflowingAdd),
            id if id == Self::OVERFLOWING_SUB => Self::new(IntOperator::OverflowingSub),
            _ => None,
        }
    }

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        if !args.is_empty() {
            return Err(SpecializationError::WrongNumberOfGenericArgs);
        }
        let ty = context.get_concrete_type(TSintTraits::GENERIC_TYPE_ID, &[])?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;

        let ty_param = ParamSignature::new(ty.clone());
        let rc_output_info = OutputVarInfo::new_builtin(range_check_type.clone(), 0);
        let wrapping_result_info = OutputVarInfo {
            ty: ty.clone(),
            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
        };
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature::new(range_check_type).with_allow_add_const(),
                ty_param.clone(),
                ty_param,
            ],
            branch_signatures: vec![
                BranchSignature {
                    vars: vec![
                        rc_output_info.clone(),
                        OutputVarInfo { ty, ref_info: OutputVarReferenceInfo::SimpleDerefs },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![rc_output_info.clone(), wrapping_result_info.clone()],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![rc_output_info, wrapping_result_info],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(IntOperationConcreteLibfunc {
            operator: self.operator,
            signature: self.specialize_signature(context.upcast(), args)?,
        })
    }
}

/// Libfunc for integer difference calculation.
#[derive(Default)]
pub struct SintDiffLibfunc<TSintTraits: SintTraits> {
    _phantom: PhantomData<TSintTraits>,
}
impl<TSintTraits: SintTraits> NoGenericArgsGenericLibfunc for SintDiffLibfunc<TSintTraits> {
    const STR_ID: &'static str = TSintTraits::DIFF;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let signed_ty = context.get_concrete_type(TSintTraits::GENERIC_TYPE_ID, &[])?;
        let unsigned_ty = context.get_concrete_type(TSintTraits::UNSIGNED_INT_TYPE, &[])?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;

        let signed_ty_param = ParamSignature::new(signed_ty);
        let rc_output_info = OutputVarInfo::new_builtin(range_check_type.clone(), 0);
        let wrapping_result_ref_info = if TSintTraits::IS_SMALL {
            OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic)
        } else {
            OutputVarReferenceInfo::NewTempVar { idx: 0 }
        };
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature::new(range_check_type).with_allow_add_const(),
                signed_ty_param.clone(),
                signed_ty_param,
            ],
            branch_signatures: vec![
                BranchSignature {
                    vars: vec![
                        rc_output_info.clone(),
                        OutputVarInfo {
                            ty: unsigned_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![
                        rc_output_info,
                        OutputVarInfo { ty: unsigned_ty, ref_info: wrapping_result_ref_info },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

#[derive(Default)]
pub struct Sint8Traits;

impl SintTraits for Sint8Traits {
    const OVERFLOWING_ADD: &'static str = "i8_overflowing_add_impl";
    const OVERFLOWING_SUB: &'static str = "i8_overflowing_sub_impl";
    const DIFF: &'static str = "i8_diff";
    const UNSIGNED_INT_TYPE: GenericTypeId = <Uint8Type as NamedType>::ID;
}

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

impl SintTraits for Sint16Traits {
    const OVERFLOWING_ADD: &'static str = "i16_overflowing_add_impl";
    const OVERFLOWING_SUB: &'static str = "i16_overflowing_sub_impl";
    const DIFF: &'static str = "i16_diff";
    const UNSIGNED_INT_TYPE: GenericTypeId = <Uint16Type as NamedType>::ID;
}

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

impl SintTraits for Sint32Traits {
    const OVERFLOWING_ADD: &'static str = "i32_overflowing_add_impl";
    const OVERFLOWING_SUB: &'static str = "i32_overflowing_sub_impl";
    const DIFF: &'static str = "i32_diff";
    const UNSIGNED_INT_TYPE: GenericTypeId = <Uint32Type as NamedType>::ID;
}

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

impl SintTraits for Sint64Traits {
    const OVERFLOWING_ADD: &'static str = "i64_overflowing_add_impl";
    const OVERFLOWING_SUB: &'static str = "i64_overflowing_sub_impl";
    const DIFF: &'static str = "i64_diff";
    const UNSIGNED_INT_TYPE: GenericTypeId = <Uint64Type as NamedType>::ID;
}

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
