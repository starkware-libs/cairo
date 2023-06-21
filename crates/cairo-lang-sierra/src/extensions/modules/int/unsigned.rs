use std::marker::PhantomData;

use super::unsigned128::Uint128Type;
use super::{
    IntConstLibfunc, IntEqualLibfunc, IntFromFelt252Libfunc, IntMulTraits,
    IntOperationConcreteLibfunc, IntOperator, IntToFelt252Libfunc, IntTraits, IntType,
    IntWideMulLibfunc,
};
use crate::define_libfunc_hierarchy;
use crate::extensions::bitwise::BitwiseType;
use crate::extensions::is_zero::{IsZeroLibfunc, IsZeroTraits};
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::non_zero::nonzero_ty;
use crate::extensions::range_check::RangeCheckType;
use crate::extensions::{
    GenericLibfunc, NamedType, NoGenericArgsGenericLibfunc, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::{GenericLibfuncId, GenericTypeId};
use crate::program::GenericArg;

/// Trait for implementing unsigned integers.
pub trait UintTraits: IntTraits {
    /// The generic libfunc id for addition.
    const OVERFLOWING_ADD: &'static str;
    /// The generic libfunc id for subtraction.
    const OVERFLOWING_SUB: &'static str;
    /// The generic libfunc id for calculating the integer square root.
    const SQUARE_ROOT: &'static str;
    /// The generic type id for the type's square root.
    const SQUARE_ROOT_TYPE_ID: GenericTypeId;
    /// The generic libfunc id that divides two integers.
    const DIVMOD: &'static str;
    /// The generic libfunc id that provides bitwise operations on two integers.
    const BITWISE: &'static str;
}

/// Libfunc for integer operations.
pub struct UintOperationLibfunc<TUintTraits: UintTraits> {
    pub operator: IntOperator,
    _phantom: PhantomData<TUintTraits>,
}
impl<TUintTraits: UintTraits> UintOperationLibfunc<TUintTraits> {
    const OVERFLOWING_ADD: &'static str = TUintTraits::OVERFLOWING_ADD;
    const OVERFLOWING_SUB: &'static str = TUintTraits::OVERFLOWING_SUB;
    fn new(operator: IntOperator) -> Option<Self> {
        Some(Self { operator, _phantom: PhantomData::default() })
    }
}
impl<TUintTraits: UintTraits> GenericLibfunc for UintOperationLibfunc<TUintTraits> {
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
        let ty = context.get_concrete_type(TUintTraits::GENERIC_TYPE_ID, &[])?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;

        let wrapping_result_ref_info = match (self.operator, TUintTraits::IS_SMALL) {
            (IntOperator::OverflowingSub, true) => {
                OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic)
            }
            (IntOperator::OverflowingAdd, false)
            | (IntOperator::OverflowingAdd, true)
            | (IntOperator::OverflowingSub, false) => OutputVarReferenceInfo::NewTempVar { idx: 0 },
        };

        let rc_output_info = OutputVarInfo::new_builtin(range_check_type.clone(), 0);
        let ty_param = ParamSignature::new(ty.clone());
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
                        OutputVarInfo {
                            ty: ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![
                        rc_output_info,
                        OutputVarInfo { ty, ref_info: wrapping_result_ref_info },
                    ],
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

/// Libfunc for calculating uint's square root.
#[derive(Default)]
pub struct UintSquareRootLibfunc<TUintTraits: UintTraits> {
    _phantom: PhantomData<TUintTraits>,
}
impl<TUintTraits: UintTraits> NoGenericArgsGenericLibfunc for UintSquareRootLibfunc<TUintTraits> {
    const STR_ID: &'static str = TUintTraits::SQUARE_ROOT;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = context.get_concrete_type(TUintTraits::GENERIC_TYPE_ID, &[])?;
        let sqrt_ty = context.get_concrete_type(TUintTraits::SQUARE_ROOT_TYPE_ID, &[])?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(range_check_type.clone()).with_allow_add_const(),
                ParamSignature::new(ty),
            ],
            vec![
                OutputVarInfo::new_builtin(range_check_type, 0),
                OutputVarInfo {
                    ty: sqrt_ty,
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                },
            ],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for uint divmod.
#[derive(Default)]
pub struct UintDivmodLibfunc<TUintTraits: UintTraits> {
    _phantom: PhantomData<TUintTraits>,
}
impl<TUintTraits: UintTraits> NoGenericArgsGenericLibfunc for UintDivmodLibfunc<TUintTraits> {
    const STR_ID: &'static str = TUintTraits::DIVMOD;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = context.get_concrete_type(TUintTraits::GENERIC_TYPE_ID, &[])?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(range_check_type.clone()).with_allow_add_const(),
                ParamSignature::new(ty.clone()),
                ParamSignature::new(nonzero_ty(context, &ty)?),
            ],
            vec![
                OutputVarInfo::new_builtin(range_check_type, 0),
                OutputVarInfo {
                    ty: ty.clone(),
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                },
                OutputVarInfo { ty, ref_info: OutputVarReferenceInfo::NewTempVar { idx: 1 } },
            ],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for computing the Bitwise (and,or,xor) of two uints.
/// Returns 3 uints (and the updated builtin pointer).
#[derive(Default)]
pub struct UintBitwiseLibfunc<TUintTraits: UintTraits> {
    _phantom: PhantomData<TUintTraits>,
}
impl<TUintTraits: UintTraits> NoGenericArgsGenericLibfunc for UintBitwiseLibfunc<TUintTraits> {
    const STR_ID: &'static str = TUintTraits::BITWISE;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let bitwise_ty = context.get_concrete_type(BitwiseType::id(), &[])?;
        let ty = context.get_concrete_type(TUintTraits::GENERIC_TYPE_ID, &[])?;
        let deferred_ty_output_info = OutputVarInfo {
            ty: ty.clone(),
            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
        };
        let ty_param = ParamSignature::new(ty);
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(bitwise_ty.clone()).with_allow_add_const(),
                ty_param.clone(),
                ty_param,
            ],
            vec![
                OutputVarInfo::new_builtin(bitwise_ty, 0),
                deferred_ty_output_info.clone(),
                deferred_ty_output_info.clone(),
                deferred_ty_output_info,
            ],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

define_libfunc_hierarchy! {
    pub enum UintLibfunc<TUintTraits: UintTraits + IntMulTraits + IsZeroTraits> {
        Const(IntConstLibfunc<TUintTraits>),
        Operation(UintOperationLibfunc<TUintTraits>),
        SquareRoot(UintSquareRootLibfunc<TUintTraits>),
        Equal(IntEqualLibfunc<TUintTraits>),
        ToFelt252(IntToFelt252Libfunc<TUintTraits>),
        FromFelt252(IntFromFelt252Libfunc<TUintTraits>),
        IsZero(IsZeroLibfunc<TUintTraits>),
        Divmod(UintDivmodLibfunc<TUintTraits>),
        WideMul(IntWideMulLibfunc<TUintTraits>),
        Bitwise(UintBitwiseLibfunc<TUintTraits>),
    }, UintConcrete
}

#[derive(Default)]
pub struct Uint8Traits;

impl IntTraits for Uint8Traits {
    type IntType = u8;
    const GENERIC_TYPE_ID: GenericTypeId = GenericTypeId::new_inline("u8");
    const IS_SMALL: bool = true;
    const CONST: &'static str = "u8_const";
    const EQUAL: &'static str = "u8_eq";
    const TO_FELT252: &'static str = "u8_to_felt252";
    const TRY_FROM_FELT252: &'static str = "u8_try_from_felt252";
}

impl UintTraits for Uint8Traits {
    const OVERFLOWING_ADD: &'static str = "u8_overflowing_add";
    const OVERFLOWING_SUB: &'static str = "u8_overflowing_sub";
    const SQUARE_ROOT: &'static str = "u8_sqrt";
    const SQUARE_ROOT_TYPE_ID: GenericTypeId = <Self as IntTraits>::GENERIC_TYPE_ID;
    const DIVMOD: &'static str = "u8_safe_divmod";
    const BITWISE: &'static str = "u8_bitwise";
}

impl IntMulTraits for Uint8Traits {
    const WIDE_MUL: &'static str = "u8_wide_mul";
    const WIDE_MUL_RES_TYPE_ID: GenericTypeId = <Uint16Type as NamedType>::ID;
}

impl IsZeroTraits for Uint8Traits {
    const IS_ZERO: &'static str = "u8_is_zero";
    const GENERIC_TYPE_ID: GenericTypeId = <Uint8Type as NamedType>::ID;
}

/// Type for u8.
pub type Uint8Type = IntType<Uint8Traits>;
pub type Uint8Libfunc = UintLibfunc<Uint8Traits>;
pub type Uint8Concrete = <Uint8Libfunc as GenericLibfunc>::Concrete;

#[derive(Default)]
pub struct Uint16Traits;

impl IntTraits for Uint16Traits {
    type IntType = u16;
    const GENERIC_TYPE_ID: GenericTypeId = GenericTypeId::new_inline("u16");
    const IS_SMALL: bool = true;
    const CONST: &'static str = "u16_const";
    const EQUAL: &'static str = "u16_eq";
    const TO_FELT252: &'static str = "u16_to_felt252";
    const TRY_FROM_FELT252: &'static str = "u16_try_from_felt252";
}

impl UintTraits for Uint16Traits {
    const OVERFLOWING_ADD: &'static str = "u16_overflowing_add";
    const OVERFLOWING_SUB: &'static str = "u16_overflowing_sub";
    const SQUARE_ROOT: &'static str = "u16_sqrt";
    const SQUARE_ROOT_TYPE_ID: GenericTypeId = <Uint8Type as NamedType>::ID;
    const DIVMOD: &'static str = "u16_safe_divmod";
    const BITWISE: &'static str = "u16_bitwise";
}

impl IntMulTraits for Uint16Traits {
    const WIDE_MUL: &'static str = "u16_wide_mul";
    const WIDE_MUL_RES_TYPE_ID: GenericTypeId = <Uint32Type as NamedType>::ID;
}

impl IsZeroTraits for Uint16Traits {
    const IS_ZERO: &'static str = "u16_is_zero";
    const GENERIC_TYPE_ID: GenericTypeId = <Uint16Type as NamedType>::ID;
}

/// Type for u16.
pub type Uint16Type = IntType<Uint16Traits>;
pub type Uint16Libfunc = UintLibfunc<Uint16Traits>;
pub type Uint16Concrete = <Uint16Libfunc as GenericLibfunc>::Concrete;

#[derive(Default)]
pub struct Uint32Traits;

impl IntTraits for Uint32Traits {
    type IntType = u32;
    const GENERIC_TYPE_ID: GenericTypeId = GenericTypeId::new_inline("u32");
    const IS_SMALL: bool = true;
    const CONST: &'static str = "u32_const";
    const EQUAL: &'static str = "u32_eq";
    const TO_FELT252: &'static str = "u32_to_felt252";
    const TRY_FROM_FELT252: &'static str = "u32_try_from_felt252";
}

impl UintTraits for Uint32Traits {
    const OVERFLOWING_ADD: &'static str = "u32_overflowing_add";
    const OVERFLOWING_SUB: &'static str = "u32_overflowing_sub";
    const SQUARE_ROOT: &'static str = "u32_sqrt";
    const SQUARE_ROOT_TYPE_ID: GenericTypeId = <Uint16Type as NamedType>::ID;
    const DIVMOD: &'static str = "u32_safe_divmod";
    const BITWISE: &'static str = "u32_bitwise";
}

impl IntMulTraits for Uint32Traits {
    const WIDE_MUL: &'static str = "u32_wide_mul";
    const WIDE_MUL_RES_TYPE_ID: GenericTypeId = <Uint64Type as NamedType>::ID;
}

impl IsZeroTraits for Uint32Traits {
    const IS_ZERO: &'static str = "u32_is_zero";
    const GENERIC_TYPE_ID: GenericTypeId = <Uint32Type as NamedType>::ID;
}

/// Type for u32.
pub type Uint32Type = IntType<Uint32Traits>;
pub type Uint32Libfunc = UintLibfunc<Uint32Traits>;
pub type Uint32Concrete = <Uint32Libfunc as GenericLibfunc>::Concrete;

#[derive(Default)]
pub struct Uint64Traits;

impl IntTraits for Uint64Traits {
    type IntType = u64;
    const GENERIC_TYPE_ID: GenericTypeId = GenericTypeId::new_inline("u64");
    const IS_SMALL: bool = true;
    const CONST: &'static str = "u64_const";
    const EQUAL: &'static str = "u64_eq";
    const TO_FELT252: &'static str = "u64_to_felt252";
    const TRY_FROM_FELT252: &'static str = "u64_try_from_felt252";
}

impl UintTraits for Uint64Traits {
    const OVERFLOWING_ADD: &'static str = "u64_overflowing_add";
    const OVERFLOWING_SUB: &'static str = "u64_overflowing_sub";
    const SQUARE_ROOT: &'static str = "u64_sqrt";
    const SQUARE_ROOT_TYPE_ID: GenericTypeId = <Uint32Type as NamedType>::ID;
    const DIVMOD: &'static str = "u64_safe_divmod";
    const BITWISE: &'static str = "u64_bitwise";
}

impl IntMulTraits for Uint64Traits {
    const WIDE_MUL: &'static str = "u64_wide_mul";
    const WIDE_MUL_RES_TYPE_ID: GenericTypeId = <Uint128Type as NamedType>::ID;
}

impl IsZeroTraits for Uint64Traits {
    const IS_ZERO: &'static str = "u64_is_zero";
    const GENERIC_TYPE_ID: GenericTypeId = <Uint64Type as NamedType>::ID;
}

/// Type for u64.
pub type Uint64Type = IntType<Uint64Traits>;
pub type Uint64Libfunc = UintLibfunc<Uint64Traits>;
pub type Uint64Concrete = <Uint64Libfunc as GenericLibfunc>::Concrete;
