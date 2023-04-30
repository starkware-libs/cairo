use std::marker::PhantomData;

use num_bigint::BigInt;

use super::unsigned128::Uint128Type;
use super::IntOperator;
use crate::define_libfunc_hierarchy;
use crate::extensions::felt252::Felt252Type;
use crate::extensions::is_zero::{IsZeroLibfunc, IsZeroTraits};
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::non_zero::nonzero_ty;
use crate::extensions::range_check::RangeCheckType;
use crate::extensions::try_from_felt252::{TryFromFelt252, TryFromFelt252Libfunc};
use crate::extensions::{
    GenericLibfunc, NamedLibfunc, NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType,
    OutputVarReferenceInfo, SignatureBasedConcreteLibfunc, SpecializationError,
};
use crate::ids::{GenericLibfuncId, GenericTypeId};
use crate::program::GenericArg;

/// Trait for implementing unsigned integers.
pub trait UintTraits: Default {
    /// The rust matching type to this type.
    type UintType: TryFrom<BigInt> + Into<BigInt> + Copy;
    /// Is the type smaller than 128 bits.
    /// Relevant since some implementations are different due to range check being 128 bits based.
    const IS_SMALL: bool;
    /// The generic type id for this type.
    const GENERIC_TYPE_ID: GenericTypeId;
    /// The generic libfunc id for getting a const of this type.
    const CONST: &'static str;
    /// The generic libfunc id for comparing equality.
    const EQUAL: &'static str;
    /// The generic libfunc id for calculating the integer square root.
    const SQUARE_ROOT: &'static str;
    /// The generic type id for the type's square root.
    const SQUARE_ROOT_TYPE_ID: GenericTypeId;
    /// The generic libfunc id for testing if less than.
    const LESS_THAN: &'static str;
    /// The generic libfunc id for testing if less than or equal.
    const LESS_THAN_OR_EQUAL: &'static str;
    /// The generic libfunc id for addition.
    const OVERFLOWING_ADD: &'static str;
    /// The generic libfunc id for subtraction.
    const OVERFLOWING_SUB: &'static str;
    /// The generic libfunc id for conversion to felt252.
    const TO_FELT252: &'static str;
    /// The generic libfunc id for conversion from felt252.
    const TRY_FROM_FELT252: &'static str;
    /// The generic libfunc id that divides two integers.
    const DIVMOD: &'static str;
}

/// Trait for implementing multiplication for unsigned integers.
pub trait UintMulTraits: UintTraits {
    /// The generic libfunc id that multiplies two integers.
    const WIDE_MUL: &'static str;
    /// The generic type id for this type multiplication result.
    const WIDE_MUL_RES_TYPE_ID: GenericTypeId;
}

#[derive(Default)]
pub struct UintType<TUintTraits: UintTraits> {
    _phantom: PhantomData<TUintTraits>,
}
impl<TUintTraits: UintTraits> NoGenericArgsGenericType for UintType<TUintTraits> {
    const ID: GenericTypeId = TUintTraits::GENERIC_TYPE_ID;
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const SIZE: i16 = 1;
}

/// Libfunc for creating a constant unsigned integer.
#[derive(Default)]
pub struct UintConstLibfunc<TUintTraits: UintTraits> {
    _phantom: PhantomData<TUintTraits>,
}
impl<TUintTraits: UintTraits> NamedLibfunc for UintConstLibfunc<TUintTraits> {
    const STR_ID: &'static str = TUintTraits::CONST;
    type Concrete = UintConstConcreteLibfunc<TUintTraits>;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        _args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(TUintTraits::GENERIC_TYPE_ID, &[])?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Const),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [GenericArg::Value(c)] => Ok(Self::Concrete {
                c: TUintTraits::UintType::try_from(c.clone())
                    .map_err(|_| SpecializationError::UnsupportedGenericArg)?,
                signature: <Self as NamedLibfunc>::specialize_signature(
                    self,
                    context.upcast(),
                    args,
                )?,
            }),
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

pub struct UintConstConcreteLibfunc<TUintTraits: UintTraits> {
    pub c: TUintTraits::UintType,
    pub signature: LibfuncSignature,
}
impl<TUintTraits: UintTraits> SignatureBasedConcreteLibfunc
    for UintConstConcreteLibfunc<TUintTraits>
{
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Libfunc for comparing uints` equality.
#[derive(Default)]
pub struct UintEqualLibfunc<TUintTraits: UintTraits> {
    _phantom: PhantomData<TUintTraits>,
}
impl<TUintTraits: UintTraits> NoGenericArgsGenericLibfunc for UintEqualLibfunc<TUintTraits> {
    const STR_ID: &'static str = TUintTraits::EQUAL;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = context.get_concrete_type(TUintTraits::GENERIC_TYPE_ID, &[])?;
        let param_signatures = vec![
            ParamSignature::new(ty.clone()),
            ParamSignature { ty, allow_deferred: false, allow_add_const: false, allow_const: true },
        ];
        let branch_signatures = (0..2)
            .map(|_| BranchSignature {
                vars: vec![],
                ap_change: SierraApChange::Known { new_vars_only: false },
            })
            .collect();
        Ok(LibfuncSignature { param_signatures, branch_signatures, fallthrough: Some(0) })
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
                ParamSignature {
                    ty: range_check_type.clone(),
                    allow_deferred: false,
                    allow_add_const: true,
                    allow_const: false,
                },
                ParamSignature::new(ty),
            ],
            vec![
                OutputVarInfo {
                    ty: range_check_type,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                },
                OutputVarInfo {
                    ty: sqrt_ty,
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                },
            ],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for comparing uints.
#[derive(Default)]
pub struct UintLessThanLibfunc<TUintTraits: UintTraits> {
    _phantom: PhantomData<TUintTraits>,
}
impl<TUintTraits: UintTraits> NoGenericArgsGenericLibfunc for UintLessThanLibfunc<TUintTraits> {
    const STR_ID: &'static str = TUintTraits::LESS_THAN;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = context.get_concrete_type(TUintTraits::GENERIC_TYPE_ID, &[])?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let param_signatures = vec![
            ParamSignature {
                ty: range_check_type.clone(),
                allow_deferred: false,
                allow_add_const: true,
                allow_const: false,
            },
            ParamSignature::new(ty.clone()),
            ParamSignature::new(ty),
        ];
        let branch_signatures = (0..2)
            .map(|_| BranchSignature {
                vars: vec![OutputVarInfo {
                    ty: range_check_type.clone(),
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                }],
                ap_change: SierraApChange::Known { new_vars_only: false },
            })
            .collect();
        Ok(LibfuncSignature { param_signatures, branch_signatures, fallthrough: Some(0) })
    }
}

/// Libfunc for comparing uints.
#[derive(Default)]
pub struct UintLessThanOrEqualLibfunc<TUintTraits: UintTraits> {
    _phantom: PhantomData<TUintTraits>,
}
impl<TUintTraits: UintTraits> NoGenericArgsGenericLibfunc
    for UintLessThanOrEqualLibfunc<TUintTraits>
{
    const STR_ID: &'static str = TUintTraits::LESS_THAN_OR_EQUAL;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = context.get_concrete_type(TUintTraits::GENERIC_TYPE_ID, &[])?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let param_signatures = vec![
            ParamSignature {
                ty: range_check_type.clone(),
                allow_deferred: false,
                allow_add_const: true,
                allow_const: false,
            },
            ParamSignature::new(ty.clone()),
            ParamSignature::new(ty),
        ];
        let branch_signatures = (0..2)
            .map(|_| BranchSignature {
                vars: vec![OutputVarInfo {
                    ty: range_check_type.clone(),
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                }],
                ap_change: SierraApChange::Known { new_vars_only: false },
            })
            .collect();
        Ok(LibfuncSignature { param_signatures, branch_signatures, fallthrough: Some(0) })
    }
}

pub struct UintOperationConcreteLibfunc {
    pub operator: IntOperator,
    pub signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for UintOperationConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Libfunc for uints operations.
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
    type Concrete = UintOperationConcreteLibfunc;

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
        let is_wrapping_result_at_end =
            !TUintTraits::IS_SMALL || self.operator == IntOperator::OverflowingSub;
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature {
                    ty: range_check_type.clone(),
                    allow_deferred: false,
                    allow_add_const: true,
                    allow_const: false,
                },
                ParamSignature::new(ty.clone()),
                ParamSignature::new(ty.clone()),
            ],
            branch_signatures: vec![
                BranchSignature {
                    vars: vec![
                        OutputVarInfo {
                            ty: range_check_type.clone(),
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 0 },
                            ),
                        },
                        OutputVarInfo {
                            ty: ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![
                        OutputVarInfo {
                            ty: range_check_type,
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 0 },
                            ),
                        },
                        OutputVarInfo {
                            ty,
                            ref_info: OutputVarReferenceInfo::NewTempVar {
                                idx: if is_wrapping_result_at_end { Some(0) } else { None },
                            },
                        },
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
        Ok(UintOperationConcreteLibfunc {
            operator: self.operator,
            signature: self.specialize_signature(context.upcast(), args)?,
        })
    }
}

/// Libfunc for converting a uint into a felt252.
#[derive(Default)]
pub struct UintToFelt252Libfunc<TUintTraits: UintTraits> {
    _phantom: PhantomData<TUintTraits>,
}
impl<TUintTraits: UintTraits> NoGenericArgsGenericLibfunc for UintToFelt252Libfunc<TUintTraits> {
    const STR_ID: &'static str = TUintTraits::TO_FELT252;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![ParamSignature {
                ty: context.get_concrete_type(TUintTraits::GENERIC_TYPE_ID, &[])?,
                allow_deferred: true,
                allow_add_const: true,
                allow_const: true,
            }],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(Felt252Type::id(), &[])?,
                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Libfunc for attempting to convert a felt252 into a uint.
#[derive(Default)]
pub struct UintFromFelt252Trait<TUintTraits: UintTraits> {
    _phantom: PhantomData<TUintTraits>,
}
impl<TUintTraits: UintTraits> TryFromFelt252 for UintFromFelt252Trait<TUintTraits> {
    const STR_ID: &'static str = TUintTraits::TRY_FROM_FELT252;
    const GENERIC_TYPE_ID: GenericTypeId = TUintTraits::GENERIC_TYPE_ID;
}

pub type UintFromFelt252Libfunc<T> = TryFromFelt252Libfunc<UintFromFelt252Trait<T>>;

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
                ParamSignature {
                    ty: range_check_type.clone(),
                    allow_deferred: false,
                    allow_add_const: true,
                    allow_const: false,
                },
                ParamSignature::new(ty.clone()),
                ParamSignature::new(nonzero_ty(context, &ty)?),
            ],
            vec![
                OutputVarInfo {
                    ty: range_check_type,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                },
                OutputVarInfo {
                    ty: ty.clone(),
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                },
                OutputVarInfo { ty, ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(1) } },
            ],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for uint wide multiplication.
#[derive(Default)]
pub struct UintWideMulLibfunc<TUintMulTraits: UintMulTraits> {
    _phantom: PhantomData<TUintMulTraits>,
}
impl<TUintMulTraits: UintMulTraits> NoGenericArgsGenericLibfunc
    for UintWideMulLibfunc<TUintMulTraits>
{
    const STR_ID: &'static str = TUintMulTraits::WIDE_MUL;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = context.get_concrete_type(TUintMulTraits::GENERIC_TYPE_ID, &[])?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(ty.clone()),
                ParamSignature {
                    ty,
                    allow_deferred: false,
                    allow_add_const: false,
                    allow_const: true,
                },
            ],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(TUintMulTraits::WIDE_MUL_RES_TYPE_ID, &[])?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

define_libfunc_hierarchy! {
    pub enum UintLibfunc<TUintTraits: UintMulTraits + IsZeroTraits> {
        Const(UintConstLibfunc<TUintTraits>),
        Operation(UintOperationLibfunc<TUintTraits>),
        LessThan(UintLessThanLibfunc<TUintTraits>),
        SquareRoot(UintSquareRootLibfunc<TUintTraits>),
        Equal(UintEqualLibfunc<TUintTraits>),
        LessThanOrEqual(UintLessThanOrEqualLibfunc<TUintTraits>),
        ToFelt252(UintToFelt252Libfunc<TUintTraits>),
        FromFelt252(UintFromFelt252Libfunc<TUintTraits>),
        IsZero(IsZeroLibfunc<TUintTraits>),
        Divmod(UintDivmodLibfunc<TUintTraits>),
        WideMul(UintWideMulLibfunc<TUintTraits>),
    }, UintConcrete
}

#[derive(Default)]
pub struct Uint8Traits;

impl UintTraits for Uint8Traits {
    type UintType = u8;
    const GENERIC_TYPE_ID: GenericTypeId = GenericTypeId::new_inline("u8");
    const IS_SMALL: bool = true;
    const CONST: &'static str = "u8_const";
    const EQUAL: &'static str = "u8_eq";
    const SQUARE_ROOT: &'static str = "u8_sqrt";
    const SQUARE_ROOT_TYPE_ID: GenericTypeId = <Self as UintTraits>::GENERIC_TYPE_ID;
    const LESS_THAN: &'static str = "u8_lt";
    const LESS_THAN_OR_EQUAL: &'static str = "u8_le";
    const OVERFLOWING_ADD: &'static str = "u8_overflowing_add";
    const OVERFLOWING_SUB: &'static str = "u8_overflowing_sub";
    const TO_FELT252: &'static str = "u8_to_felt252";
    const TRY_FROM_FELT252: &'static str = "u8_try_from_felt252";
    const DIVMOD: &'static str = "u8_safe_divmod";
}

impl UintMulTraits for Uint8Traits {
    const WIDE_MUL: &'static str = "u8_wide_mul";
    const WIDE_MUL_RES_TYPE_ID: GenericTypeId = <Uint16Type as NamedType>::ID;
}

impl IsZeroTraits for Uint8Traits {
    const IS_ZERO: &'static str = "u8_is_zero";
    const GENERIC_TYPE_ID: GenericTypeId = <Uint8Type as NamedType>::ID;
}

/// Type for u8.
pub type Uint8Type = UintType<Uint8Traits>;
pub type Uint8Libfunc = UintLibfunc<Uint8Traits>;
pub type Uint8Concrete = <Uint8Libfunc as GenericLibfunc>::Concrete;

#[derive(Default)]
pub struct Uint16Traits;

impl UintTraits for Uint16Traits {
    type UintType = u16;
    const GENERIC_TYPE_ID: GenericTypeId = GenericTypeId::new_inline("u16");
    const IS_SMALL: bool = true;
    const CONST: &'static str = "u16_const";
    const EQUAL: &'static str = "u16_eq";
    const SQUARE_ROOT: &'static str = "u16_sqrt";
    const SQUARE_ROOT_TYPE_ID: GenericTypeId = <Uint8Type as NamedType>::ID;
    const LESS_THAN: &'static str = "u16_lt";
    const LESS_THAN_OR_EQUAL: &'static str = "u16_le";
    const OVERFLOWING_ADD: &'static str = "u16_overflowing_add";
    const OVERFLOWING_SUB: &'static str = "u16_overflowing_sub";
    const TO_FELT252: &'static str = "u16_to_felt252";
    const TRY_FROM_FELT252: &'static str = "u16_try_from_felt252";
    const DIVMOD: &'static str = "u16_safe_divmod";
}

impl UintMulTraits for Uint16Traits {
    const WIDE_MUL: &'static str = "u16_wide_mul";
    const WIDE_MUL_RES_TYPE_ID: GenericTypeId = <Uint32Type as NamedType>::ID;
}

impl IsZeroTraits for Uint16Traits {
    const IS_ZERO: &'static str = "u16_is_zero";
    const GENERIC_TYPE_ID: GenericTypeId = <Uint16Type as NamedType>::ID;
}

/// Type for u16.
pub type Uint16Type = UintType<Uint16Traits>;
pub type Uint16Libfunc = UintLibfunc<Uint16Traits>;
pub type Uint16Concrete = <Uint16Libfunc as GenericLibfunc>::Concrete;

#[derive(Default)]
pub struct Uint32Traits;

impl UintTraits for Uint32Traits {
    type UintType = u32;
    const GENERIC_TYPE_ID: GenericTypeId = GenericTypeId::new_inline("u32");
    const IS_SMALL: bool = true;
    const CONST: &'static str = "u32_const";
    const EQUAL: &'static str = "u32_eq";
    const SQUARE_ROOT: &'static str = "u32_sqrt";
    const SQUARE_ROOT_TYPE_ID: GenericTypeId = <Uint16Type as NamedType>::ID;
    const LESS_THAN: &'static str = "u32_lt";
    const LESS_THAN_OR_EQUAL: &'static str = "u32_le";
    const OVERFLOWING_ADD: &'static str = "u32_overflowing_add";
    const OVERFLOWING_SUB: &'static str = "u32_overflowing_sub";
    const TO_FELT252: &'static str = "u32_to_felt252";
    const TRY_FROM_FELT252: &'static str = "u32_try_from_felt252";
    const DIVMOD: &'static str = "u32_safe_divmod";
}

impl UintMulTraits for Uint32Traits {
    const WIDE_MUL: &'static str = "u32_wide_mul";
    const WIDE_MUL_RES_TYPE_ID: GenericTypeId = <Uint64Type as NamedType>::ID;
}

impl IsZeroTraits for Uint32Traits {
    const IS_ZERO: &'static str = "u32_is_zero";
    const GENERIC_TYPE_ID: GenericTypeId = <Uint32Type as NamedType>::ID;
}

/// Type for u32.
pub type Uint32Type = UintType<Uint32Traits>;
pub type Uint32Libfunc = UintLibfunc<Uint32Traits>;
pub type Uint32Concrete = <Uint32Libfunc as GenericLibfunc>::Concrete;

#[derive(Default)]
pub struct Uint64Traits;

impl UintTraits for Uint64Traits {
    type UintType = u64;
    const GENERIC_TYPE_ID: GenericTypeId = GenericTypeId::new_inline("u64");
    const IS_SMALL: bool = true;
    const CONST: &'static str = "u64_const";
    const EQUAL: &'static str = "u64_eq";
    const SQUARE_ROOT: &'static str = "u64_sqrt";
    const SQUARE_ROOT_TYPE_ID: GenericTypeId = <Uint32Type as NamedType>::ID;
    const LESS_THAN: &'static str = "u64_lt";
    const LESS_THAN_OR_EQUAL: &'static str = "u64_le";
    const OVERFLOWING_ADD: &'static str = "u64_overflowing_add";
    const OVERFLOWING_SUB: &'static str = "u64_overflowing_sub";
    const TO_FELT252: &'static str = "u64_to_felt252";
    const TRY_FROM_FELT252: &'static str = "u64_try_from_felt252";
    const DIVMOD: &'static str = "u64_safe_divmod";
}

impl UintMulTraits for Uint64Traits {
    const WIDE_MUL: &'static str = "u64_wide_mul";
    const WIDE_MUL_RES_TYPE_ID: GenericTypeId = <Uint128Type as NamedType>::ID;
}

impl IsZeroTraits for Uint64Traits {
    const IS_ZERO: &'static str = "u64_is_zero";
    const GENERIC_TYPE_ID: GenericTypeId = <Uint64Type as NamedType>::ID;
}

/// Type for u64.
pub type Uint64Type = UintType<Uint64Traits>;
pub type Uint64Libfunc = UintLibfunc<Uint64Traits>;
pub type Uint64Concrete = <Uint64Libfunc as GenericLibfunc>::Concrete;
