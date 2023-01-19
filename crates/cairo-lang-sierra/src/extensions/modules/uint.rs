use std::marker::PhantomData;

use num_bigint::BigInt;

use super::range_check::RangeCheckType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{
    GenericLibfunc, NamedLibfunc, NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType,
    OutputVarReferenceInfo, SignatureBasedConcreteLibfunc, SpecializationError,
};
use crate::ids::{id_from_string, GenericLibfuncId, GenericTypeId};
use crate::program::GenericArg;

/// Operators for integers.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IntOperator {
    OverflowingAdd,
    OverflowingSub,
}

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
    /// The generic libfunc id for testing if less than.
    const LESS_THAN: &'static str;
    /// The generic libfunc id for testing if less than or equal.
    const LESS_THAN_OR_EQUAL: &'static str;
    /// The generic libfunc id for addition.
    const OVERFLOWING_ADD: &'static str;
    /// The generic libfunc id for subtraction.
    const OVERFLOWING_SUB: &'static str;
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
            ParamSignature {
                ty: ty.clone(),
                allow_deferred: false,
                allow_add_const: false,
                allow_const: true,
            },
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

/// Libfunc for comparing u128s.
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
    const OVERFLOWING_ADD: u64 = id_from_string(TUintTraits::OVERFLOWING_ADD);
    const OVERFLOWING_SUB: u64 = id_from_string(TUintTraits::OVERFLOWING_SUB);
    fn new(operator: IntOperator) -> Option<Self> {
        Some(Self { operator, _phantom: PhantomData::default() })
    }
}
impl<TUintTraits: UintTraits> GenericLibfunc for UintOperationLibfunc<TUintTraits> {
    type Concrete = UintOperationConcreteLibfunc;

    fn by_id(id: &GenericLibfuncId) -> Option<Self> {
        match id.id {
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

#[derive(Default)]
pub struct Uint8Traits;

impl UintTraits for Uint8Traits {
    type UintType = u8;
    const GENERIC_TYPE_ID: GenericTypeId = GenericTypeId::new_inline("u8");
    const IS_SMALL: bool = true;
    const CONST: &'static str = "u8_const";
    const EQUAL: &'static str = "u8_eq";
    const LESS_THAN: &'static str = "u8_lt";
    const LESS_THAN_OR_EQUAL: &'static str = "u8_le";
    const OVERFLOWING_ADD: &'static str = "u8_overflowing_add";
    const OVERFLOWING_SUB: &'static str = "u8_overflowing_sub";
}

/// Type for u8.
pub type Uint8Type = UintType<Uint8Traits>;

define_libfunc_hierarchy! {
    pub enum Uint8Libfunc {
        Const(UintConstLibfunc<Uint8Traits>),
        Operation(UintOperationLibfunc<Uint8Traits>),
        LessThan(UintLessThanLibfunc<Uint8Traits>),
        Equal(UintEqualLibfunc<Uint8Traits>),
        LessThanOrEqual(UintLessThanOrEqualLibfunc<Uint8Traits>),
    }, Uint8Concrete
}
