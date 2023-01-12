use super::felt::FeltType;
use super::jump_not_zero::{JumpNotZeroLibfunc, JumpNotZeroTraits};
use super::non_zero::NonZeroType;
use super::range_check::RangeCheckType;
use super::uint::{IntOperator, UintConstLibfunc, UintTraits, UintType};
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{
    GenericLibfunc, NamedType, NoGenericArgsGenericLibfunc, OutputVarReferenceInfo,
    SignatureBasedConcreteLibfunc, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericLibfuncId, GenericTypeId};
use crate::program::GenericArg;

/// Type for u128.
pub type Uint128Type = UintType<Uint128Traits>;

define_libfunc_hierarchy! {
    pub enum Uint128Libfunc {
        Operation(Uint128OperationLibfunc),
        LessThan(Uint128LessThanLibfunc),
        Equal(Uint128EqualLibfunc),
        LessThanOrEqual(Uint128LessThanOrEqualLibfunc),
        Const(UintConstLibfunc<Uint128Traits>),
        FromFelt(Uint128sFromFeltLibfunc),
        ToFelt(Uint128ToFeltLibfunc),
        JumpNotZero(JumpNotZeroLibfunc<Uint128Traits>),
    }, Uint128Concrete
}

#[derive(Default)]
pub struct Uint128Traits;

impl UintTraits for Uint128Traits {
    type UintType = u128;
    const GENERIC_TYPE_ID: GenericTypeId = GenericTypeId::new_inline("u128");
    const CONST: GenericLibfuncId = GenericLibfuncId::new_inline("u128_const");
}

impl JumpNotZeroTraits for Uint128Traits {
    const JUMP_NOT_ZERO: GenericLibfuncId = GenericLibfuncId::new_inline("u128_jump_nz");
    const GENERIC_TYPE_ID: GenericTypeId = <Uint128Type as NamedType>::ID;
}

/// Libfunc for u128 operations.
pub struct Uint128OperationLibfunc {
    pub operator: IntOperator,
}
impl Uint128OperationLibfunc {
    fn new(operator: IntOperator) -> Self {
        Self { operator }
    }
}
impl GenericLibfunc for Uint128OperationLibfunc {
    type Concrete = Uint128OperationConcreteLibfunc;

    fn by_id(id: &GenericLibfuncId) -> Option<Self> {
        const OVERFLOWING_ADD: GenericLibfuncId = GenericLibfuncId::new_inline("u128_overflow_add");
        const OVERFLOWING_SUB: GenericLibfuncId = GenericLibfuncId::new_inline("u128_overflow_sub");
        const OVERFLOWING_MUL: GenericLibfuncId = GenericLibfuncId::new_inline("u128_overflow_mul");
        const DIVMOD: GenericLibfuncId = GenericLibfuncId::new_inline("u128_safe_divmod");
        const WIDE_MUL: GenericLibfuncId = GenericLibfuncId::new_inline("u128_wide_mul");
        match id {
            id if id == &OVERFLOWING_ADD => Some(Self::new(IntOperator::OverflowingAdd)),
            id if id == &OVERFLOWING_SUB => Some(Self::new(IntOperator::OverflowingSub)),
            id if id == &OVERFLOWING_MUL => Some(Self::new(IntOperator::OverflowingMul)),
            id if id == &DIVMOD => Some(Self::new(IntOperator::DivMod)),
            id if id == &WIDE_MUL => Some(Self::new(IntOperator::WideMul)),
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
        let ty = context.get_concrete_type(Uint128Type::id(), &[])?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        match self.operator {
            IntOperator::DivMod => Ok(LibfuncSignature::new_non_branch_ex(
                vec![
                    ParamSignature {
                        ty: range_check_type.clone(),
                        allow_deferred: false,
                        allow_add_const: true,
                        allow_const: false,
                    },
                    ParamSignature::new(ty.clone()),
                    ParamSignature::new(
                        context.get_wrapped_concrete_type(NonZeroType::id(), ty.clone())?,
                    ),
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
                    OutputVarInfo {
                        ty,
                        ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(1) },
                    },
                ],
                SierraApChange::Known { new_vars_only: false },
            )),
            IntOperator::WideMul => Ok(LibfuncSignature::new_non_branch_ex(
                vec![
                    ParamSignature {
                        ty: range_check_type.clone(),
                        allow_deferred: false,
                        allow_add_const: true,
                        allow_const: false,
                    },
                    ParamSignature::new(ty.clone()),
                    ParamSignature::new(ty.clone()),
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
                    OutputVarInfo {
                        ty,
                        ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(1) },
                    },
                ],
                SierraApChange::Known { new_vars_only: false },
            )),
            IntOperator::OverflowingAdd
            | IntOperator::OverflowingSub
            | IntOperator::OverflowingMul => Ok(LibfuncSignature {
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
                                ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                            },
                        ],
                        ap_change: SierraApChange::Known { new_vars_only: false },
                    },
                ],
                fallthrough: Some(0),
            }),
        }
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(Uint128OperationConcreteLibfunc {
            operator: self.operator,
            signature: self.specialize_signature(context.upcast(), args)?,
        })
    }
}

pub struct Uint128OperationConcreteLibfunc {
    pub operator: IntOperator,
    pub signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for Uint128OperationConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

fn get_u128_comparison_types(
    context: &dyn SignatureSpecializationContext,
) -> Result<(ConcreteTypeId, ConcreteTypeId), SpecializationError> {
    // Returns u128 and range_check types.
    Ok((
        context.get_concrete_type(Uint128Type::id(), &[])?,
        context.get_concrete_type(RangeCheckType::id(), &[])?,
    ))
}

/// Utility method to output the two branches of u128 comparison signatures.
fn get_u128_comparison_branch_signatures(
    context: &dyn SignatureSpecializationContext,
) -> Result<Vec<BranchSignature>, SpecializationError> {
    let (_, range_check_type) = get_u128_comparison_types(context)?;
    Ok((0..2)
        .map(|_| BranchSignature {
            vars: vec![OutputVarInfo {
                ty: range_check_type.clone(),
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                    param_idx: 0,
                }),
            }],
            ap_change: SierraApChange::Known { new_vars_only: false },
        })
        .collect())
}

/// Utility method to output the parameter signatures of u128 comparison signatures.
fn get_u128_comparison_param_signatures(
    context: &dyn SignatureSpecializationContext,
) -> Result<Vec<ParamSignature>, SpecializationError> {
    let (u128_type, range_check_type) = get_u128_comparison_types(context)?;
    Ok(vec![
        ParamSignature {
            ty: range_check_type,
            allow_deferred: false,
            allow_add_const: true,
            allow_const: false,
        },
        ParamSignature::new(u128_type.clone()),
        ParamSignature::new(u128_type),
    ])
}

/// Libfunc for comparing u128s.
#[derive(Default)]
pub struct Uint128LessThanLibfunc {}
impl NoGenericArgsGenericLibfunc for Uint128LessThanLibfunc {
    const ID: GenericLibfuncId = GenericLibfuncId::new_inline("u128_lt");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature {
            param_signatures: get_u128_comparison_param_signatures(context)?,
            branch_signatures: get_u128_comparison_branch_signatures(context)?,
            fallthrough: Some(0),
        })
    }
}

/// Libfunc for comparing u128s` equality.
#[derive(Default)]
pub struct Uint128EqualLibfunc {}
impl NoGenericArgsGenericLibfunc for Uint128EqualLibfunc {
    const ID: GenericLibfuncId = GenericLibfuncId::new_inline("u128_eq");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let u128_ty = context.get_concrete_type(Uint128Type::id(), &[])?;
        let branch_signatures = (0..2)
            .map(|_| BranchSignature {
                vars: vec![],
                ap_change: SierraApChange::Known { new_vars_only: false },
            })
            .collect();
        let param_signature_a = ParamSignature {
            ty: u128_ty.clone(),
            allow_deferred: false,
            allow_add_const: false,
            allow_const: true,
        };
        let param_signature_b = ParamSignature { ty: u128_ty, ..param_signature_a };
        Ok(LibfuncSignature {
            param_signatures: vec![param_signature_a, param_signature_b],
            branch_signatures,
            fallthrough: Some(0),
        })
    }
}

/// Libfunc for comparing u128s.
#[derive(Default)]
pub struct Uint128LessThanOrEqualLibfunc {}
impl NoGenericArgsGenericLibfunc for Uint128LessThanOrEqualLibfunc {
    const ID: GenericLibfuncId = GenericLibfuncId::new_inline("u128_le");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature {
            param_signatures: get_u128_comparison_param_signatures(context)?,
            branch_signatures: get_u128_comparison_branch_signatures(context)?,
            fallthrough: Some(0),
        })
    }
}

/// Libfunc for converting a felt into a u128, or the number and the overflow in the case of
/// failure.
#[derive(Default)]
pub struct Uint128sFromFeltLibfunc {}
impl NoGenericArgsGenericLibfunc for Uint128sFromFeltLibfunc {
    // TODO(lior): Rename to split_felt and remove the branches. Add a separate u128_from_felt()
    //   for the conversion.
    const ID: GenericLibfuncId = GenericLibfuncId::new_inline("u128s_from_felt");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature {
                    ty: range_check_type.clone(),
                    allow_deferred: false,
                    allow_add_const: true,
                    allow_const: false,
                },
                ParamSignature::new(context.get_concrete_type(FeltType::id(), &[])?),
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
                            ty: context.get_concrete_type(Uint128Type::id(), &[])?,
                            ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 1 },
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
                            ty: context.get_concrete_type(Uint128Type::id(), &[])?,
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                        },
                        OutputVarInfo {
                            ty: context.get_concrete_type(Uint128Type::id(), &[])?,
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(1) },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// Libfunc for converting a u128 into a felt.
#[derive(Default)]
pub struct Uint128ToFeltLibfunc {}
impl NoGenericArgsGenericLibfunc for Uint128ToFeltLibfunc {
    const ID: GenericLibfuncId = GenericLibfuncId::new_inline("u128_to_felt");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch(
            vec![context.get_concrete_type(Uint128Type::id(), &[])?],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(FeltType::id(), &[])?,
                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
