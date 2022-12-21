use num_traits::Zero;

use super::felt::FeltType;
use super::jump_not_zero::{JumpNotZeroLibFunc, JumpNotZeroTraits};
use super::non_zero::NonZeroType;
use super::range_check::RangeCheckType;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibFuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::types::{InfoOnlyConcreteType, TypeInfo};
use crate::extensions::{
    GenericLibFunc, NamedLibFunc, NamedType, NoGenericArgsGenericLibFunc, NoGenericArgsGenericType,
    OutputVarReferenceInfo, SignatureBasedConcreteLibFunc, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericLibFuncId, GenericTypeId};
use crate::program::GenericArg;
use crate::{define_concrete_libfunc_hierarchy, define_libfunc_hierarchy};

/// Type for u128.
#[derive(Default)]
pub struct Uint128Type {}
impl NoGenericArgsGenericType for Uint128Type {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("u128");

    fn specialize(&self) -> Self::Concrete {
        InfoOnlyConcreteType {
            info: TypeInfo {
                long_id: Self::concrete_type_long_id(&[]),
                storable: true,
                droppable: true,
                duplicatable: true,
                size: 1,
            },
        }
    }
}

define_libfunc_hierarchy! {
    pub enum Uint128LibFunc {
        Operation(Uint128OperationLibFunc),
        LessThan(Uint128LessThanLibFunc),
        Equal(Uint128EqualLibFunc),
        LessThanOrEqual(Uint128LessThanOrEqualLibFunc),
        Const(Uint128ConstLibFunc),
        FromFelt(Uint128sFromFeltLibFunc),
        ToFelt(Uint128ToFeltLibFunc),
        JumpNotZero(Uint128JumpNotZeroLibFunc),
    }, Uint128Concrete
}

#[derive(Default)]
pub struct Uint128Traits {}
impl JumpNotZeroTraits for Uint128Traits {
    const JUMP_NOT_ZERO: GenericLibFuncId = GenericLibFuncId::new_inline("u128_jump_nz");
    const GENERIC_TYPE_ID: GenericTypeId = <Uint128Type as NamedType>::ID;
}
pub type Uint128JumpNotZeroLibFunc = JumpNotZeroLibFunc<Uint128Traits>;

/// Operators for integers.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IntOperator {
    OverflowingAdd,
    OverflowingSub,
    OverflowingMul,
    DivMod,
}

/// Libfunc for u128 operations.
pub struct Uint128OperationLibFunc {
    pub operator: IntOperator,
}
impl Uint128OperationLibFunc {
    fn new(operator: IntOperator) -> Self {
        Self { operator }
    }
}
impl GenericLibFunc for Uint128OperationLibFunc {
    type Concrete = Uint128OperationConcreteLibFunc;

    fn by_id(id: &GenericLibFuncId) -> Option<Self> {
        const OVERFLOWING_ADD: GenericLibFuncId = GenericLibFuncId::new_inline("u128_overflow_add");
        const OVERFLOWING_SUB: GenericLibFuncId = GenericLibFuncId::new_inline("u128_overflow_sub");
        const OVERFLOWING_MUL: GenericLibFuncId = GenericLibFuncId::new_inline("u128_overflow_mul");
        const DIVMOD: GenericLibFuncId = GenericLibFuncId::new_inline("u128_safe_divmod");
        match id {
            id if id == &OVERFLOWING_ADD => Some(Self::new(IntOperator::OverflowingAdd)),
            id if id == &OVERFLOWING_SUB => Some(Self::new(IntOperator::OverflowingSub)),
            id if id == &OVERFLOWING_MUL => Some(Self::new(IntOperator::OverflowingMul)),
            id if id == &DIVMOD => Some(Self::new(IntOperator::DivMod)),
            _ => None,
        }
    }

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        if !args.is_empty() {
            return Err(SpecializationError::WrongNumberOfGenericArgs);
        }
        let ty = context.get_concrete_type(Uint128Type::id(), &[])?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        match self.operator {
            IntOperator::DivMod => Ok(LibFuncSignature::new_non_branch_ex(
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
            IntOperator::OverflowingAdd
            | IntOperator::OverflowingSub
            | IntOperator::OverflowingMul => Ok(LibFuncSignature {
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
        match args {
            [] => {
                Ok(Uint128OperationConcreteLibFunc::Binary(Uint128BinaryOperationConcreteLibFunc {
                    operator: self.operator,
                    signature: self.specialize_signature(context.upcast(), args)?,
                }))
            }
            [GenericArg::Value(c)] => {
                if matches!(self.operator, IntOperator::DivMod) && c.is_zero() {
                    Err(SpecializationError::UnsupportedGenericArg)
                } else {
                    Ok(Uint128OperationConcreteLibFunc::Const(
                        Uint128OperationWithConstConcreteLibFunc {
                            operator: self.operator,
                            c: u128::try_from(c).unwrap(),
                            signature: self.specialize_signature(context.upcast(), args)?,
                        },
                    ))
                }
            }
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

pub struct Uint128BinaryOperationConcreteLibFunc {
    pub operator: IntOperator,
    pub signature: LibFuncSignature,
}
impl SignatureBasedConcreteLibFunc for Uint128BinaryOperationConcreteLibFunc {
    fn signature(&self) -> &LibFuncSignature {
        &self.signature
    }
}

/// u128 operations with a const.
pub struct Uint128OperationWithConstConcreteLibFunc {
    pub operator: IntOperator,
    pub c: u128,
    pub signature: LibFuncSignature,
}
define_concrete_libfunc_hierarchy! {
    pub enum Uint128OperationConcreteLibFunc {
        Binary(Uint128BinaryOperationConcreteLibFunc),
        Const(Uint128OperationWithConstConcreteLibFunc),
    }
}

impl SignatureBasedConcreteLibFunc for Uint128OperationWithConstConcreteLibFunc {
    fn signature(&self) -> &LibFuncSignature {
        &self.signature
    }
}

/// LibFunc for creating a constant u128.
#[derive(Default)]
pub struct Uint128ConstLibFunc {}
impl NamedLibFunc for Uint128ConstLibFunc {
    type Concrete = Uint128ConstConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("u128_const");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        _args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(Uint128Type::id(), &[])?,
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
            [GenericArg::Value(c)] => Ok(Uint128ConstConcreteLibFunc {
                c: u128::try_from(c).unwrap(),
                signature: <Self as NamedLibFunc>::specialize_signature(
                    self,
                    context.upcast(),
                    args,
                )?,
            }),
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

pub struct Uint128ConstConcreteLibFunc {
    pub c: u128,
    pub signature: LibFuncSignature,
}
impl SignatureBasedConcreteLibFunc for Uint128ConstConcreteLibFunc {
    fn signature(&self) -> &LibFuncSignature {
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

/// LibFunc for comparing u128s.
#[derive(Default)]
pub struct Uint128LessThanLibFunc {}
impl NoGenericArgsGenericLibFunc for Uint128LessThanLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("u128_lt");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature {
            param_signatures: get_u128_comparison_param_signatures(context)?,
            branch_signatures: get_u128_comparison_branch_signatures(context)?,
            fallthrough: Some(0),
        })
    }
}

/// LibFunc for comparing u128s` equality.
#[derive(Default)]
pub struct Uint128EqualLibFunc {}
impl NoGenericArgsGenericLibFunc for Uint128EqualLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("u128_eq");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        let u128_ty = context.get_concrete_type(Uint128Type::id(), &[])?;
        let branch_signatures = (0..2)
            .map(|_| BranchSignature {
                vars: vec![],
                ap_change: SierraApChange::Known { new_vars_only: false },
            })
            .collect();

        Ok(LibFuncSignature {
            param_signatures: vec![
                ParamSignature::new(u128_ty.clone()),
                ParamSignature::new(u128_ty),
            ],
            branch_signatures,
            fallthrough: Some(0),
        })
    }
}

/// LibFunc for comparing u128s.
#[derive(Default)]
pub struct Uint128LessThanOrEqualLibFunc {}
impl NoGenericArgsGenericLibFunc for Uint128LessThanOrEqualLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("u128_le");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature {
            param_signatures: get_u128_comparison_param_signatures(context)?,
            branch_signatures: get_u128_comparison_branch_signatures(context)?,
            fallthrough: Some(0),
        })
    }
}

/// LibFunc for converting a felt into a u128, or the number and the overflow in the case of
/// failure.
#[derive(Default)]
pub struct Uint128sFromFeltLibFunc {}
impl NoGenericArgsGenericLibFunc for Uint128sFromFeltLibFunc {
    // TODO(lior): Rename to split_felt and remove the branches. Add a separate u128_from_felt()
    //   for the conversion.
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("u128s_from_felt");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        Ok(LibFuncSignature {
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

/// LibFunc for converting a u128 into a felt.
#[derive(Default)]
pub struct Uint128ToFeltLibFunc {}
impl NoGenericArgsGenericLibFunc for Uint128ToFeltLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("u128_to_felt");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature::new_non_branch(
            vec![context.get_concrete_type(Uint128Type::id(), &[])?],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(FeltType::id(), &[])?,
                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
