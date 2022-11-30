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

/// Type for uint128.
#[derive(Default)]
pub struct Uint128Type {}
impl NoGenericArgsGenericType for Uint128Type {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("uint128");

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
        LessThanOrEqual(Uint128LessThanOrEqualLibFunc),
        Const(Uint128ConstLibFunc),
        FromFelt(Uint128FromFeltLibFunc),
        ToFelt(Uint128ToFeltLibFunc),
        JumpNotZero(Uint128JumpNotZeroLibFunc),
    }, Uint128Concrete
}

#[derive(Default)]
pub struct Uint128Traits {}
impl JumpNotZeroTraits for Uint128Traits {
    const JUMP_NOT_ZERO: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_jump_nz");
    const GENERIC_TYPE_ID: GenericTypeId = <Uint128Type as NamedType>::ID;
}
pub type Uint128JumpNotZeroLibFunc = JumpNotZeroLibFunc<Uint128Traits>;

/// Operators for integers.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IntOperator {
    WrappingAdd,
    WrappingSub,
    WrappingMul,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    DivMod,
}

/// Libfunc for uint128 operations.
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
        const WRAPPING_ADD: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_wrapping_add");
        const WRAPPING_SUB: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_wrapping_sub");
        const WRAPPING_MUL: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_wrapping_mul");
        const ADD: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_checked_add");
        const SUB: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_checked_sub");
        const MUL: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_checked_mul");
        const DIV: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_div");
        const MOD: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_mod");
        const DIVMOD: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_divmod");
        match id {
            id if id == &WRAPPING_ADD => Some(Self::new(IntOperator::WrappingAdd)),
            id if id == &WRAPPING_SUB => Some(Self::new(IntOperator::WrappingSub)),
            id if id == &WRAPPING_MUL => Some(Self::new(IntOperator::WrappingMul)),
            id if id == &ADD => Some(Self::new(IntOperator::Add)),
            id if id == &SUB => Some(Self::new(IntOperator::Sub)),
            id if id == &MUL => Some(Self::new(IntOperator::Mul)),
            id if id == &DIV => Some(Self::new(IntOperator::Div)),
            id if id == &MOD => Some(Self::new(IntOperator::Mod)),
            id if id == &DIVMOD => Some(Self::new(IntOperator::DivMod)),
            _ => None,
        }
    }

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = context.get_concrete_type(Uint128Type::id(), &[])?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        match (args, self.operator) {
            ([], IntOperator::Div | IntOperator::Mod) => Ok(LibFuncSignature::new_non_branch(
                vec![
                    range_check_type.clone(),
                    ty.clone(),
                    context.get_wrapped_concrete_type(NonZeroType::id(), ty.clone())?,
                ],
                vec![
                    OutputVarInfo {
                        ty: range_check_type,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                            param_idx: 0,
                        }),
                    },
                    OutputVarInfo {
                        ty,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                    },
                ],
                SierraApChange::NotImplemented,
            )),
            ([], IntOperator::DivMod) => Ok(LibFuncSignature::new_non_branch(
                vec![
                    range_check_type.clone(),
                    ty.clone(),
                    context.get_wrapped_concrete_type(NonZeroType::id(), ty.clone())?,
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
                        ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                    },
                    OutputVarInfo { ty, ref_info: OutputVarReferenceInfo::NewTempVar { idx: 1 } },
                ],
                SierraApChange::Known(5),
            )),
            (
                [],
                IntOperator::WrappingAdd | IntOperator::WrappingSub | IntOperator::WrappingMul,
            ) => Ok(LibFuncSignature::new_non_branch(
                vec![range_check_type.clone(), ty.clone(), ty.clone()],
                vec![
                    OutputVarInfo {
                        ty: range_check_type,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                            param_idx: 0,
                        }),
                    },
                    OutputVarInfo {
                        ty,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                    },
                ],
                SierraApChange::NotImplemented,
            )),
            ([], IntOperator::Add | IntOperator::Sub | IntOperator::Mul) => Ok(LibFuncSignature {
                param_signatures: vec![
                    ParamSignature::new(range_check_type.clone()),
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
                                ty,
                                ref_info: OutputVarReferenceInfo::Deferred(
                                    DeferredOutputKind::Generic,
                                ),
                            },
                        ],
                        ap_change: SierraApChange::Known(2),
                    },
                    BranchSignature {
                        vars: vec![OutputVarInfo {
                            ty: range_check_type,
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 0 },
                            ),
                        }],
                        ap_change: SierraApChange::Known(3),
                    },
                ],
                fallthrough: Some(0),
            }),
            ([GenericArg::Value(c)], IntOperator::Div | IntOperator::Mod) if !c.is_zero() => {
                Ok(LibFuncSignature::new_non_branch(
                    vec![range_check_type.clone(), ty.clone()],
                    vec![
                        OutputVarInfo {
                            ty: range_check_type,
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 0 },
                            ),
                        },
                        OutputVarInfo {
                            ty,
                            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                        },
                    ],
                    SierraApChange::NotImplemented,
                ))
            }
            ([GenericArg::Value(c)], IntOperator::DivMod) if !c.is_zero() => {
                Ok(LibFuncSignature::new_non_branch(
                    vec![range_check_type.clone(), ty.clone()],
                    vec![
                        OutputVarInfo {
                            ty: range_check_type,
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 0 },
                            ),
                        },
                        OutputVarInfo {
                            ty: ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                        },
                        OutputVarInfo {
                            ty,
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 1 },
                        },
                    ],
                    SierraApChange::Known(5),
                ))
            }
            (
                [GenericArg::Value(_c)],
                IntOperator::WrappingAdd | IntOperator::WrappingSub | IntOperator::WrappingMul,
            ) => Ok(LibFuncSignature::new_non_branch(
                vec![range_check_type.clone(), ty.clone()],
                vec![
                    OutputVarInfo {
                        ty: range_check_type,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                            param_idx: 0,
                        }),
                    },
                    OutputVarInfo {
                        ty,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                    },
                ],
                SierraApChange::NotImplemented,
            )),
            ([GenericArg::Value(_c)], IntOperator::Add | IntOperator::Sub | IntOperator::Mul) => {
                Ok(LibFuncSignature {
                    param_signatures: vec![
                        ParamSignature::new(range_check_type.clone()),
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
                                    ty,
                                    ref_info: OutputVarReferenceInfo::Deferred(
                                        DeferredOutputKind::Generic,
                                    ),
                                },
                            ],
                            ap_change: SierraApChange::NotImplemented,
                        },
                        BranchSignature {
                            vars: vec![OutputVarInfo {
                                ty: range_check_type,
                                ref_info: OutputVarReferenceInfo::Deferred(
                                    DeferredOutputKind::AddConst { param_idx: 0 },
                                ),
                            }],
                            ap_change: SierraApChange::NotImplemented,
                        },
                    ],
                    fallthrough: Some(0),
                })
            }
            _ => Err(SpecializationError::UnsupportedGenericArg),
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
                if matches!(
                    self.operator,
                    IntOperator::Div | IntOperator::Mod | IntOperator::DivMod
                ) && c.is_zero()
                {
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

/// uint128 operations with a const.
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

/// LibFunc for creating a constant uint128.
#[derive(Default)]
pub struct Uint128ConstLibFunc {}
impl NamedLibFunc for Uint128ConstLibFunc {
    type Concrete = Uint128ConstConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_const");

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
            SierraApChange::Known(0),
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

fn get_uint128_comparison_types(
    context: &dyn SignatureSpecializationContext,
) -> Result<(ConcreteTypeId, ConcreteTypeId), SpecializationError> {
    // Returns uint128 and range_check types.
    Ok((
        context.get_concrete_type(Uint128Type::id(), &[])?,
        context.get_concrete_type(RangeCheckType::id(), &[])?,
    ))
}

/// Utility method to output the two branches of uint128 comparison signatures.
fn get_uint128_comparison_branch_signatures(
    context: &dyn SignatureSpecializationContext,
    ap_changes: [usize; 2],
) -> Result<Vec<BranchSignature>, SpecializationError> {
    let (_, range_check_type) = get_uint128_comparison_types(context)?;
    Ok(ap_changes
        .iter()
        .map(|ap_change| BranchSignature {
            vars: vec![OutputVarInfo {
                ty: range_check_type.clone(),
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                    param_idx: 0,
                }),
            }],
            ap_change: SierraApChange::Known(*ap_change),
        })
        .collect())
}

/// Utility method to output the parameter signatures of uint128 comparison signatures.
fn get_uint128_comparison_param_signatures(
    context: &dyn SignatureSpecializationContext,
) -> Result<Vec<ParamSignature>, SpecializationError> {
    let (uint128_type, range_check_type) = get_uint128_comparison_types(context)?;
    Ok(vec![
        ParamSignature::new(range_check_type),
        ParamSignature::new(uint128_type.clone()),
        ParamSignature::new(uint128_type),
    ])
}

/// LibFunc for comparing uint128s.
#[derive(Default)]
pub struct Uint128LessThanLibFunc {}
impl NoGenericArgsGenericLibFunc for Uint128LessThanLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_lt");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature {
            param_signatures: get_uint128_comparison_param_signatures(context)?,
            branch_signatures: get_uint128_comparison_branch_signatures(context, [2, 3])?,
            fallthrough: Some(0),
        })
    }
}

/// LibFunc for comparing uint128s.
#[derive(Default)]
pub struct Uint128LessThanOrEqualLibFunc {}
impl NoGenericArgsGenericLibFunc for Uint128LessThanOrEqualLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_le");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature {
            param_signatures: get_uint128_comparison_param_signatures(context)?,
            branch_signatures: get_uint128_comparison_branch_signatures(context, [3, 2])?,
            fallthrough: Some(0),
        })
    }
}

/// LibFunc for converting a felt into a uint128.
#[derive(Default)]
pub struct Uint128FromFeltLibFunc {}
impl NoGenericArgsGenericLibFunc for Uint128FromFeltLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_try_from_felt");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        Ok(LibFuncSignature {
            param_signatures: vec![
                ParamSignature::new(range_check_type.clone()),
                ParamSignature {
                    ty: context.get_concrete_type(FeltType::id(), &[])?,
                    allow_deferred: false,
                    allow_add_const: false,
                    allow_const: true,
                },
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
                    ap_change: SierraApChange::Known(1),
                },
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: range_check_type,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                            param_idx: 0,
                        }),
                    }],
                    ap_change: SierraApChange::Known(4),
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// LibFunc for converting a uint128 into a felt.
#[derive(Default)]
pub struct Uint128ToFeltLibFunc {}
impl NoGenericArgsGenericLibFunc for Uint128ToFeltLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_to_felt");

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
            SierraApChange::Known(0),
        ))
    }
}
