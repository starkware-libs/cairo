use super::felt::FeltType;
use super::jump_not_zero::{JumpNotZeroLibfunc, JumpNotZeroTraits};
use super::non_zero::NonZeroType;
use super::range_check::RangeCheckType;
use super::uint::{
    IntOperator, UintConstLibfunc, UintEqualLibfunc, UintLessThanLibfunc,
    UintLessThanOrEqualLibfunc, UintTraits, UintType,
};
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{
    GenericLibfunc, NamedType, NoGenericArgsGenericLibfunc, OutputVarReferenceInfo,
    SignatureBasedConcreteLibfunc, SpecializationError,
};
use crate::ids::{id_from_string, GenericLibfuncId, GenericTypeId};
use crate::program::GenericArg;

/// Type for u128.
pub type Uint128Type = UintType<Uint128Traits>;

define_libfunc_hierarchy! {
    pub enum Uint128Libfunc {
        Operation(Uint128OperationLibfunc),
        LessThan(UintLessThanLibfunc<Uint128Traits>),
        Equal(UintEqualLibfunc<Uint128Traits>),
        LessThanOrEqual(UintLessThanOrEqualLibfunc<Uint128Traits>),
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
    const CONST: &'static str = "u128_const";
    const EQUAL: &'static str = "u128_eq";
    const LESS_THAN: &'static str = "u128_lt";
    const LESS_THAN_OR_EQUAL: &'static str = "u128_le";
}

impl JumpNotZeroTraits for Uint128Traits {
    const JUMP_NOT_ZERO: &'static str = "u128_jump_nz";
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
        const OVERFLOWING_ADD: u64 = id_from_string("u128_overflowing_add");
        const OVERFLOWING_SUB: u64 = id_from_string("u128_overflowing_sub");
        const OVERFLOWING_MUL: u64 = id_from_string("u128_overflowing_mul");
        const DIVMOD: u64 = id_from_string("u128_safe_divmod");
        const WIDE_MUL: u64 = id_from_string("u128_wide_mul");
        match id.id {
            OVERFLOWING_ADD => Some(Self::new(IntOperator::OverflowingAdd)),
            OVERFLOWING_SUB => Some(Self::new(IntOperator::OverflowingSub)),
            OVERFLOWING_MUL => Some(Self::new(IntOperator::OverflowingMul)),
            DIVMOD => Some(Self::new(IntOperator::DivMod)),
            WIDE_MUL => Some(Self::new(IntOperator::WideMul)),
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

/// Libfunc for converting a felt into a u128, or the number and the overflow in the case of
/// failure.
#[derive(Default)]
pub struct Uint128sFromFeltLibfunc {}
impl NoGenericArgsGenericLibfunc for Uint128sFromFeltLibfunc {
    // TODO(lior): Rename to split_felt and remove the branches. Add a separate u128_from_felt()
    //   for the conversion.
    const STR_ID: &'static str = "u128s_from_felt";

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
    const STR_ID: &'static str = "u128_to_felt";

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
