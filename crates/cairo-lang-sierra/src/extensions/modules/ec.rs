use super::felt::FeltType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::GenericTypeId;

// Type representing the EcOp builtin.
#[derive(Default)]
pub struct EcOpType {}
impl NoGenericArgsGenericType for EcOpType {
    const ID: GenericTypeId = GenericTypeId::new_inline("EcOp");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = false;
    const DROPPABLE: bool = false;
    const SIZE: i16 = 1;
}

/// An EC point is a pair (x,y) on the curve.
#[derive(Default)]
pub struct EcPointType {}
impl NoGenericArgsGenericType for EcPointType {
    const ID: GenericTypeId = GenericTypeId::new_inline("EcPoint");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const SIZE: i16 = 2;
}

/// An EC state is an EC point and a pointer to a random EC point shift.
#[derive(Default)]
pub struct EcStateType {}
impl NoGenericArgsGenericType for EcStateType {
    const ID: GenericTypeId = GenericTypeId::new_inline("EcState");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const SIZE: i16 = 3;
}

define_libfunc_hierarchy! {
    pub enum EcLibfunc {
        Neg(EcNegLibfunc),
        StateAdd(EcStateAddLibfunc),
        TryNew(EcCreatePointLibfunc),
        StateFinalize(EcStateFinalizeLibfunc),
        StateInit(EcStateInitLibfunc),
        StateAddMul(EcStateAddMulLibfunc),
        PointFromX(EcPointFromXLibfunc),
        UnwrapPoint(EcUnwrapPointLibfunc),
    }, EcConcreteLibfunc
}

/// Libfunc for creating an EC point from its coordinates `x` and `y`.
/// If `(x, y)` is not on the curve, nothing is returned.
#[derive(Default)]
pub struct EcCreatePointLibfunc {}
impl NoGenericArgsGenericLibfunc for EcCreatePointLibfunc {
    const STR_ID: &'static str = "ec_point_try_new";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature::new(felt_ty.clone()),
                ParamSignature::new(felt_ty),
            ],
            branch_signatures: vec![
                // Success.
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: context.get_concrete_type(EcPointType::id(), &[])?,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure.
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// Libfunc for creating an EC point from its x coordinate.
/// If there exists `y` such that `(x, y)` is on the curve, either `(x, y)` or `(x, -y)` (both
/// constitute valid points on the curve) is returned.
/// Otherwise, nothing is returned.
#[derive(Default)]
pub struct EcPointFromXLibfunc {}
impl NoGenericArgsGenericLibfunc for EcPointFromXLibfunc {
    const STR_ID: &'static str = "ec_point_from_x";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(felt_ty)],
            branch_signatures: vec![
                // Success.
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: context.get_concrete_type(EcPointType::id(), &[])?,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure.
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// Libfunc for unwrapping the x,y values of an EC point.
#[derive(Default)]
pub struct EcUnwrapPointLibfunc {}
impl NoGenericArgsGenericLibfunc for EcUnwrapPointLibfunc {
    const STR_ID: &'static str = "ec_point_unwrap";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch(
            vec![context.get_concrete_type(EcPointType::id(), &[])?],
            vec![
                OutputVarInfo {
                    ty: felt_ty.clone(),
                    ref_info: OutputVarReferenceInfo::PartialParam { param_idx: 0 },
                },
                OutputVarInfo {
                    ty: felt_ty,
                    ref_info: OutputVarReferenceInfo::PartialParam { param_idx: 0 },
                },
            ],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Libfunc for unwrapping the x,y values of an EC point.
#[derive(Default)]
pub struct EcNegLibfunc {}
impl NoGenericArgsGenericLibfunc for EcNegLibfunc {
    const STR_ID: &'static str = "ec_neg";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let point_ty = context.get_concrete_type(EcPointType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch(
            vec![point_ty.clone()],
            vec![OutputVarInfo {
                ty: point_ty,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Libfunc for initializing an EC state from an EC point.
#[derive(Default)]
pub struct EcStateInitLibfunc {}
impl NoGenericArgsGenericLibfunc for EcStateInitLibfunc {
    const STR_ID: &'static str = "ec_state_init";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(EcStateType::id(), &[])?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for initializing an EC state from an EC point.
#[derive(Default)]
pub struct EcStateAddLibfunc {}
impl NoGenericArgsGenericLibfunc for EcStateAddLibfunc {
    const STR_ID: &'static str = "ec_state_add";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let state_ty = context.get_concrete_type(EcStateType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch(
            vec![state_ty.clone(), context.get_concrete_type(EcPointType::id(), &[])?],
            vec![OutputVarInfo {
                ty: state_ty,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for initializing an EC state from an EC point.
#[derive(Default)]
pub struct EcStateFinalizeLibfunc {}
impl NoGenericArgsGenericLibfunc for EcStateFinalizeLibfunc {
    const STR_ID: &'static str = "ec_state_finalize";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(
                context.get_concrete_type(EcStateType::id(), &[])?,
            )],
            branch_signatures: vec![
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: context.get_concrete_type(EcPointType::id(), &[])?,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// Libfunc for applying the EC op builtin: given an EC state `S`, a scalar `M` and an EC point `Q`,
/// computes a new EC state `S + M * Q`.
#[derive(Default)]
pub struct EcStateAddMulLibfunc {}
impl NoGenericArgsGenericLibfunc for EcStateAddMulLibfunc {
    const STR_ID: &'static str = "ec_state_add_mul";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ec_builtin_ty = context.get_concrete_type(EcOpType::id(), &[])?;
        let ec_state_ty = context.get_concrete_type(EcStateType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch(
            vec![
                ec_builtin_ty.clone(),
                ec_state_ty.clone(),
                context.get_concrete_type(FeltType::id(), &[])?,
                context.get_concrete_type(EcPointType::id(), &[])?,
            ],
            vec![
                OutputVarInfo {
                    ty: ec_builtin_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                },
                OutputVarInfo {
                    ty: ec_state_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
            ],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
