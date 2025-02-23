use super::felt252::Felt252Type;
use super::non_zero::nonzero_ty;
use super::range_check::RangeCheckType;
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
    const ZERO_SIZED: bool = false;
}

/// An EC point is a pair (x,y) on the curve.
#[derive(Default)]
pub struct EcPointType {}
impl NoGenericArgsGenericType for EcPointType {
    const ID: GenericTypeId = GenericTypeId::new_inline("EcPoint");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const ZERO_SIZED: bool = false;
}

/// An EC state is an EC point and a pointer to a random EC point shift.
#[derive(Default)]
pub struct EcStateType {}
impl NoGenericArgsGenericType for EcStateType {
    const ID: GenericTypeId = GenericTypeId::new_inline("EcState");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const ZERO_SIZED: bool = false;
}

define_libfunc_hierarchy! {
    pub enum EcLibfunc {
        IsZero(EcIsZeroLibfunc),
        Neg(EcNegLibfunc),
        StateAdd(EcStateAddLibfunc),
        TryNew(EcCreatePointLibfunc),
        StateFinalize(EcStateFinalizeLibfunc),
        StateInit(EcStateInitLibfunc),
        StateAddMul(EcStateAddMulLibfunc),
        PointFromX(EcPointFromXLibfunc),
        UnwrapPoint(EcUnwrapPointLibfunc),
        Zero(EcZeroLibfunc),
    }, EcConcreteLibfunc
}

/// Libfunc for returning the zero point (the point at infinity).
#[derive(Default)]
pub struct EcZeroLibfunc {}
impl NoGenericArgsGenericLibfunc for EcZeroLibfunc {
    const STR_ID: &'static str = "ec_point_zero";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ecpoint_ty = context.get_concrete_type(EcPointType::id(), &[])?;

        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: ecpoint_ty,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Const),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Libfunc for creating an EC point from its coordinates `x` and `y`.
/// If `(x, y)` is not on the curve, nothing is returned.
#[derive(Default)]
pub struct EcCreatePointLibfunc {}
impl NoGenericArgsGenericLibfunc for EcCreatePointLibfunc {
    const STR_ID: &'static str = "ec_point_try_new_nz";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ecpoint_ty = context.get_concrete_type(EcPointType::id(), &[])?;
        let nonzero_ecpoint_ty = nonzero_ty(context, &ecpoint_ty)?;
        let felt252_param = ParamSignature::new(context.get_concrete_type(Felt252Type::id(), &[])?);

        Ok(LibfuncSignature {
            param_signatures: vec![felt252_param.clone(), felt252_param],
            branch_signatures: vec![
                // Success.
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: nonzero_ecpoint_ty,
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
///
/// If there exists `y` such that `(x, y)` is on the curve, either `(x, y)` or `(x, -y)` (both
/// constitute valid points on the curve) is returned.
/// Otherwise, nothing is returned.
#[derive(Default)]
pub struct EcPointFromXLibfunc {}
impl NoGenericArgsGenericLibfunc for EcPointFromXLibfunc {
    const STR_ID: &'static str = "ec_point_from_x_nz";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt252_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        let ecpoint_ty = context.get_concrete_type(EcPointType::id(), &[])?;
        let nonzero_ecpoint_ty = nonzero_ty(context, &ecpoint_ty)?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;

        let rc_output_info = OutputVarInfo::new_builtin(range_check_type.clone(), 0);
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature::new(range_check_type).with_allow_add_const(),
                ParamSignature::new(felt252_ty),
            ],
            branch_signatures: vec![
                // Success.
                BranchSignature {
                    vars: vec![
                        rc_output_info.clone(),
                        OutputVarInfo {
                            ty: nonzero_ecpoint_ty,
                            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure.
                BranchSignature {
                    vars: vec![rc_output_info],
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
        let felt252_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        let ecpoint_ty = context.get_concrete_type(EcPointType::id(), &[])?;
        let nonzero_ecpoint_ty = nonzero_ty(context, &ecpoint_ty)?;

        let felt252_partial_param_0_output_info = OutputVarInfo {
            ty: felt252_ty,
            ref_info: OutputVarReferenceInfo::PartialParam { param_idx: 0 },
        };
        // TODO(orizi): Consider making the returned `y` value non-zero.
        Ok(LibfuncSignature::new_non_branch(
            vec![nonzero_ecpoint_ty],
            vec![felt252_partial_param_0_output_info.clone(), felt252_partial_param_0_output_info],
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
        let ecpoint_ty = context.get_concrete_type(EcPointType::id(), &[])?;

        Ok(LibfuncSignature::new_non_branch(
            vec![ecpoint_ty.clone()],
            vec![OutputVarInfo {
                ty: ecpoint_ty,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Libfunc for checking whether the given `EcPoint` is the zero point.
#[derive(Default)]
pub struct EcIsZeroLibfunc {}
impl NoGenericArgsGenericLibfunc for EcIsZeroLibfunc {
    const STR_ID: &'static str = "ec_point_is_zero";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ecpoint_ty = context.get_concrete_type(EcPointType::id(), &[])?;
        let nonzero_ecpoint_ty = nonzero_ty(context, &ecpoint_ty)?;

        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(ecpoint_ty)],
            branch_signatures: vec![
                // Zero.
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                },
                // NonZero.
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: nonzero_ecpoint_ty,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                },
            ],
            fallthrough: Some(0),
        })
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
                ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
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
        let ecpoint_ty = context.get_concrete_type(EcPointType::id(), &[])?;
        let nonzero_ecpoint_ty = nonzero_ty(context, &ecpoint_ty)?;

        Ok(LibfuncSignature::new_non_branch(
            vec![state_ty.clone(), nonzero_ecpoint_ty],
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
    const STR_ID: &'static str = "ec_state_try_finalize_nz";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ecpoint_ty = context.get_concrete_type(EcPointType::id(), &[])?;
        let nonzero_ecpoint_ty = nonzero_ty(context, &ecpoint_ty)?;

        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(
                context.get_concrete_type(EcStateType::id(), &[])?,
            )],
            branch_signatures: vec![
                // Non-zero.
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: nonzero_ecpoint_ty,
                        ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Zero.
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
        let ecpoint_ty = context.get_concrete_type(EcPointType::id(), &[])?;
        let nonzero_ecpoint_ty = nonzero_ty(context, &ecpoint_ty)?;

        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(ec_builtin_ty.clone()).with_allow_add_const(),
                ParamSignature::new(ec_state_ty.clone()),
                ParamSignature::new(context.get_concrete_type(Felt252Type::id(), &[])?),
                ParamSignature::new(nonzero_ecpoint_ty),
            ],
            vec![
                OutputVarInfo::new_builtin(ec_builtin_ty, 0),
                OutputVarInfo {
                    ty: ec_state_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
            ],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
