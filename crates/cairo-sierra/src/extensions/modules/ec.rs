use super::felt::FeltType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibFuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibFunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::{GenericLibFuncId, GenericTypeId};

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
    pub enum EcLibFunc {
        CreatePoint(EcCreatePointLibFunc),
        InitState(EcInitStateLibFunc),
    }, EcConcreteLibFunc
}

/// LibFunc for creating an EC point. Inputs are verified to be on the curve.
#[derive(Default)]
pub struct EcCreatePointLibFunc {}
impl NoGenericArgsGenericLibFunc for EcCreatePointLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("ec_point_try_create");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        Ok(LibFuncSignature {
            param_signatures: vec![
                ParamSignature::new(felt_ty.clone()),
                ParamSignature::new(felt_ty),
            ],
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

/// LibFunc for initializing an EC state from an EC point.
#[derive(Default)]
pub struct EcInitStateLibFunc {}
impl NoGenericArgsGenericLibFunc for EcInitStateLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("ec_init_state");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ec_point_ty = context.get_concrete_type(EcPointType::id(), &[])?;
        let ec_state_ty = context.get_concrete_type(EcStateType::id(), &[])?;
        Ok(LibFuncSignature::new_non_branch(
            vec![ec_point_ty],
            vec![OutputVarInfo {
                ty: ec_state_ty,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}
