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
use crate::ids::{GenericLibfuncId, GenericTypeId};

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

define_libfunc_hierarchy! {
    pub enum EcLibfunc {
        CreatePoint(EcCreatePointLibfunc),
    }, EcConcreteLibfunc
}

/// Libfunc for creating an EC point. Inputs are verified to be on the curve.
#[derive(Default)]
pub struct EcCreatePointLibfunc {}
impl NoGenericArgsGenericLibfunc for EcCreatePointLibfunc {
    const ID: GenericLibfuncId = GenericLibfuncId::new_inline("ec_point_try_create");

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
