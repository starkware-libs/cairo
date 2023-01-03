use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    LibfuncSignature, SignatureSpecializationContext, SierraApChange, OutputVarInfo, ParamSignature, BranchSignature,
};
use crate::extensions::{SpecializationError, NoGenericArgsGenericLibfunc, NamedType, OutputVarReferenceInfo};
use crate::ids::{GenericLibfuncId};

use super::felt::FeltType;

define_libfunc_hierarchy! {
    pub enum CheatcodesLibFunc {
        Roll(RollLibFunc),
    }, CheatcodesConcreteLibFunc
}

/// LibFunc for creating a new array.
#[derive(Default)]
pub struct RollLibFunc {}
impl NoGenericArgsGenericLibfunc for RollLibFunc {
    const ID: GenericLibfuncId = GenericLibfuncId::new_inline("cheat_roll");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                // Address
                ParamSignature::new(felt_ty.clone()),
                // Value
                ParamSignature::new(felt_ty.clone()),
            ],
            branch_signatures: vec![
                // Success branch
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![
                        // Error reason
                        OutputVarInfo {
                            ty: felt_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
        // Ok(LibFuncSignature::new_non_branch(vec![
        //     felt_ty_a,
        //     felt_ty_b
        // ], vec![
        //     OutputVarInfo {
        //         ty: felt_ty_o,
        //         ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
        //     },
        // ], SierraApChange::Known { new_vars_only: true} ))
    }
}
