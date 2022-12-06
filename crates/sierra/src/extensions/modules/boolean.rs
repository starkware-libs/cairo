use super::get_bool_type;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibFuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::{NoGenericArgsGenericLibFunc, OutputVarReferenceInfo, SpecializationError};
use crate::ids::GenericLibFuncId;

define_libfunc_hierarchy! {
    pub enum BoolLibFunc {
        And(BoolAndLibFunc),
    }, BoolConcreteLibFunc
}

/// LibFunc for boolean AND.
#[derive(Default)]
pub struct BoolAndLibFunc {}
impl NoGenericArgsGenericLibFunc for BoolAndLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("bool_and_impl");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        let bool_type = get_bool_type(context)?;
        Ok(LibFuncSignature {
            param_signatures: vec![
                ParamSignature::new(bool_type.clone()),
                ParamSignature::new(bool_type.clone()),
            ],
            branch_signatures: vec![BranchSignature {
                vars: vec![OutputVarInfo {
                    ty: bool_type,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                }],
                ap_change: SierraApChange::Known { new_vars_only: false },
            }],
            fallthrough: Some(0),
        })
    }
}
