use super::get_bool_type;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibFuncSignature, OutputVarInfo, SierraApChange,
    SignatureSpecializationContext,
};
use crate::extensions::{NoGenericArgsGenericLibFunc, OutputVarReferenceInfo, SpecializationError};
use crate::ids::GenericLibFuncId;

define_libfunc_hierarchy! {
    pub enum BoolLibFunc {
        And(BoolAndLibFunc),
        Not(BoolNotLibFunc),
    }, BoolConcreteLibFunc
}

/// Utility for common boolean libfunc signature definitions.
fn boolean_libfunc_signature(
    context: &dyn SignatureSpecializationContext,
    new_vars_only: bool,
    is_unary: bool,
) -> Result<LibFuncSignature, SpecializationError> {
    let bool_type = get_bool_type(context)?;
    Ok(LibFuncSignature::new_non_branch(
        if is_unary { vec![bool_type.clone()] } else { vec![bool_type.clone(), bool_type.clone()] },
        vec![OutputVarInfo {
            ty: bool_type,
            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
        }],
        SierraApChange::Known { new_vars_only },
    ))
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
        boolean_libfunc_signature(context, true, false)
    }
}

/// LibFunc for boolean NOT.
#[derive(Default)]
pub struct BoolNotLibFunc {}
impl NoGenericArgsGenericLibFunc for BoolNotLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("bool_not_impl");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        boolean_libfunc_signature(context, false, true)
    }
}
