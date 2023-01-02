use super::get_bool_type;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, SierraApChange,
    SignatureSpecializationContext,
};
use crate::extensions::{NoGenericArgsGenericLibfunc, OutputVarReferenceInfo, SpecializationError};
use crate::ids::GenericLibfuncId;

define_libfunc_hierarchy! {
    pub enum BoolLibfunc {
        And(BoolAndLibfunc),
        Not(BoolNotLibfunc),
    }, BoolConcreteLibfunc
}

/// Utility for common boolean libfunc signature definitions.
fn boolean_libfunc_signature(
    context: &dyn SignatureSpecializationContext,
    new_vars_only: bool,
    is_unary: bool,
) -> Result<LibfuncSignature, SpecializationError> {
    let bool_type = get_bool_type(context)?;
    Ok(LibfuncSignature::new_non_branch(
        if is_unary { vec![bool_type.clone()] } else { vec![bool_type.clone(), bool_type.clone()] },
        vec![OutputVarInfo {
            ty: bool_type,
            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
        }],
        SierraApChange::Known { new_vars_only },
    ))
}

/// Libfunc for boolean AND.
#[derive(Default)]
pub struct BoolAndLibfunc {}
impl NoGenericArgsGenericLibfunc for BoolAndLibfunc {
    const ID: GenericLibfuncId = GenericLibfuncId::new_inline("bool_and_impl");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        boolean_libfunc_signature(context, true, false)
    }
}

/// Libfunc for boolean NOT.
#[derive(Default)]
pub struct BoolNotLibfunc {}
impl NoGenericArgsGenericLibfunc for BoolNotLibfunc {
    const ID: GenericLibfuncId = GenericLibfuncId::new_inline("bool_not_impl");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        boolean_libfunc_signature(context, false, true)
    }
}
