use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    LibFuncSignature, SignatureSpecializationContext, SierraApChange,
};
use crate::extensions::{SpecializationError, NoGenericArgsGenericLibFunc, NamedType};
use crate::ids::{GenericLibFuncId};

use super::felt::FeltType;

define_libfunc_hierarchy! {
    pub enum CheatcodesLibFunc {
        Roll(RollLibFunc),
    }, CheatcodesConcreteLibFunc
}

/// LibFunc for creating a new array.
#[derive(Default)]
pub struct RollLibFunc {}
impl NoGenericArgsGenericLibFunc for RollLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("roll");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        let felt_ty_a = context.get_concrete_type(FeltType::id(), &[])?;
        let felt_ty_b = context.get_concrete_type(FeltType::id(), &[])?;
        Ok(LibFuncSignature::new_non_branch(vec![
            felt_ty_a,
            felt_ty_b
        ], vec![], SierraApChange::Known { new_vars_only: true} ))
    }
}
