use crate::extensions::lib_func::{
    BranchSignature, LibfuncSignature, SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::{NoGenericArgsGenericLibfunc, SpecializationError};

#[derive(Default)]
pub struct UnconditionalJumpLibfunc {}
impl NoGenericArgsGenericLibfunc for UnconditionalJumpLibfunc {
    const STR_ID: &'static str = "jump";

    fn specialize_signature(
        &self,
        _context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature {
            param_signatures: vec![],
            branch_signatures: vec![BranchSignature {
                vars: vec![],
                ap_change: SierraApChange::Known { new_vars_only: true },
            }],
            fallthrough: None,
        })
    }
}
