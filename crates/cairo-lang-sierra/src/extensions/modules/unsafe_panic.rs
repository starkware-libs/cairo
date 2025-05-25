use crate::extensions::lib_func::{LibfuncSignature, SignatureSpecializationContext};
use crate::extensions::{NoGenericArgsGenericLibfunc, SpecializationError};

/// Libfunc for generating unprovable code when a panic is encountered.
#[derive(Default)]
pub struct UnsafePanicLibfunc {}
impl NoGenericArgsGenericLibfunc for UnsafePanicLibfunc {
    const STR_ID: &'static str = "unsafe_panic";

    fn specialize_signature(
        &self,
        _context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature {
            param_signatures: vec![],
            branch_signatures: vec![],
            fallthrough: None,
        })
    }
}
