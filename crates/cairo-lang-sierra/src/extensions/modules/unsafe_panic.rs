use crate::extensions::lib_func::{
    LibfuncSignature, ParamSignature, SignatureOnlyGenericLibfunc, SignatureSpecializationContext
    
};
use crate::extensions::{
      args_as_single_type, SpecializationError    
};
use crate::program::GenericArg;




/// Libfunc for finalizing the locals for current function.
#[derive(Default)]
pub struct UnsafePanicLibfunc {}
impl SignatureOnlyGenericLibfunc for UnsafePanicLibfunc {
    const STR_ID: &'static str = "unsafe_panic";

    fn specialize_signature(
        &self,
        _context: &dyn SignatureSpecializationContext,
        generic_args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(generic_args)?;
        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature {
                ty,
                allow_deferred: true,
                allow_add_const: true,
                allow_const: true,
            }],
            branch_signatures: vec![],
            fallthrough: None,
        })
    }
}
