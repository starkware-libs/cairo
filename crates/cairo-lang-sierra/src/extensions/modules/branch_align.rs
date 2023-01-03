use crate::extensions::lib_func::{
    LibfuncSignature, SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::{NoGenericArgsGenericLibfunc, SpecializationError};
use crate::ids::GenericLibfuncId;

/// Libfunc for aligning branches.
/// Used to equalize environment changes across merging paths.
/// This may include gas usages and ap changes.
#[derive(Default)]
pub struct BranchAlignLibfunc {}
impl NoGenericArgsGenericLibfunc for BranchAlignLibfunc {
    const ID: GenericLibfuncId = GenericLibfuncId::new_inline("branch_align");

    fn specialize_signature(
        &self,
        _context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}
