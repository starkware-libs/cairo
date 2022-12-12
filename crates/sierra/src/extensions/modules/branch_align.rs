use crate::extensions::lib_func::{
    LibFuncSignature, SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::{NoGenericArgsGenericLibFunc, SpecializationError};
use crate::ids::GenericLibFuncId;

/// LibFunc for aligning branches.
/// Used to equalize environment changes across merging paths.
/// This may include gas usages and ap changes.
#[derive(Default)]
pub struct BranchAlignLibFunc {}
impl NoGenericArgsGenericLibFunc for BranchAlignLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("branch_align");

    fn specialize_signature(
        &self,
        _context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature::new_non_branch(
            vec![],
            vec![],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}
