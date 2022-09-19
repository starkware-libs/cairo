use crate::extensions::lib_func::{
    BranchReferenceInfo, LibFuncSignature, SignatureOnlyConcreteLibFunc, SpecializationContext,
};
use crate::extensions::{NoGenericArgsGenericLibFunc, SpecializationError};
use crate::ids::GenericLibFuncId;

/// Revoke the ap tracking.
/// This LibFunc is changes to ap_tracking state to unknown, allowing
/// allowing a path with known ap tracking to converge with a path with unknown ap tracking.
#[derive(Default)]
pub struct RevokeApTrackingLibFunc {}
impl NoGenericArgsGenericLibFunc for RevokeApTrackingLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("revoke_ap_tracking");

    fn specialize_signature(
        &self,
        _context: SpecializationContext<'_>,
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature {
            input_types: vec![],
            output_types: vec![vec![]],
            fallthrough: None,
            output_ref_info: vec![BranchReferenceInfo(vec![])],
        })
    }

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibFunc {
            signature: <Self as NoGenericArgsGenericLibFunc>::specialize_signature(self, context)?,
        })
    }
}
