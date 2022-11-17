use crate::extensions::lib_func::{
    LibFuncSignature, SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::{NoGenericArgsGenericLibFunc, SpecializationError};
use crate::ids::GenericLibFuncId;

/// Revoke the ap tracking.
/// This LibFunc is changes to ap_tracking state to unknown,
/// allowing a path with known ap tracking to converge with a path with unknown ap tracking.
#[derive(Default)]
pub struct RevokeApTrackingLibFunc {}
impl NoGenericArgsGenericLibFunc for RevokeApTrackingLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("revoke_ap_tracking");

    fn specialize_signature(
        &self,
        _context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature::new_non_branch(vec![], vec![], SierraApChange::Unknown))
    }
}
