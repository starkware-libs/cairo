use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    LibfuncSignature, SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::{NoGenericArgsGenericLibfunc, SpecializationError};

define_libfunc_hierarchy! {
    pub enum ApTrackingLibfunc {
        Revoke(RevokeApTrackingLibfunc),
        Enable(EnableApTrackingLibfunc),
    }, ApTrackingConcreteLibfunc
}

/// Revoke the ap tracking.
/// This Libfunc is changes to ap_tracking state to unknown,
/// allowing a path with known ap tracking to converge with a path with unknown ap tracking.
#[derive(Default)]
pub struct RevokeApTrackingLibfunc {}
impl NoGenericArgsGenericLibfunc for RevokeApTrackingLibfunc {
    const STR_ID: &'static str = "revoke_ap_tracking";

    fn specialize_signature(
        &self,
        _context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch(vec![], vec![], SierraApChange::Unknown))
    }
}

/// Enable ap tracking.
/// This Libfunc is used to enable ap tracking to allow branches that may diverge and merge after
/// this point to have an aligned ap.
#[derive(Default)]
pub struct EnableApTrackingLibfunc {}
impl NoGenericArgsGenericLibfunc for EnableApTrackingLibfunc {
    const STR_ID: &'static str = "enable_ap_tracking";

    fn specialize_signature(
        &self,
        _context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
