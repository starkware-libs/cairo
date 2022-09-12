use casm::ap_change::ApChange;
use thiserror::Error;

pub mod ap_tracking;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum EnvironmentError {
    #[error("Inconsistent ap tracking")]
    InconsistentApTracking,
}

/// An extension of the ProgramAnnotations that is accisiable to libfuncs.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Environment {
    // The ap tracking from the beginning of the current function.
    // Once it changes to ApChange::Unknown it remains in that state.
    pub ap_tracking: ApChange,
}

/// Validates that the a a==b and if not returns the appropriate error,
pub fn validate_environment_equalit(
    a: &Environment,
    b: &Environment,
) -> Result<(), EnvironmentError> {
    if a.ap_tracking != b.ap_tracking {
        return Err(EnvironmentError::InconsistentApTracking);
    }
    Ok(())
}
