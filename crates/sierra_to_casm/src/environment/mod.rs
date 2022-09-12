use casm::ap_change::ApChange;
use thiserror::Error;

pub mod ap_tracking;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum EnvironmentError {
    #[error("Inconsistent ap tracking")]
    InconsistentApTracking,
}

/// Part of the program annotations that libfuncs may access as part of their run.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Environment {
    // The ap tracking from the beginning of the current function.
    // Once it changes to ApChange::Unknown it remains in that state.
    pub ap_tracking: ApChange,
}

// Validates that the environments match and returns appropriate error if not.
pub fn validate_environment_equality(
    a: &Environment,
    b: &Environment,
) -> Result<(), EnvironmentError> {
    if a.ap_tracking != b.ap_tracking {
        return Err(EnvironmentError::InconsistentApTracking);
    }
    Ok(())
}
