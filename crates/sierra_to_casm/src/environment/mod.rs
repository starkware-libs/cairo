use casm::ap_change::ApChange;
use frame_state::FrameState;
use thiserror::Error;

pub mod ap_tracking;
pub mod frame_state;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum EnvironmentError {
    #[error("Inconsistent ap tracking")]
    InconsistentApTracking,
    #[error("Inconsistent frame state")]
    InconsistentFrameState,
}

/// Part of the program annotations that libfuncs may access as part of their run.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Environment {
    // The ap tracking from the beginning of the current function.
    // Once it changes to ApChange::Unknown it remains in that state.
    pub ap_tracking: ApChange,
    pub frame_state: FrameState,
}
impl Default for Environment {
    fn default() -> Self {
        let ap_tracking = ApChange::Known(0);
        Self {
            ap_tracking,
            frame_state: FrameState::Allocating { allocated: 0, last_ap_tracking: ap_tracking },
        }
    }
}

// Validates that the environments match and returns appropriate error if not.
pub fn validate_environment_equality(
    a: &Environment,
    b: &Environment,
) -> Result<(), EnvironmentError> {
    if a.ap_tracking != b.ap_tracking {
        Err(EnvironmentError::InconsistentApTracking)
    } else if a.frame_state != b.frame_state {
        Err(EnvironmentError::InconsistentFrameState)
    } else {
        Ok(())
    }
}
