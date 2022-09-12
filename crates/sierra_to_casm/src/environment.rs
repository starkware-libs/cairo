use thiserror::Error;

use crate::frame_state::FrameState;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum EnvironmentError {
    #[error("Inconsistent frame state")]
    InconsistentFrameState,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Environment {
    pub frame_state: FrameState,
}

pub fn assert_equal_environments(a: &Environment, b: &Environment) -> Result<(), EnvironmentError> {
    if a.frame_state != b.frame_state {
        return Err(EnvironmentError::InconsistentFrameState);
    }
    Ok(())
}
