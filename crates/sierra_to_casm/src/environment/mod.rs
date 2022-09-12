use thiserror::Error;

use self::frame_state::FrameState;

pub mod frame_state;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum EnvironmentError {
    #[error("Inconsistent frame state")]
    InconsistentFrameState,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Environment {
    pub frame_state: FrameState,
}

pub fn validate_equal_equality(a: &Environment, b: &Environment) -> Result<(), EnvironmentError> {
    if a.frame_state != b.frame_state {
        return Err(EnvironmentError::InconsistentFrameState);
    }
    Ok(())
}
