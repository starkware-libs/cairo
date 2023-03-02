use cairo_lang_casm::ap_change::ApChange;
use cairo_lang_sierra::program::StatementIdx;
use frame_state::{FrameState, FrameStateError};
use thiserror::Error;

use self::frame_state::validate_final_frame_state;
use self::gas_wallet::GasWallet;

pub mod ap_tracking;
pub mod frame_state;
pub mod gas_wallet;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum EnvironmentError {
    #[error("Inconsistent ap tracking.")]
    InconsistentApTracking,
    #[error("Inconsistent ap tracking base.")]
    InconsistentApTrackingBase,
    #[error("Inconsistent frame state.")]
    InconsistentFrameState,
    #[error("Inconsistent gas wallet state.")]
    InconsistentGasWallet,
    #[error("{0}")]
    InvalidFinalFrameState(FrameStateError),
}

/// Part of the program annotations that libfuncs may access as part of their run.
#[derive(Clone, Debug)]
pub struct Environment {
    /// The ap change starting from `ap_tracking_base`.
    /// Once it changes to ApChange::Unknown it remains in that state, unless it is reenabled.
    pub ap_tracking: ApChange,
    pub ap_tracking_base: Option<StatementIdx>,
    /// The size of the continuous known stack.
    pub stack_size: usize,
    pub frame_state: FrameState,
    pub gas_wallet: GasWallet,
    /// The generation of the currently created variables.
    /// Defined by the number of continuous-stack invalidations that occurred.
    pub generation: usize,
}
impl Environment {
    pub fn new(gas_wallet: GasWallet, ap_tracking_base: StatementIdx) -> Self {
        let ap_tracking = ApChange::Known(0);
        Self {
            ap_tracking,
            ap_tracking_base: Some(ap_tracking_base),
            stack_size: 0,
            frame_state: FrameState::Allocating { allocated: 0, last_ap_tracking: ap_tracking },
            gas_wallet,
            generation: 0,
        }
    }
}

// Validates that the environments match and returns appropriate error if not.
pub fn validate_environment_equality(
    a: &Environment,
    b: &Environment,
) -> Result<(), EnvironmentError> {
    if a.ap_tracking_base != b.ap_tracking_base {
        Err(EnvironmentError::InconsistentApTrackingBase)
    } else if a.ap_tracking != b.ap_tracking {
        Err(EnvironmentError::InconsistentApTracking)
    } else if a.frame_state != b.frame_state {
        Err(EnvironmentError::InconsistentFrameState)
    } else if a.gas_wallet != b.gas_wallet {
        Err(EnvironmentError::InconsistentGasWallet)
    } else {
        Ok(())
    }
}

// Validates that the state at the end of a function is valid.
pub fn validate_final_environment(env: &Environment) -> Result<(), EnvironmentError> {
    validate_final_frame_state(&env.frame_state).map_err(EnvironmentError::InvalidFinalFrameState)
}
