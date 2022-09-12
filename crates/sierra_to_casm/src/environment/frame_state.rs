use casm::ap_change::ApChange;
use thiserror::Error;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum FrameStateError {
    #[error("InvalidTransition")]
    InvalidTransition,
    #[error("alloc_locals is not allowed at this point.")]
    InvalidAllocLocals(FrameState),
}

// The frame state of the current function.
// This state keeps track of how many locals have been allocated and whether the
// frame has been finalized.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum FrameState {
    // alloc_locals was called and the frame has been finalized.
    Finalized,
    // 'allocated' felts have been allocated for local variables.
    Allocating { allocated: i16, last_ap_tracking: ApChange },
}

// Returns the number of slots to allocates for locals and the new frame state.
pub fn handle_alloc_locals(
    frame_state: FrameState,
    ap_tracking: ApChange,
) -> Result<(i16, FrameState), FrameStateError> {
    match frame_state {
        FrameState::Finalized => Err(FrameStateError::InvalidAllocLocals(frame_state)),
        FrameState::Allocating { allocated, last_ap_tracking } => {
            if last_ap_tracking != ap_tracking
                && (ap_tracking == ApChange::Unknown || allocated > 0)
            {
                // If the new ap change is unknown, or the ap tracking changed
                // after we allocated locals, then it is too late to call alloc_locals.
                Err(FrameStateError::InvalidAllocLocals(frame_state))
            } else {
                Ok((allocated, FrameState::Finalized))
            }
        }
    }
}
