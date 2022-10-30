use casm::ap_change::ApChange;
use thiserror::Error;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum FrameStateError {
    #[error("InvalidTransition")]
    InvalidTransition,
    #[error("alloc_local is not allowed at this point.")]
    InvalidAllocLocal(FrameState),
    #[error("finalize_locals is not allowed at this point.")]
    InvalidFinalizeLocals(FrameState),
    #[error("locals were allocated but finalize_locals was not called.")]
    FinalizeLocalsMissing(FrameState),
}

/// The frame state of the current function.
/// This state keeps track of how many locals have been allocated and whether the
/// frame has been finalized.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum FrameState {
    /// finalize_locals was called and the frame has been finalized.
    Finalized,
    /// finalize_locals wasn't called yet.
    /// 'allocated' is the number of stack slot that were allocated for a local variables.
    /// last_ap_tracking is the last ap_tracking that was seen by this module.
    Allocating { allocated: i16, last_ap_tracking: ApChange },
}

// Returns the number of slots to allocates for locals and the new frame state.
pub fn handle_finalize_locals(
    frame_state: FrameState,
    ap_tracking: ApChange,
) -> Result<(i16, FrameState), FrameStateError> {
    match frame_state {
        FrameState::Finalized => Err(FrameStateError::InvalidFinalizeLocals(frame_state)),
        FrameState::Allocating { allocated, last_ap_tracking } => {
            match ap_tracking {
                // TODO(ilya, 10/10/2022): Do we want to support allocating 0 locals?
                ApChange::Known(_) if allocated == 0 || (ap_tracking == last_ap_tracking) => {
                    Ok((allocated, FrameState::Finalized))
                }
                _ => Err(FrameStateError::InvalidFinalizeLocals(frame_state)),
            }
        }
    }
}

// Returns the offset of the newly allocated variable and the new frame state.
pub fn handle_alloc_local(
    frame_state: FrameState,
    ap_tracking: ApChange,
    allocation_size: i16,
) -> Result<(i16, FrameState), FrameStateError> {
    match frame_state {
        FrameState::Finalized => Err(FrameStateError::InvalidAllocLocal(frame_state)),
        FrameState::Allocating { allocated, last_ap_tracking } => match ap_tracking {
            // Ap change is forbidden between allocations of locals, so the allocation is
            // valid if this is the first local that is being allocated or if the
            // ap_tracking didn't change.
            ApChange::Known(offset) if allocated == 0 || (ap_tracking == last_ap_tracking) => Ok((
                offset + allocated,
                FrameState::Allocating {
                    allocated: allocated + allocation_size,
                    last_ap_tracking: ap_tracking,
                },
            )),
            _ => Err(FrameStateError::InvalidAllocLocal(frame_state)),
        },
    }
}

// Validates that the state at the end of a function is valid.
pub fn validate_final_frame_state(frame_state: &FrameState) -> Result<(), FrameStateError> {
    match frame_state {
        FrameState::Allocating { allocated, .. } if *allocated > 0 => {
            Err(FrameStateError::FinalizeLocalsMissing(frame_state.clone()))
        }
        _ => Ok(()),
    }
}
