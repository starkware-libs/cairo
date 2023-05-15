use thiserror::Error;

use super::ApTracking;

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
    Finalized { allocated: usize },
    /// `finalize_locals` wasn't called yet.
    /// `allocated` is the number of stack slot that were already allocated for local variables.
    /// `locals_start_ap_offset` is the ap-tracking at the first `alloc_local`. It is used to
    /// validate that there were no ap changes between the allocations and the call to
    /// `handle_finalize_locals`.
    Allocating { allocated: usize, locals_start_ap_offset: usize },
}

/// Checks that there were no ap changes between allocations of locals.
fn is_valid_transition(
    allocated: usize,
    current_ap_change: usize,
    locals_start_ap_offset: usize,
) -> bool {
    if allocated == 0 {
        // If no locals were allocated the transition is always valid.
        return true;
    }

    // ap changes are forbidden between the allocations of locals and the finalization, so the
    // transition is valid if and only if the ap_tracking didn't change.
    current_ap_change == locals_start_ap_offset
}

/// Returns the number of slots that were allocated for locals and the new frame state.
pub fn handle_finalize_locals(
    frame_state: FrameState,
    ap_tracking: ApTracking,
) -> Result<(usize, FrameState), FrameStateError> {
    match frame_state {
        FrameState::Finalized { .. } => Err(FrameStateError::InvalidFinalizeLocals(frame_state)),
        FrameState::Allocating { allocated, locals_start_ap_offset } => {
            match ap_tracking {
                // TODO(ilya, 10/10/2022): Do we want to support allocating 0 locals?
                // TODO(lior, 15/05/2023): Base must point to the beginning of the function.
                ApTracking::Enabled { ap_change, base: _ }
                    if is_valid_transition(allocated, ap_change, locals_start_ap_offset) =>
                {
                    Ok((allocated, FrameState::Finalized { allocated }))
                }
                _ => Err(FrameStateError::InvalidFinalizeLocals(frame_state)),
            }
        }
    }
}

/// Returns the offset of the newly allocated variable and the new frame state.
pub fn handle_alloc_local(
    frame_state: FrameState,
    ap_tracking: ApTracking,
    allocation_size: usize,
) -> Result<(usize, FrameState), FrameStateError> {
    match frame_state {
        FrameState::Finalized { .. } => Err(FrameStateError::InvalidAllocLocal(frame_state)),
        FrameState::Allocating { allocated, locals_start_ap_offset } => match ap_tracking {
            // TODO(lior): base must point to the beginning of the function.
            ApTracking::Enabled { ap_change, base: _ }
                if is_valid_transition(allocated, ap_change, locals_start_ap_offset) =>
            {
                Ok((
                    ap_change + allocated,
                    FrameState::Allocating {
                        allocated: allocated + allocation_size,
                        locals_start_ap_offset: ap_change,
                    },
                ))
            }
            _ => Err(FrameStateError::InvalidAllocLocal(frame_state)),
        },
    }
}

/// Validates that the state at the end of a function is valid.
pub fn validate_final_frame_state(frame_state: &FrameState) -> Result<(), FrameStateError> {
    match frame_state {
        FrameState::Allocating { allocated, .. } if *allocated > 0 => {
            Err(FrameStateError::FinalizeLocalsMissing(frame_state.clone()))
        }
        _ => Ok(()),
    }
}
