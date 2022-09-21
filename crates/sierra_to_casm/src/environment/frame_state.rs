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
}

// The frame state of the current function.
// This state keeps track of how many locals have been allocated and whether the
// frame has been finalized.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum FrameState {
    // finalize_locals was called and the frame has been finalized.
    Finalized,
    // 'allocated' felts have been allocated for local variables.
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
            let allocated = match ap_tracking {
                // TODO(ilya, 10/10/2022): Do we want to support allocating 0 locals?
                ApChange::Known(_) if allocated == 0 => Ok(0),
                ApChange::Known(offset) if ap_tracking == last_ap_tracking => {
                    Ok(allocated - offset)
                }
                _ => Err(FrameStateError::InvalidFinalizeLocals(frame_state)),
            }?;
            Ok((allocated, FrameState::Finalized))
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

        FrameState::Allocating { allocated, last_ap_tracking } => {
            let allocated = match ap_tracking {
                ApChange::Known(offset) if allocated == 0 => Ok(offset),
                ApChange::Known(_) if ap_tracking == last_ap_tracking => Ok(allocated),
                _ => Err(FrameStateError::InvalidAllocLocal(frame_state)),
            }?;

            Ok((
                allocated,
                FrameState::Allocating {
                    allocated: allocated + allocation_size,
                    last_ap_tracking: ap_tracking,
                },
            ))
        }
    }
}
