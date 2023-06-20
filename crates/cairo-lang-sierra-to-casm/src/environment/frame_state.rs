use thiserror::Error;

use super::{ApTracking, ApTrackingBase};

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
    /// Before any locals were allocated (`alloc_local` wasn't called).
    BeforeAllocation,
    /// `finalize_locals` wasn't called yet.
    /// `allocated` is the number of stack slot that were already allocated for local variables.
    /// `locals_start_ap_offset` is the ap change between the first `alloc_local` and the beginning
    /// of the function. It is used to validate that there were no ap changes between the
    /// allocations and the call to `handle_finalize_locals`.
    Allocating { allocated: usize, locals_start_ap_offset: usize },
    /// finalize_locals was called and the frame has been finalized.
    Finalized { allocated: usize },
}

/// Returns the number of slots that were allocated for locals and the new frame state.
pub fn handle_finalize_locals(
    frame_state: FrameState,
    ap_tracking: ApTracking,
) -> Result<(usize, FrameState), FrameStateError> {
    match frame_state {
        FrameState::BeforeAllocation => {
            // TODO(ilya, 10/10/2022): Do we want to support allocating 0 locals?
            if matches!(
                ap_tracking,
                ApTracking::Enabled { ap_change: _, base: ApTrackingBase::FunctionStart }
            ) {
                Ok((0, FrameState::Finalized { allocated: 0 }))
            } else {
                Err(FrameStateError::InvalidFinalizeLocals(frame_state))
            }
        }
        FrameState::Allocating { allocated, locals_start_ap_offset } => {
            if matches!(
                ap_tracking,
                ApTracking::Enabled { ap_change, base: ApTrackingBase::FunctionStart }
                if ap_change == locals_start_ap_offset
            ) {
                Ok((allocated, FrameState::Finalized { allocated }))
            } else {
                Err(FrameStateError::InvalidFinalizeLocals(frame_state))
            }
        }
        FrameState::Finalized { .. } => Err(FrameStateError::InvalidFinalizeLocals(frame_state)),
    }
}

/// Returns the offset of the newly allocated variable and the new frame state.
pub fn handle_alloc_local(
    frame_state: FrameState,
    ap_tracking: ApTracking,
    allocation_size: usize,
) -> Result<(usize, FrameState), FrameStateError> {
    match frame_state {
        FrameState::BeforeAllocation => {
            if let ApTracking::Enabled { ap_change, base: ApTrackingBase::FunctionStart } =
                ap_tracking
            {
                Ok((
                    ap_change,
                    FrameState::Allocating {
                        allocated: allocation_size,
                        locals_start_ap_offset: ap_change,
                    },
                ))
            } else {
                Err(FrameStateError::InvalidAllocLocal(frame_state))
            }
        }
        FrameState::Allocating { allocated, locals_start_ap_offset } => {
            if matches!(
                ap_tracking,
                ApTracking::Enabled { ap_change, base: ApTrackingBase::FunctionStart }
                if ap_change == locals_start_ap_offset
            ) {
                Ok((
                    locals_start_ap_offset + allocated,
                    FrameState::Allocating {
                        allocated: allocated + allocation_size,
                        locals_start_ap_offset,
                    },
                ))
            } else {
                Err(FrameStateError::InvalidAllocLocal(frame_state))
            }
        }
        FrameState::Finalized { .. } => Err(FrameStateError::InvalidAllocLocal(frame_state)),
    }
}

/// Validates that the state at the end of a function is valid.
pub fn validate_final_frame_state(frame_state: &FrameState) -> Result<(), FrameStateError> {
    if matches!(frame_state, FrameState::Allocating { .. }) {
        Err(FrameStateError::FinalizeLocalsMissing(frame_state.clone()))
    } else {
        Ok(())
    }
}
