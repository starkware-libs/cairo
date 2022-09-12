use casm::ap_change::ApChange;
use thiserror::Error;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum FrameStateError {
    #[error("InvalidTransition")]
    InvalidTransition,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum FrameState {
    // The frame has been finalized ('max' is fixed till the end of this function).
    // A finalization may occur because of ap_tracking was lost or because alloc_locals was
    // called. 'used' stack slots have been used for temps and locals, and up to
    // 'max' slots can be used.
    Finalized { used: usize, max: usize },
    // The frame wasn't finalized yet.
    // 'used' stack slots where used by temps and store_local is not allowed yet.
    Allocating { used: usize },
}

pub fn update_frame_state(
    frame_state: FrameState,
    ap_change: ApChange,
) -> Result<FrameState, FrameStateError> {
    Ok(match (frame_state, ap_change) {
        (FrameState::Finalized { used, max }, _) => FrameState::Finalized { used, max },
        (FrameState::Allocating { used }, ApChange::Unknown) => {
            FrameState::Finalized { used, max: used }
        }
        (FrameState::Allocating { used }, ApChange::Known(ap_change)) => {
            FrameState::Allocating { used: used + ap_change as usize }
        }
    })
}
