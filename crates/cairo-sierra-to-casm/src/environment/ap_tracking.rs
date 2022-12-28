use casm::ap_change::{ApChange, ApChangeError};

/// Updates the function level ap_tracking based on ap_change.
pub fn update_ap_tracking(
    ap_tracking: ApChange,
    ap_change: ApChange,
) -> Result<ApChange, ApChangeError> {
    Ok(match (ap_tracking, ap_change) {
        (ApChange::Known(current), ApChange::Known(change)) => {
            ApChange::Known(current.checked_add(change).ok_or(ApChangeError::OffsetOverflow)?)
        }
        _ => ApChange::Unknown,
    })
}
