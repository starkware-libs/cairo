use cairo_lang_casm::ap_change::{ApChange, ApChangeError};

use super::ApTracking;

/// Updates the function level ap_tracking based on ap_change.
pub fn update_ap_tracking(
    ap_tracking: ApTracking,
    ap_change: ApChange,
) -> Result<ApTracking, ApChangeError> {
    Ok(match (ap_tracking, ap_change) {
        (ApTracking::Enabled { ap_change: current }, ApChange::Known(change)) => {
            ApTracking::Enabled {
                ap_change: current.checked_add(change).ok_or(ApChangeError::OffsetOverflow)?,
            }
        }
        _ => ApTracking::Disabled,
    })
}
