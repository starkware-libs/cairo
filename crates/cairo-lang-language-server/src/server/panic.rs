use std::any::Any;

use salsa::Cancelled;

/// Checks if the panic was caused by Salsa cancellation.
pub fn is_cancelled(err: &(dyn Any + Send)) -> bool {
    // Salsa is broken and sometimes when cancelled throws regular assert instead of `Cancelled`.
    err.is::<Cancelled>()
        || err.downcast_ref::<&str>().is_some_and(|msg| {
            msg.contains("assertion failed: old_memo.revisions.changed_at <= revisions.changed_at")
        })
}
