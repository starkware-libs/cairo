use std::any::Any;

use salsa::Cancelled;

/// Checks if the panic was caused by Salsa cancellation.
///
/// This function takes an explicit reference to `Box<dyn Any + Send>` (instead of
/// `&(dyn Any + Send)` for example), to facilitate using it while handling `catch_unwind` results.
/// This is because downcasting works differently for `&(dyn Any + Send)`, and this function would
/// not work in that use case.
pub fn is_cancelled(err: &Box<dyn Any + Send>) -> bool {
    // Salsa is broken and sometimes when cancelled throws regular assert instead of `Cancelled`.
    err.is::<Cancelled>()
        || err.downcast_ref::<&str>().is_some_and(|msg| {
            msg.contains("assertion failed: old_memo.revisions.changed_at <= revisions.changed_at")
        })
}
