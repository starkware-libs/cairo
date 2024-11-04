use std::any::Any;

use salsa::Cancelled;

/// Checks if the panic was caused by Salsa cancellation.
///
/// ## Using with `catch_unwind`
/// [`std::panic::catch_unwind`] returns panic payload as a `Box<dyn Any + Send>`.
/// Always pass `is_cancelled(payload.as_ref())` instead of `is_cancelled(&payload)` when operating
/// with these results, because the second expression will lose type information and this function
/// will always return `false`.
pub fn is_cancelled(err: &(dyn Any + Send)) -> bool {
    // Salsa is broken and sometimes when cancelled throws regular assert instead of `Cancelled`.
    err.is::<Cancelled>()
        || err.downcast_ref::<&str>().is_some_and(|msg| {
            msg.contains("assertion failed: old_memo.revisions.changed_at <= revisions.changed_at")
        })
}

/// Attempts to convert Salsa cancellation to meaningful [`anyhow::Error`].
///
/// The `context` parameter is used to provide additional context to the error.
pub fn cancelled_anyhow(
    err: Box<dyn Any + Send>,
    context: &'static str,
) -> Result<anyhow::Error, Box<dyn Any + Send>> {
    match err.downcast::<Cancelled>() {
        Ok(err) => Ok(anyhow::Error::new(err).context(context)),
        Err(err) => match err.downcast::<&str>() {
            Ok(msg)
                if msg.contains(
                    "assertion failed: old_memo.revisions.changed_at <= revisions.changed_at",
                ) =>
            {
                Ok(anyhow::Error::msg(msg).context(context))
            }
            Ok(msg) => Err(Box::new(msg)),
            Err(err) => Err(err),
        },
    }
}
