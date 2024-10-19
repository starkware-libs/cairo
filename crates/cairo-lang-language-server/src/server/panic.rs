use std::any::Any;
use std::panic::{AssertUnwindSafe, catch_unwind};

use anyhow::anyhow;
use lsp_server::ErrorCode;
use salsa::Cancelled;
use tracing::{debug, error};

use crate::lsp::result::{LSPError, LSPResult};

/// Invokes a closure, capturing the cause of an unwinding panic if one occurs and builds
/// [`LSPResult`] out of it.
pub fn ls_catch_unwind<T>(f: impl FnOnce() -> T) -> LSPResult<T> {
    catch_unwind(AssertUnwindSafe(f)).map_err(|err| {
        if is_cancelled(&err) {
            debug!("LSP worker thread was cancelled");
            LSPError::new(anyhow!("LSP worker thread was cancelled"), ErrorCode::ServerCancelled)
        } else {
            error!("caught panic in LSP worker thread");
            LSPError::new(anyhow!("caught panic in LSP worker thread"), ErrorCode::InternalError)
        }
    })
}

/// Checks if the panic was caused by Salsa cancellation.
fn is_cancelled(err: &(dyn Any + Send)) -> bool {
    // Salsa is broken and sometimes when cancelled throws regular assert instead of `Cancelled`.
    err.is::<Cancelled>()
        || err.downcast_ref::<&str>().is_some_and(|msg| {
            msg.contains("assertion failed: old_memo.revisions.changed_at <= revisions.changed_at")
        })
}
