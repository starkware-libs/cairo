use std::panic::{catch_unwind, AssertUnwindSafe};

use anyhow::{anyhow, Result};
use lsp_server::ErrorCode;
use salsa::Cancelled;
use tracing::{debug, error};

use crate::lsp::result::{LSPError, LSPResult};

#[derive(Debug, Clone, Copy)]
pub enum UnwindErrorKind {
    Canceled,
    Other,
}

/// Catches panics and returns Err.
pub fn catch_cancellation<T>(f: impl FnOnce() -> T) -> Result<T, UnwindErrorKind> {
    catch_unwind(AssertUnwindSafe(f)).map_err(|err| {
        // Salsa is broken and sometimes when cancelled throws regular assert instead of
        // [`Cancelled`]. Catch this case too.
        if err.is::<Cancelled>()
            || err.downcast_ref::<&str>().is_some_and(|msg| {
                msg.contains(
                    "assertion failed: old_memo.revisions.changed_at <= revisions.changed_at",
                )
            })
        {
            UnwindErrorKind::Canceled
        } else {
            UnwindErrorKind::Other
        }
    })
}

pub fn ls_catch_panics<T>(f: impl FnOnce() -> T) -> LSPResult<T> {
    catch_cancellation(f).map_err(|kind| match kind {
        UnwindErrorKind::Canceled => {
            debug!("LSP worker thread was cancelled");
            LSPError::new(anyhow!("LSP worker thread was cancelled"), ErrorCode::ServerCancelled)
        }
        UnwindErrorKind::Other => {
            error!("caught panic in LSP worker thread");
            LSPError::new(anyhow!("caught panic in LSP worker thread"), ErrorCode::InternalError)
        }
    })
}
