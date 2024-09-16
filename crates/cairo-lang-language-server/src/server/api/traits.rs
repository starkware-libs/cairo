//! A stateful LSP implementation that calls into the LS API.

use lsp_types::notification::Notification;
use lsp_types::request::Request;

use crate::server::api::LSPResult;
use crate::server::client::{Notifier, Requester};
use crate::state::{State, StateSnapshot};

/// A request handler that needs mutable access to the session.
/// This will block the main message receiver loop, meaning that no
/// incoming requests or notifications will be handled while `run` is
/// executing. Try to avoid doing any I/O or long-running computations.
pub trait SyncRequestHandler: Request {
    fn run(
        state: &mut State,
        notifier: Notifier,
        requester: &mut Requester<'_>,
        params: <Self as Request>::Params,
    ) -> LSPResult<<Self as Request>::Result>;
}

/// A request handler that can be run on a background thread.
pub trait BackgroundDocumentRequestHandler: Request {
    fn run_with_snapshot(
        snapshot: StateSnapshot,
        notifier: Notifier,
        params: <Self as Request>::Params,
    ) -> LSPResult<<Self as Request>::Result>;
}

/// A notification handler that needs mutable access to the session.
/// This will block the main message receiver loop, meaning that no
/// incoming requests or notifications will be handled while `run` is
/// executing. Try to avoid doing any I/O or long-running computations.
pub trait SyncNotificationHandler: Notification {
    fn run(
        state: &mut State,
        notifier: Notifier,
        requester: &mut Requester<'_>,
        params: <Self as Notification>::Params,
    ) -> LSPResult<()>;
}
