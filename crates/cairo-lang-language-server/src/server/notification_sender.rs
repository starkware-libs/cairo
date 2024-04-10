use std::fmt;

use tokio::runtime::Handle;
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::Client;

/// A minimal interface for sending notifications to the language client synchronously.
///
/// This object is small and cheap to clone, so it can be passed around freely.
#[derive(Clone)]
pub struct Notifier {
    client: Client,
}

impl Notifier {
    pub fn new(client: &Client) -> Self {
        Notifier { client: client.clone() }
    }

    /// Sends a custom notification to the client.
    pub fn send_notification<N>(&self, params: N::Params)
    where
        N: Notification,
    {
        Handle::current().block_on(self.client.send_notification::<N>(params))
    }
}

impl fmt::Debug for Notifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NotificationSender")
    }
}
