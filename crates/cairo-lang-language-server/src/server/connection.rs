// +-----------------------------------------------------+
// | Code adopted from:                                  |
// | Repository: https://github.com/astral-sh/ruff       |
// | File: `crates/ruff_server/src/server/connection.rs` |
// | Commit: 46a457318d8d259376a2b458b3f814b9b795fe69    |
// +-----------------------------------------------------+

use std::sync::{Arc, Weak};

use anyhow::{Result, bail};
use lsp_server::{
    Connection as LSPConnection, IoThreads, Message, Notification, Request, RequestId, Response,
};
use lsp_types::notification::{Exit, Notification as NotificationTrait};
use lsp_types::request::{Request as RequestTrait, Shutdown};
use lsp_types::{InitializeResult, ServerCapabilities};
use tracing::{error, info};

type ConnectionSender = crossbeam::channel::Sender<Message>;
type ConnectionReceiver = crossbeam::channel::Receiver<Message>;

/// A builder for `Connection` that handles LSP initialization.
pub struct ConnectionInitializer {
    connection: LSPConnection,
    /// None in tests, Some(_) otherwise
    threads: Option<IoThreads>,
}

/// Handles inbound and outbound messages with the client.
pub struct Connection {
    sender: Arc<ConnectionSender>,
    receiver: ConnectionReceiver,
    /// None in tests, Some(_) otherwise
    threads: Option<IoThreads>,
}

impl ConnectionInitializer {
    /// Create a new LSP server connection over stdin/stdout.
    pub fn stdio() -> Self {
        let (connection, threads) = LSPConnection::stdio();
        Self { connection, threads: Some(threads) }
    }

    #[cfg(feature = "testing")]
    /// Create a new LSP server connection in memory.
    pub fn memory() -> (Self, LSPConnection) {
        let (server, client) = LSPConnection::memory();
        (Self { connection: server, threads: None }, client)
    }

    /// Starts the initialization process with the client by listening for an initialization
    /// request. Returns a request ID that should be passed into `initialize_finish` later,
    /// along with the initialization parameters that were provided.
    pub fn initialize_start(&self) -> Result<(RequestId, lsp_types::InitializeParams)> {
        let (id, params) = self.connection.initialize_start()?;
        Ok((id, serde_json::from_value(params)?))
    }

    /// Finishes the initialization process with the client,
    /// returning an initialized `Connection`.
    pub fn initialize_finish(
        self,
        id: RequestId,
        server_capabilities: ServerCapabilities,
    ) -> Result<Connection> {
        let initialize_result =
            InitializeResult { capabilities: server_capabilities, server_info: None };
        self.connection.initialize_finish(id, serde_json::to_value(initialize_result).unwrap())?;
        let Self { connection: LSPConnection { sender, receiver }, threads } = self;
        Ok(Connection { sender: Arc::new(sender), receiver, threads })
    }
}

impl Connection {
    /// Make a new `ClientSender` for sending messages to the client.
    pub fn make_sender(&self) -> ClientSender {
        ClientSender { weak_sender: Arc::downgrade(&self.sender) }
    }

    /// An iterator over incoming messages from the client.
    pub fn incoming(&self) -> crossbeam::channel::Iter<'_, Message> {
        self.receiver.iter()
    }

    /// Check and respond to any incoming shutdown requests; returns `true` if the server should be
    /// shutdown.
    pub fn handle_shutdown(&self, message: &Message) -> Result<bool> {
        match message {
            Message::Request(Request { id, method, .. }) if method == Shutdown::METHOD => {
                self.sender.send(Response::new_ok(id.clone(), ()).into())?;
                info!("shutdown request received, waiting for an exit notification...");
                match self.receiver.recv_timeout(std::time::Duration::from_secs(30))? {
                    Message::Notification(Notification { method, .. })
                        if method == Exit::METHOD =>
                    {
                        info!("exit notification received, server shutting down...");
                        Ok(true)
                    }
                    message => bail!(
                        "server received unexpected message {message:?} while waiting for exit \
                         notification"
                    ),
                }
            }
            Message::Notification(Notification { method, .. }) if method == Exit::METHOD => {
                error!(
                    "server received an exit notification before a shutdown request was sent, \
                     exiting..."
                );
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    /// Join the I/O threads that underpin this connection.
    /// This is guaranteed to be nearly immediate since
    /// we close the only active channels to these threads prior
    /// to joining them.
    pub fn close(self) -> Result<()> {
        drop(
            Arc::into_inner(self.sender)
                .expect("the client sender shouldn't have more than one strong reference"),
        );
        drop(self.receiver);

        if let Some(threads) = self.threads {
            threads.join()?;
        }
        Ok(())
    }
}

/// A weak reference to an underlying sender channel, used for communication with the client.
/// If the `Connection` that created this `ClientSender` is dropped, any `send` calls will throw
/// an error.
#[derive(Clone, Debug)]
pub struct ClientSender {
    weak_sender: Weak<ConnectionSender>,
}

impl ClientSender {
    pub fn send(&self, msg: Message) -> Result<()> {
        let Some(sender) = self.weak_sender.upgrade() else {
            bail!("the connection with the client has been closed");
        };

        Ok(sender.send(msg)?)
    }
}
