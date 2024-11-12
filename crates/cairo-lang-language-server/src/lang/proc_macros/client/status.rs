use std::sync::{Arc, Mutex};

use scarb_proc_macro_server_types::methods::defined_macros::DefinedMacrosResponse;

use super::ProcMacroClient;

#[derive(Debug, Clone, Default)]
pub struct ProcMacroClientStatusChange(Arc<Mutex<Option<ClientStatusChange>>>);

impl ProcMacroClientStatusChange {
    pub fn update(&self, change: ClientStatusChange) {
        *self.0.lock().unwrap() = Some(change);
    }

    pub fn changed(&self) -> Option<ClientStatusChange> {
        self.0.lock().unwrap().take()
    }
}

#[derive(Debug, Default, Clone)]
pub enum ClientStatus {
    /// Disabled is default because it is initialized before receiving client config.
    #[default]
    Disabled,
    Initializing,
    Ready(Arc<ProcMacroClient>),
    /// Failed to initialize multiple times. No more actions will be taken.
    FailedToInitialize,
}

impl ClientStatus {
    pub fn ready(&self) -> Option<&ProcMacroClient> {
        if let Self::Ready(client) = self { Some(client) } else { None }
    }

    pub fn disabled(&self) -> bool {
        matches!(self, Self::Disabled)
    }
}

/// Edges of [`ClientStatus`].
/// Represents possible state transitions.
#[derive(Debug)]
pub enum ClientStatusChange {
    Ready(DefinedMacrosResponse, Arc<ProcMacroClient>),
    // We can retry.
    Failed,
    // Even if we retry it probably won't work anyway.
    FatalFailed,
}
