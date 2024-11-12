use std::sync::Arc;

use super::ProcMacroClient;

#[derive(Debug, Default, Clone)]
pub enum ClientStatus {
    #[default]
    Uninitialized,
    Initializing(Arc<ProcMacroClient>),
    Ready(Arc<ProcMacroClient>),
    /// Failed to initialize multiple times. No more actions will be taken.
    FailedToInitialize,
}

impl ClientStatus {
    pub fn is_uninitialized(&self) -> bool {
        matches!(self, Self::Uninitialized)
    }
}
