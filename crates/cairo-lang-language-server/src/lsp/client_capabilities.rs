use tower_lsp::lsp_types::ClientCapabilities;

macro_rules! try_or_default {
    ($expr:expr) => {
        || -> Option<_> { Some($expr) }().unwrap_or_default()
    };
}

/// Extension methods for the [`ClientCapabilities`] struct.
pub trait ClientCapabilitiesExt {
    /// The client supports `workspace/configuration` requests.
    fn workspace_configuration_support(&self) -> bool;
}

impl ClientCapabilitiesExt for ClientCapabilities {
    fn workspace_configuration_support(&self) -> bool {
        try_or_default! {
            self.workspace.as_ref()?.configuration?
        }
    }
}
