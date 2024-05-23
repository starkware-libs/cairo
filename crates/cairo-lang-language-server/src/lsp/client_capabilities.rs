use tower_lsp::lsp_types::ClientCapabilities;

macro_rules! try_or_default {
    ($expr:expr) => {
        || -> Option<_> { Some($expr) }().unwrap_or_default()
    };
}

/// Extension methods for the [`ClientCapabilities`] struct.
pub trait ClientCapabilitiesExt {
    /// The client supports dynamic registration for the `workspace/didChangeWatchedFiles`
    /// notification.
    fn did_change_watched_files_dynamic_registration(&self) -> bool;

    /// The client supports `workspace/configuration` requests.
    fn workspace_configuration_support(&self) -> bool;
}

impl ClientCapabilitiesExt for ClientCapabilities {
    fn did_change_watched_files_dynamic_registration(&self) -> bool {
        try_or_default!(
            self.workspace.as_ref()?.did_change_watched_files.as_ref()?.dynamic_registration?
        )
    }

    fn workspace_configuration_support(&self) -> bool {
        try_or_default! {
            self.workspace.as_ref()?.configuration?
        }
    }
}
