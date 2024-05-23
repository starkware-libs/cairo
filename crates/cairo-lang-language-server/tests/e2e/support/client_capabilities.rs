use tower_lsp::lsp_types;

/// Produces minimal client capabilities provided by the mock language client.
///
/// Tests will most often need to extend these with test-specific additions using the
/// `client_capabilities` property of the [`super::sandbox!`] macro.
pub fn base() -> lsp_types::ClientCapabilities {
    lsp_types::ClientCapabilities {
        workspace: Some(lsp_types::WorkspaceClientCapabilities {
            configuration: Some(false),
            ..Default::default()
        }),
        window: Some(lsp_types::WindowClientCapabilities {
            work_done_progress: Some(false),
            ..Default::default()
        }),
        ..Default::default()
    }
}
