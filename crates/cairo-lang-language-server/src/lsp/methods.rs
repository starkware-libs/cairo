use std::sync::Arc;

use cairo_lang_filesystem::db::{AsFilesGroupMut, FilesGroupEx, PrivRawFileContentQuery};
use serde_json::Value;
use tower_lsp::jsonrpc::Result as LSPResult;
use tower_lsp::lsp_types::{
    CodeActionParams, CodeActionResponse, CompletionParams, CompletionResponse,
    DidChangeConfigurationParams, DidChangeTextDocumentParams, DidChangeWatchedFilesParams,
    DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams,
    DocumentFormattingParams, ExecuteCommandParams, GotoDefinitionParams, GotoDefinitionResponse,
    Hover, HoverParams, InitializeParams, InitializeResult, InitializedParams, MessageType,
    SemanticTokensParams, SemanticTokensResult, TextDocumentContentChangeEvent, TextEdit, Url,
    WorkspaceEdit,
};
use tower_lsp::LanguageServer;
use tracing::{error, warn};

use crate::lang::lsp::LsProtoGroup;
use crate::lsp::capabilities::server::{
    collect_dynamic_registrations, collect_server_capabilities,
};
use crate::server::commands::ServerCommands;
use crate::state::Owned;
use crate::{ide, Backend};

/// TODO: Remove when we move to sync world.
/// This is macro because of lifetimes problems with `self`.
macro_rules! state_mut_async {
    ($state:ident, $this:ident, $($f:tt)+) => {
        async {
            let mut state = $this.state_mutex.lock().await;
            let $state = &mut *state;

            $($f)+
        }
    };
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    #[tracing::instrument(level = "debug", skip_all)]
    async fn initialize(&self, params: InitializeParams) -> LSPResult<InitializeResult> {
        let client_capabilities = Owned::new(Arc::new(params.capabilities));
        let client_capabilities_snapshot = client_capabilities.snapshot();
        self.with_state_mut(move |state| {
            state.client_capabilities = client_capabilities;
        })
        .await;

        Ok(InitializeResult {
            server_info: None,
            capabilities: collect_server_capabilities(&client_capabilities_snapshot),
        })
    }

    #[tracing::instrument(level = "debug", skip_all)]
    async fn initialized(&self, _: InitializedParams) {
        // Initialize the configuration.
        self.reload_config().await;

        // Dynamically register capabilities.
        let client_capabilities = self.state_snapshot().await.client_capabilities;

        let dynamic_registrations = collect_dynamic_registrations(&client_capabilities);
        if !dynamic_registrations.is_empty() {
            let result = self.client.register_capability(dynamic_registrations).await;
            if let Err(err) = result {
                warn!("failed to register dynamic capabilities: {err:#?}");
            }
        }
    }

    async fn shutdown(&self) -> LSPResult<()> {
        Ok(())
    }

    #[tracing::instrument(level = "debug", skip_all)]
    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.reload_config().await;
    }

    #[tracing::instrument(level = "debug", skip_all)]
    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        // Invalidate changed cairo files.
        self.with_state_mut(|state| {
            for change in &params.changes {
                if is_cairo_file_path(&change.uri) {
                    let Some(file) = state.db.file_for_url(&change.uri) else { continue };
                    PrivRawFileContentQuery
                        .in_db_mut(state.db.as_files_group_mut())
                        .invalidate(&file);
                }
            }
        })
        .await;

        // Reload workspace if a config file has changed.
        for change in params.changes {
            let changed_file_path = change.uri.to_file_path().unwrap_or_default();
            let changed_file_name = changed_file_path.file_name().unwrap_or_default();
            // TODO(pmagiera): react to Scarb.lock. Keep in mind Scarb does save Scarb.lock on each
            //  metadata call, so it is easy to fall in a loop here.
            if ["Scarb.toml", "cairo_project.toml"].map(Some).contains(&changed_file_name.to_str())
            {
                self.reload().await.ok();
            }
        }
    }

    #[tracing::instrument(level = "debug", skip_all, fields(command = params.command))]
    async fn execute_command(&self, params: ExecuteCommandParams) -> LSPResult<Option<Value>> {
        let command = ServerCommands::try_from(params.command);
        if let Ok(cmd) = command {
            match cmd {
                ServerCommands::Reload => {
                    self.reload().await?;
                }
            }
        }

        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(MessageType::ERROR, err).await,
        }

        Ok(None)
    }

    #[tracing::instrument(level = "debug", skip_all, fields(uri = %params.text_document.uri))]
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let refresh = state_mut_async! {state, self,
                let uri = params.text_document.uri;

                // Try to detect the crate for physical files.
                // The crate for virtual files is already known.
                if uri.scheme() == "file" {
                    let Ok(path) = uri.to_file_path() else { return false };
                    self.detect_crate_for(&mut state.db, &state.config, path).await;
                }

                let Some(file_id) = state.db.file_for_url(&uri) else { return false };
                state.open_files.insert(uri);
                state.db.override_file_content(file_id, Some(params.text_document.text.into()));

                true
        }
        .await;

        if refresh {
            self.refresh_diagnostics().await.ok();
        }
    }

    #[tracing::instrument(level = "debug", skip_all, fields(uri = %params.text_document.uri))]
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let text = if let Ok([TextDocumentContentChangeEvent { text, .. }]) =
            TryInto::<[_; 1]>::try_into(params.content_changes)
        {
            text
        } else {
            error!("unexpected format of document change");
            return;
        };
        let refresh = self
            .with_state_mut(|state| {
                let uri = params.text_document.uri;
                let Some(file) = state.db.file_for_url(&uri) else { return false };
                state.db.override_file_content(file, Some(text.into()));

                true
            })
            .await;

        if refresh {
            self.refresh_diagnostics().await.ok();
        }
    }

    #[tracing::instrument(level = "debug", skip_all, fields(uri = %params.text_document.uri))]
    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.with_state_mut(|state| {
            let Some(file) = state.db.file_for_url(&params.text_document.uri) else { return };
            PrivRawFileContentQuery.in_db_mut(state.db.as_files_group_mut()).invalidate(&file);
            state.db.override_file_content(file, None);
        })
        .await;
    }

    #[tracing::instrument(level = "debug", skip_all, fields(uri = %params.text_document.uri))]
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let refresh = self
            .with_state_mut(|state| {
                state.open_files.remove(&params.text_document.uri);
                let Some(file) = state.db.file_for_url(&params.text_document.uri) else {
                    return false;
                };
                state.db.override_file_content(file, None);

                true
            })
            .await;

        if refresh {
            self.refresh_diagnostics().await.ok();
        }
    }

    #[tracing::instrument(level = "trace", skip_all)]
    async fn completion(&self, params: CompletionParams) -> LSPResult<Option<CompletionResponse>> {
        let db = self.db_snapshot().await;
        self.catch_panics(move || ide::completion::complete(params, &db)).await
    }

    #[tracing::instrument(level = "trace", skip_all)]
    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> LSPResult<Option<SemanticTokensResult>> {
        let db = self.db_snapshot().await;
        self.catch_panics(move || ide::semantic_highlighting::semantic_highlight_full(params, &db))
            .await
    }

    #[tracing::instrument(level = "trace", skip_all)]
    async fn formatting(
        &self,
        params: DocumentFormattingParams,
    ) -> LSPResult<Option<Vec<TextEdit>>> {
        let db = self.db_snapshot().await;
        self.catch_panics(move || ide::formatter::format(params, &db)).await
    }

    #[tracing::instrument(level = "trace", skip_all)]
    async fn hover(&self, params: HoverParams) -> LSPResult<Option<Hover>> {
        let db = self.db_snapshot().await;
        self.catch_panics(move || ide::hover::hover(params, &db)).await
    }

    #[tracing::instrument(level = "trace", skip_all)]
    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> LSPResult<Option<GotoDefinitionResponse>> {
        let db = self.db_snapshot().await;
        self.catch_panics(move || ide::navigation::goto_definition::goto_definition(params, &db))
            .await
    }

    #[tracing::instrument(level = "trace", skip_all)]
    async fn code_action(&self, params: CodeActionParams) -> LSPResult<Option<CodeActionResponse>> {
        let db = self.db_snapshot().await;
        self.catch_panics(move || ide::code_actions::code_actions(params, &db)).await
    }
}

fn is_cairo_file_path(file_path: &Url) -> bool {
    file_path.path().ends_with(".cairo")
}
