// +-----------------------------------------------------+
// | Traits adopted from:                                |
// | Repository: https://github.com/astral-sh/ruff       |
// | File: `crates/ruff_server/src/server/api/traits.rs` |
// | Commit: 46a457318d8d259376a2b458b3f814b9b795fe69    |
// +-----------------------------------------------------+

use cairo_lang_filesystem::db::{
    AsFilesGroupMut, FilesGroup, FilesGroupEx, PrivRawFileContentQuery,
};
use lsp_types::notification::{
    Cancel, DidChangeConfiguration, DidChangeTextDocument, DidChangeWatchedFiles,
    DidCloseTextDocument, DidOpenTextDocument, DidSaveTextDocument, Notification,
};
use lsp_types::request::{
    CodeActionRequest, Completion, ExecuteCommand, Formatting, GotoDefinition, HoverRequest,
    Request, SemanticTokensFullRequest,
};
use lsp_types::{
    CancelParams, CodeActionParams, CodeActionResponse, CompletionParams, CompletionResponse,
    DidChangeConfigurationParams, DidChangeTextDocumentParams, DidChangeWatchedFilesParams,
    DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams,
    DocumentFormattingParams, ExecuteCommandParams, GotoDefinitionParams, GotoDefinitionResponse,
    Hover, HoverParams, SemanticTokensParams, SemanticTokensResult, TextDocumentContentChangeEvent,
    TextDocumentPositionParams, TextEdit, Url,
};
use serde_json::Value;
use tracing::{error, warn};

use crate::lang::lsp::LsProtoGroup;
use crate::lsp::ext::{
    ExpandMacro, ProvideVirtualFile, ProvideVirtualFileRequest, ProvideVirtualFileResponse,
    ViewAnalyzedCrates,
};
use crate::lsp::result::{LSPError, LSPResult};
use crate::server::client::{Notifier, Requester};
use crate::server::commands::ServerCommands;
use crate::state::{State, StateSnapshot};
use crate::{Backend, ide, lang};

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

impl BackgroundDocumentRequestHandler for CodeActionRequest {
    #[tracing::instrument(level = "trace", skip_all)]
    fn run_with_snapshot(
        snapshot: StateSnapshot,
        _notifier: Notifier,
        params: CodeActionParams,
    ) -> Result<Option<CodeActionResponse>, LSPError> {
        Ok(ide::code_actions::code_actions(params, &snapshot.db))
    }
}

impl SyncRequestHandler for ExecuteCommand {
    #[tracing::instrument(level = "debug", skip_all, fields(command = params.command))]
    fn run(
        state: &mut State,
        notifier: Notifier,
        requester: &mut Requester<'_>,
        params: ExecuteCommandParams,
    ) -> LSPResult<Option<Value>> {
        let command = ServerCommands::try_from(params.command);

        if let Ok(cmd) = command {
            match cmd {
                ServerCommands::Reload => {
                    Backend::reload(state, &notifier, requester)?;
                }
            }
        }

        Ok(None)
    }
}

impl BackgroundDocumentRequestHandler for HoverRequest {
    #[tracing::instrument(level = "trace", skip_all)]
    fn run_with_snapshot(
        snapshot: StateSnapshot,
        _notifier: Notifier,
        params: HoverParams,
    ) -> LSPResult<Option<Hover>> {
        Ok(ide::hover::hover(params, &snapshot.db))
    }
}

impl BackgroundDocumentRequestHandler for Formatting {
    #[tracing::instrument(level = "trace", skip_all)]
    fn run_with_snapshot(
        snapshot: StateSnapshot,
        _notifier: Notifier,
        params: DocumentFormattingParams,
    ) -> LSPResult<Option<Vec<TextEdit>>> {
        Ok(ide::formatter::format(params, &snapshot.db))
    }
}

impl SyncNotificationHandler for Cancel {
    #[tracing::instrument(level = "trace", skip_all)]
    fn run(
        _state: &mut State,
        _notifier: Notifier,
        _requester: &mut Requester<'_>,
        _params: CancelParams,
    ) -> LSPResult<()> {
        Ok(())
    }
}

impl SyncNotificationHandler for DidChangeTextDocument {
    #[tracing::instrument(level = "debug", skip_all, fields(uri = %params.text_document.uri))]
    fn run(
        state: &mut State,
        notifier: Notifier,
        _requester: &mut Requester<'_>,
        params: DidChangeTextDocumentParams,
    ) -> LSPResult<()> {
        let text = if let Ok([TextDocumentContentChangeEvent { text, .. }]) =
            TryInto::<[_; 1]>::try_into(params.content_changes)
        {
            text
        } else {
            error!("unexpected format of document change");
            return Ok(());
        };

        if let Some(file) = state.db.file_for_url(&params.text_document.uri) {
            state.db.override_file_content(file, Some(text.into()));
            Backend::refresh_diagnostics(state, &notifier)?;
        };

        Ok(())
    }
}

impl SyncNotificationHandler for DidChangeConfiguration {
    #[tracing::instrument(level = "debug", skip_all)]
    fn run(
        state: &mut State,
        _notifier: Notifier,
        requester: &mut Requester<'_>,
        _params: DidChangeConfigurationParams,
    ) -> LSPResult<()> {
        Backend::reload_config(state, requester)
    }
}

impl SyncNotificationHandler for DidChangeWatchedFiles {
    #[tracing::instrument(level = "debug", skip_all)]
    fn run(
        state: &mut State,
        notifier: Notifier,
        requester: &mut Requester<'_>,
        params: DidChangeWatchedFilesParams,
    ) -> LSPResult<()> {
        // Invalidate changed cairo files.
        for change in &params.changes {
            if is_cairo_file_path(&change.uri) {
                let Some(file) = state.db.file_for_url(&change.uri) else { continue };
                PrivRawFileContentQuery.in_db_mut(state.db.as_files_group_mut()).invalidate(&file);
            }
        }

        // Reload workspace if a config file has changed.
        for change in params.changes {
            let changed_file_path = change.uri.to_file_path().unwrap_or_default();
            let changed_file_name = changed_file_path.file_name().unwrap_or_default();
            // TODO(pmagiera): react to Scarb.lock. Keep in mind Scarb does save Scarb.lock on each
            //  metadata call, so it is easy to fall in a loop here.
            if ["Scarb.toml", "cairo_project.toml"].map(Some).contains(&changed_file_name.to_str())
            {
                Backend::reload(state, &notifier, requester)?;
            }
        }

        Ok(())
    }
}

impl SyncNotificationHandler for DidCloseTextDocument {
    #[tracing::instrument(level = "debug", skip_all, fields(uri = %params.text_document.uri))]
    fn run(
        state: &mut State,
        notifier: Notifier,
        _requester: &mut Requester<'_>,
        params: DidCloseTextDocumentParams,
    ) -> LSPResult<()> {
        state.open_files.remove(&params.text_document.uri);
        if let Some(file) = state.db.file_for_url(&params.text_document.uri) {
            state.db.override_file_content(file, None);
            Backend::refresh_diagnostics(state, &notifier)?;
        }

        Ok(())
    }
}

impl SyncNotificationHandler for DidOpenTextDocument {
    #[tracing::instrument(level = "debug", skip_all, fields(uri = %params.text_document.uri))]
    fn run(
        state: &mut State,
        notifier: Notifier,
        _requester: &mut Requester<'_>,
        params: DidOpenTextDocumentParams,
    ) -> LSPResult<()> {
        let uri = params.text_document.uri;

        // Try to detect the crate for physical files.
        // The crate for virtual files is already known.
        if uri.scheme() == "file" {
            let Ok(path) = uri.to_file_path() else { return Ok(()) };

            Backend::detect_crate_for(
                &mut state.db,
                &state.scarb_toolchain,
                &state.config,
                &path,
                &notifier,
            );
        }

        if let Some(file_id) = state.db.file_for_url(&uri) {
            state.open_files.insert(uri);
            state.db.override_file_content(file_id, Some(params.text_document.text.into()));

            Backend::refresh_diagnostics(state, &notifier)?;
        }

        Ok(())
    }
}

impl SyncNotificationHandler for DidSaveTextDocument {
    #[tracing::instrument(level = "debug", skip_all, fields(uri = %params.text_document.uri))]
    fn run(
        state: &mut State,
        _notifier: Notifier,
        _requester: &mut Requester<'_>,
        params: DidSaveTextDocumentParams,
    ) -> LSPResult<()> {
        if let Some(file) = state.db.file_for_url(&params.text_document.uri) {
            PrivRawFileContentQuery.in_db_mut(state.db.as_files_group_mut()).invalidate(&file);
            state.db.override_file_content(file, None);
        }

        Ok(())
    }
}

impl BackgroundDocumentRequestHandler for GotoDefinition {
    #[tracing::instrument(level = "trace", skip_all)]
    fn run_with_snapshot(
        snapshot: StateSnapshot,
        _notifier: Notifier,
        params: GotoDefinitionParams,
    ) -> LSPResult<Option<GotoDefinitionResponse>> {
        Ok(ide::navigation::goto_definition::goto_definition(params, &snapshot.db))
    }
}

impl BackgroundDocumentRequestHandler for Completion {
    #[tracing::instrument(level = "trace", skip_all)]
    fn run_with_snapshot(
        snapshot: StateSnapshot,
        _notifier: Notifier,
        params: CompletionParams,
    ) -> LSPResult<Option<CompletionResponse>> {
        Ok(ide::completion::complete(params, &snapshot.db))
    }
}

impl BackgroundDocumentRequestHandler for SemanticTokensFullRequest {
    #[tracing::instrument(level = "trace", skip_all)]
    fn run_with_snapshot(
        snapshot: StateSnapshot,
        _notifier: Notifier,
        params: SemanticTokensParams,
    ) -> LSPResult<Option<SemanticTokensResult>> {
        Ok(ide::semantic_highlighting::semantic_highlight_full(params, &snapshot.db))
    }
}

impl BackgroundDocumentRequestHandler for ProvideVirtualFile {
    #[tracing::instrument(level = "trace", skip_all)]
    fn run_with_snapshot(
        snapshot: StateSnapshot,
        _notifier: Notifier,
        params: ProvideVirtualFileRequest,
    ) -> LSPResult<ProvideVirtualFileResponse> {
        let content = snapshot
            .db
            .file_for_url(&params.uri)
            .and_then(|file_id| snapshot.db.file_content(file_id))
            .map(|content| content.to_string());

        Ok(ProvideVirtualFileResponse { content })
    }
}

impl BackgroundDocumentRequestHandler for ViewAnalyzedCrates {
    #[tracing::instrument(level = "trace", skip_all)]
    fn run_with_snapshot(
        snapshot: StateSnapshot,
        _notifier: Notifier,
        _params: (),
    ) -> LSPResult<String> {
        Ok(lang::inspect::crates::inspect_analyzed_crates(&snapshot.db))
    }
}

impl BackgroundDocumentRequestHandler for ExpandMacro {
    #[tracing::instrument(level = "trace", skip_all)]
    fn run_with_snapshot(
        snapshot: StateSnapshot,
        _notifier: Notifier,
        params: TextDocumentPositionParams,
    ) -> LSPResult<Option<String>> {
        Ok(ide::macros::expand::expand_macro(&snapshot.db, &params))
    }
}

fn is_cairo_file_path(file_path: &Url) -> bool {
    file_path.path().ends_with(".cairo")
}
