//! Cairo language server. Implements the LSP protocol over stdin/out.

mod semantic_highlighting;

use filesystem::db::FilesGroup;
use filesystem::ids::{FileId, FileLongId};
use parser::db::ParserGroup;
use semantic::test_utils::SemanticDatabaseForTesting;
use semantic_highlighting::token_kind::SemanticTokenKind;
use semantic_highlighting::SemanticTokensTraverser;
use serde_json::Value;
use syntax::node::db::AsSyntaxGroup;
use syntax::node::TypedSyntaxNode;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

pub type RootDatabase = SemanticDatabaseForTesting;

pub struct Backend {
    pub client: Client,
    // TODO(spapini): Remove this once we support ParallelDatabase.
    pub db_mutex: tokio::sync::Mutex<RootDatabase>,
}
impl Backend {
    async fn db(&self) -> tokio::sync::MutexGuard<'_, RootDatabase> {
        self.db_mutex.lock().await
    }
    fn file(&self, db: &RootDatabase, document: TextDocumentIdentifier) -> FileId {
        let path = document.uri.to_file_path().expect("Only file URIs are supported.");
        db.intern_file(FileLongId::OnDisk(path))
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                }),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["dummy.do_something".to_string()],
                    work_done_progress_options: Default::default(),
                }),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensOptions {
                        legend: SemanticTokensLegend {
                            token_types: SemanticTokenKind::legend(),
                            token_modifiers: vec![],
                        },
                        full: Some(SemanticTokensFullOptions::Bool(true)),
                        ..SemanticTokensOptions::default()
                    }
                    .into(),
                ),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::INFO, "initialized!").await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        self.client.log_message(MessageType::INFO, "workspace folders changed!").await;
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.client.log_message(MessageType::INFO, "configuration changed!").await;
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client.log_message(MessageType::INFO, "watched files have changed!").await;
    }

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
        self.client.log_message(MessageType::INFO, "command executed!").await;

        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(MessageType::ERROR, err).await,
        }

        Ok(None)
    }

    async fn did_open(&self, _: DidOpenTextDocumentParams) {
        self.client.log_message(MessageType::INFO, "file opened!").await;
    }

    async fn did_change(&self, _: DidChangeTextDocumentParams) {
        self.client.log_message(MessageType::INFO, "file changed!").await;
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client.log_message(MessageType::INFO, "file saved!").await;
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client.log_message(MessageType::INFO, "file closed!").await;
    }

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
            CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
        ])))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let db = self.db().await;
        let file_uri = params.text_document.uri.clone();
        let file = self.file(&db, params.text_document);
        let syntax = if let Some(syntax) = db.file_syntax(file) {
            syntax
        } else {
            eprintln!("Semantic analysis failed. File '{file_uri}' does not exist.");
            return Ok(None);
        };

        let node = syntax.as_syntax_node();
        let mut data: Vec<SemanticToken> = Vec::new();
        SemanticTokensTraverser::default().find_semantic_tokens(
            db.as_syntax_group(),
            &mut data,
            node,
        );
        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens { result_id: None, data })))
    }
}
