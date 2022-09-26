//! Cairo language server. Implements the LSP protocol over stdin/out.

mod semantic_highlighting;

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::Arc;

use db_utils::Upcast;
use defs::db::DefsGroup;
use defs::ids::FreeFunctionLongId;
use diagnostics::{DiagnosticEntry, Diagnostics};
use filesystem::db::{AsFilesGroupMut, FilesGroup, FilesGroupEx, PrivRawFileContentQuery};
use filesystem::ids::{FileId, FileLongId};
use filesystem::span::TextPosition;
use parser::db::ParserGroup;
use parser::formatter::{get_formatted_file, FormatterConfig};
use parser::ParserDiagnostic;
use project::ProjectConfig;
use semantic::db::SemanticGroup;
use semantic::items::free_function::SemanticExprLookup;
use semantic::test_utils::SemanticDatabaseForTesting;
use semantic::SemanticDiagnostic;
use semantic_highlighting::token_kind::SemanticTokenKind;
use semantic_highlighting::SemanticTokensTraverser;
use serde_json::Value;
use syntax::node::kind::SyntaxKind;
use syntax::node::{ast, TypedSyntaxNode};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use utils::ordered_hash_set::OrderedHashSet;

pub type RootDatabase = SemanticDatabaseForTesting;

const MAX_CRATE_DETECTION_DEPTH: usize = 20;

#[derive(Default, PartialEq, Eq)]
pub struct FileDiagnostics {
    pub parser: Diagnostics<ParserDiagnostic>,
    pub semantic: Diagnostics<SemanticDiagnostic>,
}
#[derive(Default)]
pub struct State {
    pub file_diagnostics: HashMap<FileId, FileDiagnostics>,
    pub open_files: HashSet<FileId>,
}
pub struct Backend {
    pub client: Client,
    // TODO(spapini): Remove this once we support ParallelDatabase.
    pub db_mutex: tokio::sync::Mutex<RootDatabase>,
    pub state_mutex: tokio::sync::Mutex<State>,
}
fn from_pos(pos: TextPosition) -> Position {
    Position { line: pos.line as u32, character: pos.col as u32 }
}
impl Backend {
    async fn db(&self) -> tokio::sync::MutexGuard<'_, RootDatabase> {
        self.db_mutex.lock().await
    }
    fn file(&self, db: &RootDatabase, uri: Url) -> FileId {
        let path = uri.to_file_path().expect("Only file URIs are supported.");
        FileId::new(db, path)
    }
    async fn refresh_diagnostics(&self) {
        let db = self.db().await;
        let mut state = self.state_mutex.lock().await;

        // Get all files. Try to go over open files first.
        let mut files_set: OrderedHashSet<_> = state.open_files.iter().copied().collect();
        for crate_id in db.crates() {
            for module_id in db.crate_modules(crate_id).iter() {
                if let Some(file_id) = db.module_file(*module_id) {
                    files_set.insert(file_id);
                }
            }
        }

        // Get all diagnostics.
        for file_id in files_set.iter().copied() {
            let uri = if let FileLongId::OnDisk(path) = db.lookup_intern_file(file_id) {
                Url::from_file_path(path).unwrap()
            } else {
                continue;
            };
            let new_file_diagnostics = FileDiagnostics {
                parser: db.file_syntax_diagnostics(file_id),
                semantic: db.file_semantic_diagnostics(file_id).unwrap_or_default(),
            };
            // Since we are using Arcs, this comparison should be efficient.
            if let Some(old_file_diagnostics) = state.file_diagnostics.get(&file_id) {
                if old_file_diagnostics == &new_file_diagnostics {
                    continue;
                }
            }
            let mut diags = Vec::new();
            self.get_diagnostics((*db).upcast(), &mut diags, &new_file_diagnostics.parser);
            self.get_diagnostics((*db).upcast(), &mut diags, &new_file_diagnostics.semantic);
            state.file_diagnostics.insert(file_id, new_file_diagnostics);

            self.client.publish_diagnostics(uri, diags, None).await
        }

        // Clear old diagnostics.
        let old_files: Vec<_> = state.file_diagnostics.keys().copied().collect();
        for file_id in old_files {
            if files_set.contains(&file_id) {
                continue;
            }
            state.file_diagnostics.remove(&file_id);
            let uri = if let FileLongId::OnDisk(path) = db.lookup_intern_file(file_id) {
                Url::from_file_path(path).unwrap()
            } else {
                continue;
            };
            self.client.publish_diagnostics(uri, Vec::new(), None).await;
        }
    }

    fn get_diagnostics<T: DiagnosticEntry>(
        &self,
        db: &T::DbType,
        diags: &mut Vec<Diagnostic>,
        diagnostics: &Diagnostics<T>,
    ) {
        for diagnostic in diagnostics.get_all() {
            let location = diagnostic.location(db);
            let message = diagnostic.format(db);
            let start = from_pos(
                location.span.start.position_in_file(db.upcast(), location.file_id).unwrap(),
            );
            let end = from_pos(
                location.span.start.position_in_file(db.upcast(), location.file_id).unwrap(),
            );
            diags.push(Diagnostic {
                range: Range { start, end },
                message,
                ..Diagnostic::default()
            });
        }
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
                document_formatting_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {}

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {}

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {}

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        let mut db = self.db().await;
        for change in params.changes {
            let file = self.file(&db, change.uri);
            PrivRawFileContentQuery.in_db_mut(db.as_files_group_mut()).invalidate(&file);
        }
    }

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(MessageType::ERROR, err).await,
        }

        Ok(None)
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let mut db = self.db().await;
        let uri = params.text_document.uri;
        let path = uri.path();
        detect_crate_for(&mut db, path);

        let file = self.file(&db, uri.clone());
        self.state_mutex.lock().await.open_files.insert(file);
        drop(db);
        self.refresh_diagnostics().await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let text =
            if let [TextDocumentContentChangeEvent { text, .. }] = &params.content_changes[..] {
                text
            } else {
                eprintln!("Unexpected format of document change.");
                return;
            };
        let mut db = self.db().await;
        let uri = params.text_document.uri;
        let file = self.file(&db, uri.clone());
        db.override_file_content(file, Some(Arc::new(text.into())));
        drop(db);
        self.refresh_diagnostics().await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let mut db = self.db().await;
        let file = self.file(&db, params.text_document.uri);
        PrivRawFileContentQuery.in_db_mut(db.as_files_group_mut()).invalidate(&file);
        db.override_file_content(file, None);
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let mut db = self.db().await;
        let file = self.file(&db, params.text_document.uri);
        self.state_mutex.lock().await.open_files.remove(&file);
        db.override_file_content(file, None);
        drop(db);
        self.refresh_diagnostics().await;
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
        let file_uri = params.text_document.uri;
        let file = self.file(&db, file_uri.clone());
        let syntax = if let Some(syntax) = db.file_syntax(file) {
            syntax
        } else {
            eprintln!("Semantic analysis failed. File '{file_uri}' does not exist.");
            return Ok(None);
        };

        let node = syntax.as_syntax_node();
        let mut data: Vec<SemanticToken> = Vec::new();
        SemanticTokensTraverser::default().find_semantic_tokens((*db).upcast(), &mut data, node);
        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens { result_id: None, data })))
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let db = self.db().await;
        let file_uri = params.text_document.uri;
        let file = self.file(&db, file_uri.clone());
        let syntax = if let Some(syntax) = db.file_syntax(file) {
            syntax
        } else {
            eprintln!("Formatting failed. File '{file_uri}' does not exist.");
            return Ok(None);
        };
        let new_text = get_formatted_file(
            (*db).upcast(),
            &syntax.as_syntax_node(),
            FormatterConfig::default(),
        );
        Ok(Some(vec![TextEdit {
            range: Range {
                start: Position { line: 0, character: 0 },
                end: Position { line: u32::MAX, character: 0 },
            },
            new_text,
        }]))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let db = self.db().await;
        let file_uri = params.text_document_position_params.text_document.uri;
        let file = self.file(&db, file_uri.clone());

        // Get syntax for file.
        let syntax = if let Some(syntax) = db.file_syntax(file) {
            syntax
        } else {
            eprintln!("Formatting failed. File '{file_uri}' does not exist.");
            return Ok(None);
        };

        // Get file summary.
        let file_summary = if let Some(summary) = db.file_summary(file) {
            summary
        } else {
            eprintln!("Hover failed. File '{file_uri}' does not exist.");
            return Ok(None);
        };

        // Find offset for position.
        let position = params.text_document_position_params.position;
        let line_offset =
            if let Some(offset) = file_summary.line_offsets.get(position.line as usize) {
                offset
            } else {
                eprintln!("Hover failed. Position out of bounds.");
                return Ok(None);
            };
        // TODO(spapini): Check that character is not larger than line_length.
        let mut node = syntax
            .as_syntax_node()
            .lookup_offset(&*db, line_offset.add(position.character as usize));

        // Find module.
        let modules: Vec<_> = db.file_modules(file).into_iter().flatten().collect();
        if modules.len() != 1 {
            eprintln!("Hover failed. Expected a single module for this file.");
            return Ok(None);
        }
        let module_id = modules[0];

        // Find containing expr.
        while !is_expr(node.kind(&*db)) {
            if let Some(parent) = node.parent() {
                node = parent;
            } else {
                eprintln!("Hover failed. Not inside an expression.");
                return Ok(None);
            }
        }
        let expr_node = ast::Expr::from_syntax_node(&*db, node.clone());

        // Find containing function.
        while node.kind(&*db) != SyntaxKind::ItemFreeFunction {
            if let Some(parent) = node.parent() {
                node = parent;
            } else {
                eprintln!("Hover failed. Not inside a function.");
                return Ok(None);
            }
        }
        let function_node = ast::ItemFreeFunction::from_syntax_node(&*db, node);
        let free_function_id =
            db.intern_free_function(FreeFunctionLongId(module_id, function_node.stable_ptr()));

        // Lookup semantic expression.
        let expr_id = if let Some(expr_id) =
            db.lookup_expr_by_ptr(free_function_id, expr_node.stable_ptr())
        {
            expr_id
        } else {
            eprintln!("Hover failed. Semantic model not found for expression.");
            return Ok(None);
        };

        // Semantic expression found.
        let semantic_expr = db.expr_semantic(free_function_id, expr_id);

        // Format the hover text.
        let text = format!("Type: {}", semantic_expr.ty().format(&*db));

        Ok(Some(Hover { contents: HoverContents::Scalar(MarkedString::String(text)), range: None }))
    }
}

fn is_expr(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::ExprBinary
            | SyntaxKind::ExprBlock
            | SyntaxKind::ExprParenthesized
            | SyntaxKind::ExprFunctionCall
            | SyntaxKind::ExprIf
            | SyntaxKind::ExprMatch
            | SyntaxKind::ExprMissing
            | SyntaxKind::ExprStructCtorCall
            | SyntaxKind::ExprUnary
            | SyntaxKind::ExprTuple
            | SyntaxKind::ExprPath
    )
}

/// Tries to detect the crate root the config that contains a cairo file, and add it to the system.
fn detect_crate_for(db: &mut tokio::sync::MutexGuard<'_, RootDatabase>, path: &str) {
    let mut path = PathBuf::from(path);
    for _ in 0..MAX_CRATE_DETECTION_DEPTH {
        path.pop();
        if let Ok(config) = ProjectConfig::from_directory(path.as_path()) {
            db.with_project_config(config);
        };
    }
}
