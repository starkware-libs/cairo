//! Cairo language server.
//!
//! Implements the LSP protocol over stdin/out.

mod semantic_highlighting;

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::Arc;

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::project::{setup_project, update_crate_roots_from_project_config};
use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    ConstantLongId, EnumLongId, ExternFunctionLongId, ExternTypeLongId, FileIndex,
    FreeFunctionLongId, FunctionWithBodyId, ImplFunctionLongId, ImplLongId, LanguageElementId,
    LookupItemId, ModuleFileId, ModuleId, ModuleItemId, StructLongId, TraitLongId, UseLongId,
};
use cairo_lang_diagnostics::{DiagnosticEntry, Diagnostics, ToOption};
use cairo_lang_filesystem::db::{
    AsFilesGroupMut, FilesGroup, FilesGroupEx, PrivRawFileContentQuery,
};
use cairo_lang_filesystem::ids::{FileId, FileLongId};
use cairo_lang_filesystem::span::TextPosition;
use cairo_lang_formatter::{get_formatted_file, FormatterConfig};
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::diagnostic::LoweringDiagnostic;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_parser::ParserDiagnostic;
use cairo_lang_project::ProjectConfig;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::function_with_body::SemanticExprLookup;
use cairo_lang_semantic::resolve_path::ResolvedGenericItem;
use cairo_lang_semantic::SemanticDiagnostic;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::GetIdentifier;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::stable_ptr::SyntaxStablePtr;
use cairo_lang_syntax::node::utils::is_grandparent_of_kind;
use cairo_lang_syntax::node::{ast, SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::{try_extract_matches, OptionHelper, Upcast};
use salsa::InternKey;
use semantic_highlighting::token_kind::SemanticTokenKind;
use semantic_highlighting::SemanticTokensTraverser;
use serde_json::Value;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use vfs::{ProvideVirtualFileRequest, ProvideVirtualFileResponse};
pub mod vfs;

const MAX_CRATE_DETECTION_DEPTH: usize = 20;

#[derive(Default, PartialEq, Eq)]
pub struct FileDiagnostics {
    pub parser: Diagnostics<ParserDiagnostic>,
    pub semantic: Diagnostics<SemanticDiagnostic>,
    pub lowering: Diagnostics<LoweringDiagnostic>,
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
    /// Locks and gets a database instance.
    async fn db(&self) -> tokio::sync::MutexGuard<'_, RootDatabase> {
        self.db_mutex.lock().await
    }
    /// Gets a FileId from a URI.
    fn file(&self, db: &RootDatabase, uri: Url) -> FileId {
        match uri.scheme() {
            "file" => {
                let path = uri.to_file_path().unwrap();
                FileId::new(db, path)
            }
            "vfs" => {
                let id = uri.host_str().unwrap().parse::<usize>().unwrap();
                FileId::from_intern_id(id.into())
            }
            _ => panic!(),
        }
    }

    fn get_uri(&self, db: &RootDatabase, file_id: FileId) -> Url {
        let virtual_file = match db.lookup_intern_file(file_id) {
            FileLongId::OnDisk(path) => return Url::from_file_path(path).unwrap(),
            FileLongId::Virtual(virtual_file) => virtual_file,
        };
        let uri = Url::parse(
            format!("vfs://{}/{}.cairo", file_id.as_intern_id().as_usize(), virtual_file.name)
                .as_str(),
        )
        .unwrap();
        uri
    }

    // TODO(spapini): Consider managing vfs in a different way, using the
    // client.send_notification::<UpdateVirtualFile> call.

    // Refresh diagnostics and send diffs to client.
    async fn refresh_diagnostics(&self) {
        let mut state = self.state_mutex.lock().await;

        // Get all files. Try to go over open files first.
        let mut files_set: OrderedHashSet<_> = state.open_files.iter().copied().collect();
        let db = self.db().await;
        for crate_id in db.crates() {
            for module_id in db.crate_modules(crate_id).iter() {
                for file_id in db.module_files(*module_id).unwrap_or_default() {
                    files_set.insert(file_id);
                }
            }
        }

        // Get all diagnostics.
        for file_id in files_set.iter().copied() {
            let uri = self.get_uri(&db, file_id);
            let new_file_diagnostics = FileDiagnostics {
                parser: db.file_syntax_diagnostics(file_id),
                semantic: db.file_semantic_diagnostics(file_id).unwrap_or_default(),
                lowering: db.file_lowering_diagnostics(file_id).unwrap_or_default(),
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
            self.get_diagnostics((*db).upcast(), &mut diags, &new_file_diagnostics.lowering);
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
            let uri = self.get_uri(&db, file_id);
            self.client.publish_diagnostics(uri, Vec::new(), None).await;
        }
    }

    /// Converts internal format diagnostics to LSP format.
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

    pub async fn vfs_provide(
        &self,
        params: ProvideVirtualFileRequest,
    ) -> Result<ProvideVirtualFileResponse> {
        let db = self.db().await;
        let file_id = self.file(&db, params.uri);
        Ok(ProvideVirtualFileResponse { content: db.file_content(file_id).map(|s| (*s).clone()) })
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
                definition_provider: Some(OneOf::Left(true)),
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
        let syntax = if let Ok(syntax) = db.file_syntax(file) {
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
        let syntax = if let Ok(syntax) = db.file_syntax(file) {
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

        let file_summary = if let Some(summary) = db.file_summary(file) {
            summary
        } else {
            eprintln!("Formatting failed. Cannot get summary for file '{file_uri}'.");
            return Ok(None);
        };
        let old_line_count = if let Ok(count) = file_summary.line_count().try_into() {
            count
        } else {
            eprintln!("Formatting failed. Line count out of bound in file '{file_uri}'.");
            return Ok(None);
        };

        Ok(Some(vec![TextEdit {
            range: Range {
                start: Position { line: 0, character: 0 },
                end: Position { line: old_line_count, character: 0 },
            },
            new_text,
        }]))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let db = self.db().await;
        let file_uri = params.text_document_position_params.text_document.uri;
        eprintln!("Hover {file_uri}");
        let file = self.file(&db, file_uri);
        let position = params.text_document_position_params.position;
        let Some((node, lookup_items)) =
            get_node_and_lookup_items(&*db, file, position) else { return Ok(None); };
        let Some(lookup_item_id) = lookup_items.into_iter().next() else {
                return Ok(None);
            };
        let function_id = match lookup_item_id {
            LookupItemId::ModuleItem(ModuleItemId::FreeFunction(free_function_id)) => {
                FunctionWithBodyId::Free(free_function_id)
            }
            LookupItemId::ImplFunction(impl_function_id) => {
                FunctionWithBodyId::Impl(impl_function_id)
            }
            _ => {
                return Ok(None);
            }
        };

        // Build texts.
        let mut hints = Vec::new();
        if let Some(hint) = get_expr_hint(&*db, function_id, node.clone()) {
            hints.push(MarkedString::String(hint));
        };
        if let Some(hint) = get_identifier_hint(&*db, lookup_item_id, node) {
            hints.push(MarkedString::String(hint));
        };

        Ok(Some(Hover { contents: HoverContents::Array(hints), range: None }))
    }
    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let db = self.db().await;
        let syntax_db = (*db).upcast();
        let file_uri = params.text_document_position_params.text_document.uri;
        let file = self.file(&db, file_uri.clone());
        let position = params.text_document_position_params.position;
        let Some((node, lookup_items)) = get_node_and_lookup_items(&*db, file, position) else {return Ok(None)};
        for lookup_item_id in lookup_items {
            if node.kind(syntax_db) != SyntaxKind::TokenIdentifier {
                continue;
            }
            let identifier =
                ast::TerminalIdentifier::from_syntax_node(syntax_db, node.parent().unwrap());
            let Some(item) = db.lookup_resolved_generic_item_by_ptr(
                lookup_item_id, identifier.stable_ptr())
            else { continue; };

            let defs_db = (*db).upcast();
            let (module_id, file_index, stable_ptr) = match item {
                ResolvedGenericItem::Constant(item) => (
                    item.parent_module(defs_db),
                    item.file_index(defs_db),
                    item.untyped_stable_ptr(defs_db),
                ),
                ResolvedGenericItem::Module(item) => {
                    (item, FileIndex(0), db.intern_stable_ptr(SyntaxStablePtr::Root))
                }
                ResolvedGenericItem::GenericFunction(item) => {
                    let sig = item.signature((*db).upcast());
                    (
                        sig.parent_module(defs_db),
                        sig.file_index(defs_db),
                        sig.untyped_stable_ptr(defs_db),
                    )
                }
                ResolvedGenericItem::GenericType(generic_type) => (
                    generic_type.parent_module(defs_db),
                    generic_type.file_index(defs_db),
                    generic_type.untyped_stable_ptr(defs_db),
                ),
                ResolvedGenericItem::GenericTypeAlias(type_alias) => (
                    type_alias.parent_module(defs_db),
                    type_alias.file_index(defs_db),
                    type_alias.untyped_stable_ptr(defs_db),
                ),
                ResolvedGenericItem::Variant(variant) => (
                    variant.id.parent_module(defs_db),
                    variant.id.file_index(defs_db),
                    variant.id.stable_ptr(defs_db).untyped(),
                ),
                ResolvedGenericItem::Trait(trt) => (
                    trt.parent_module(defs_db),
                    trt.file_index(defs_db),
                    trt.stable_ptr(defs_db).untyped(),
                ),
                ResolvedGenericItem::Impl(imp) => (
                    imp.parent_module(defs_db),
                    imp.file_index(defs_db),
                    imp.stable_ptr(defs_db).untyped(),
                ),
                ResolvedGenericItem::TraitFunction(trait_function) => (
                    trait_function.parent_module(defs_db),
                    trait_function.file_index(defs_db),
                    trait_function.stable_ptr(defs_db).untyped(),
                ),
            };

            let file = if let Ok(files) = db.module_files(module_id) {
                files[file_index.0]
            } else {
                return Ok(None);
            };

            let uri = self.get_uri(&db, file);
            let syntax = if let Ok(syntax) = db.file_syntax(file) {
                syntax
            } else {
                eprintln!("Formatting failed. File '{file_uri}' does not exist.");
                return Ok(None);
            };
            let node = syntax.as_syntax_node().lookup_ptr(syntax_db, stable_ptr);
            let span = node.span_without_trivia(syntax_db);

            let start = from_pos(span.start.position_in_file((*db).upcast(), file).unwrap());
            let end = from_pos(span.end.position_in_file((*db).upcast(), file).unwrap());

            return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                uri,
                range: Range { start, end },
            })));
        }
        return Ok(None);
    }
}

/// If the ast node is a lookup item, return the corresponding id. Otherwise, return None.
/// See [LookupItemId].
fn lookup_item_from_ast(
    db: &dyn SemanticGroup,
    module_file_id: ModuleFileId,
    node: SyntaxNode,
) -> Option<LookupItemId> {
    let syntax_db = db.upcast();
    // TODO(spapini): Handle trait items.
    match node.kind(syntax_db) {
        SyntaxKind::ItemConstant => Some(LookupItemId::ModuleItem(ModuleItemId::Constant(
            db.intern_constant(ConstantLongId(
                module_file_id,
                ast::ItemConstant::from_syntax_node(syntax_db, node).stable_ptr(),
            )),
        ))),
        SyntaxKind::FunctionWithBody => {
            if is_grandparent_of_kind(syntax_db, &node, SyntaxKind::ImplBody) {
                Some(LookupItemId::ImplFunction(db.intern_impl_function(ImplFunctionLongId(
                    module_file_id,
                    ast::FunctionWithBody::from_syntax_node(syntax_db, node).stable_ptr(),
                ))))
            } else {
                Some(LookupItemId::ModuleItem(ModuleItemId::FreeFunction(db.intern_free_function(
                    FreeFunctionLongId(
                        module_file_id,
                        ast::FunctionWithBody::from_syntax_node(syntax_db, node).stable_ptr(),
                    ),
                ))))
            }
        }
        SyntaxKind::ItemExternFunction => Some(LookupItemId::ModuleItem(
            ModuleItemId::ExternFunction(db.intern_extern_function(ExternFunctionLongId(
                module_file_id,
                ast::ItemExternFunction::from_syntax_node(syntax_db, node).stable_ptr(),
            ))),
        )),
        SyntaxKind::ItemExternType => Some(LookupItemId::ModuleItem(ModuleItemId::ExternType(
            db.intern_extern_type(ExternTypeLongId(
                module_file_id,
                ast::ItemExternType::from_syntax_node(syntax_db, node).stable_ptr(),
            )),
        ))),
        SyntaxKind::ItemTrait => {
            Some(LookupItemId::ModuleItem(ModuleItemId::Trait(db.intern_trait(TraitLongId(
                module_file_id,
                ast::ItemTrait::from_syntax_node(syntax_db, node).stable_ptr(),
            )))))
        }
        SyntaxKind::ItemImpl => {
            Some(LookupItemId::ModuleItem(ModuleItemId::Impl(db.intern_impl(ImplLongId(
                module_file_id,
                ast::ItemImpl::from_syntax_node(syntax_db, node).stable_ptr(),
            )))))
        }
        SyntaxKind::ItemStruct => {
            Some(LookupItemId::ModuleItem(ModuleItemId::Struct(db.intern_struct(StructLongId(
                module_file_id,
                ast::ItemStruct::from_syntax_node(syntax_db, node).stable_ptr(),
            )))))
        }
        SyntaxKind::ItemEnum => {
            Some(LookupItemId::ModuleItem(ModuleItemId::Enum(db.intern_enum(EnumLongId(
                module_file_id,
                ast::ItemEnum::from_syntax_node(syntax_db, node).stable_ptr(),
            )))))
        }
        SyntaxKind::ItemUse => Some(LookupItemId::ModuleItem(ModuleItemId::Use(db.intern_use(
            UseLongId(module_file_id, ast::ItemUse::from_syntax_node(syntax_db, node).stable_ptr()),
        )))),
        _ => None,
    }
}

/// Given a position in a file, return the syntax node for the token at that position, and all the
/// lookup items above this node.
fn get_node_and_lookup_items(
    db: &(dyn SemanticGroup + 'static),
    file: FileId,
    position: Position,
) -> Option<(SyntaxNode, Vec<LookupItemId>)> {
    let mut res = Vec::new();
    let syntax_db = db.upcast();
    let filename = file.file_name(db.upcast());

    // Get syntax for file.
    let syntax = db.file_syntax(file).to_option().on_none(|| {
        eprintln!("Formatting failed. File '{filename}' does not exist.");
    })?;

    // Get file summary.
    let file_summary = db.file_summary(file).on_none(|| {
        eprintln!("Hover failed. File '{filename}' does not exist.");
    })?;

    // Find offset for position.
    let line_offset = file_summary.line_offsets.get(position.line as usize).on_none(|| {
        eprintln!("Hover failed. Position out of bounds.");
    })?;
    // TODO(spapini): Check that character is not larger than line_length.
    let node = syntax
        .as_syntax_node()
        .lookup_offset(syntax_db, line_offset.add(position.character as usize));

    // Find module.
    let module_id = find_node_module(db, file, node.clone()).on_none(|| {
        eprintln!("Hover failed. Failed to find module.");
    })?;
    let file_index = FileIndex(0);
    let module_file_id = ModuleFileId(module_id, file_index);

    // Find containing function.
    let mut item_node = node.clone();
    loop {
        if let Some(item) = lookup_item_from_ast(db, module_file_id, item_node.clone()) {
            res.push(item);
        }
        match item_node.parent() {
            Some(next_node) => {
                item_node = next_node;
            }
            None => return Some((node, res)),
        }
    }
}

fn find_node_module(
    db: &(dyn SemanticGroup + 'static),
    main_file: FileId,
    mut node: SyntaxNode,
) -> Option<ModuleId> {
    let modules: Vec<_> = db.file_modules(main_file).into_iter().flatten().collect();
    let mut module = *modules.first()?;
    let syntax_db = db.upcast();

    let mut inner_module_names = vec![];
    while let Some(parent) = node.parent() {
        node = parent;
        if node.kind(syntax_db) == SyntaxKind::ItemModule {
            inner_module_names.push(
                ast::ItemModule::from_syntax_node(syntax_db, node.clone())
                    .stable_ptr()
                    .name_green(syntax_db)
                    .identifier(syntax_db),
            );
        }
    }
    for name in inner_module_names.into_iter().rev() {
        let submodule = try_extract_matches!(
            db.module_item_by_name(module, name).ok()??,
            ModuleItemId::Submodule
        )?;
        module = ModuleId::Submodule(submodule);
    }
    Some(module)
}

/// If the node is an identifier, retrieves a hover hint for it.
fn get_identifier_hint(
    db: &(dyn SemanticGroup + 'static),
    lookup_item_id: LookupItemId,
    node: SyntaxNode,
) -> Option<String> {
    let syntax_db = db.upcast();
    if node.kind(syntax_db) != SyntaxKind::TokenIdentifier {
        return None;
    }
    let identifier = ast::TerminalIdentifier::from_syntax_node(syntax_db, node.parent().unwrap());
    let item = db.lookup_resolved_generic_item_by_ptr(lookup_item_id, identifier.stable_ptr())?;

    // TODO(spapini): Also include concrete item hints.
    // TODO(spapini): Format this better.
    Some(format!("`{:?}`", item.debug(db)))
}

/// If the node is an expression, retrieves a hover hint for it.
fn get_expr_hint(
    db: &(dyn SemanticGroup + 'static),
    function_id: FunctionWithBodyId,
    mut node: SyntaxNode,
) -> Option<String> {
    let syntax_db = db.upcast();
    // Add type info if exists.
    while !is_expr(node.kind(syntax_db)) {
        node = node.parent()?;
    }
    let expr_node = ast::Expr::from_syntax_node(syntax_db, node);
    // Lookup semantic expression.
    let expr_id =
        db.lookup_expr_by_ptr(function_id, expr_node.stable_ptr()).to_option().on_none(|| {
            eprintln!("Hover failed. Semantic model not found for expression.");
        })?;
    let semantic_expr = db.expr_semantic(function_id, expr_id);
    // Format the hover text.
    Some(format!("Type: `{}`", semantic_expr.ty().format(db)))
}

/// Returns true if the current ast node is an expression.
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
fn detect_crate_for(db: &mut RootDatabase, file_path: &str) {
    let mut path = PathBuf::from(file_path);
    for _ in 0..MAX_CRATE_DETECTION_DEPTH {
        path.pop();
        if let Ok(config) = ProjectConfig::from_directory(path.as_path()) {
            update_crate_roots_from_project_config(db, config);
            return;
        };
    }
    // Fallback to a single file.
    if let Err(err) = setup_project(&mut *db, PathBuf::from(file_path).as_path()) {
        eprintln!("Error loading file {file_path} as a single crate: {err}");
    }
}
