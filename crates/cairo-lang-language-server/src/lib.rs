//! Cairo language server.
//!
//! Implements the LSP protocol over stdin/out.

use std::collections::{HashMap, HashSet};
use std::panic::AssertUnwindSafe;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, SystemTime};
use std::{env, io};

use anyhow::{bail, Error};
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::project::{setup_project, update_crate_roots_from_project_config};
use cairo_lang_defs::db::{get_all_path_leaves, DefsGroup};
use cairo_lang_defs::ids::{
    ConstantLongId, EnumLongId, ExternFunctionLongId, ExternTypeLongId, FileIndex,
    FreeFunctionLongId, FunctionTitleId, ImplAliasLongId, ImplDefLongId, ImplFunctionLongId,
    ImplItemId, LanguageElementId, LookupItemId, ModuleFileId, ModuleId, ModuleItemId,
    ModuleTypeAliasLongId, StructLongId, SubmoduleLongId, TraitFunctionLongId, TraitItemId,
    TraitLongId, UseLongId,
};
use cairo_lang_diagnostics::{Diagnostics, ToOption};
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_filesystem::db::{
    get_originating_location, init_dev_corelib, AsFilesGroupMut, CrateConfiguration, CrateSettings,
    FilesGroup, FilesGroupEx, PrivRawFileContentQuery,
};
use cairo_lang_filesystem::detect::detect_corelib;
use cairo_lang_filesystem::ids::{CrateId, CrateLongId, Directory, FileId, FileLongId};
use cairo_lang_filesystem::span::{FileSummary, TextOffset, TextSpan, TextWidth};
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::diagnostic::LoweringDiagnostic;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_parser::ParserDiagnostic;
use cairo_lang_project::ProjectConfig;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::functions::GenericFunctionId;
use cairo_lang_semantic::items::imp::ImplId;
use cairo_lang_semantic::resolve::{ResolvedConcreteItem, ResolvedGenericItem};
use cairo_lang_semantic::{SemanticDiagnostic, TypeLongId};
use cairo_lang_starknet::starknet_plugin_suite;
use cairo_lang_syntax::node::helpers::GetIdentifier;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::utils::is_grandparent_of_kind;
use cairo_lang_syntax::node::{ast, SyntaxNode, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_test_plugin::test_plugin_suite;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{try_extract_matches, OptionHelper, Upcast};
use serde_json::Value;
use tower_lsp::jsonrpc::{Error as LSPError, Result as LSPResult};
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tracing::{debug, error, info, trace_span, warn, Instrument};

use crate::ide::semantic_highlighting::SemanticTokenKind;
use crate::lang::diagnostics::lsp::map_cairo_diagnostics_to_lsp;
use crate::lang::lsp::LsProtoGroup;
use crate::scarb_service::{is_scarb_manifest_path, ScarbService};
use crate::vfs::{ProvideVirtualFileRequest, ProvideVirtualFileResponse};

mod ide;
mod lang;
mod scarb_service;
mod vfs;

const MAX_CRATE_DETECTION_DEPTH: usize = 20;
const DEFAULT_CAIRO_LSP_DB_REPLACE_INTERVAL: u64 = 300;

#[tokio::main]
pub async fn start() {
    let _log_guard = init_logging();

    info!("language server starting");

    let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());

    let db = configured_db();

    let (service, socket) = LspService::build(|client| Backend::new(client, db))
        .custom_method("vfs/provide", Backend::vfs_provide)
        .finish();
    Server::new(stdin, stdout, socket).serve(service).await;

    info!("language server stopped");
}

/// Initialize logging infrastructure for the language server.
///
/// Returns a guard that should be dropped when the LS ends, to flush log files.
fn init_logging() -> Option<impl Drop> {
    use std::ffi::OsString;
    use std::fs;
    use std::io::IsTerminal;

    use tracing_chrome::{ChromeLayerBuilder, TraceStyle};
    use tracing_subscriber::filter::{EnvFilter, LevelFilter};
    use tracing_subscriber::fmt::format::FmtSpan;
    use tracing_subscriber::fmt::time::Uptime;
    use tracing_subscriber::fmt::Layer;
    use tracing_subscriber::prelude::*;

    let mut guard = None;

    let fmt_layer = Layer::new()
        .with_writer(io::stderr)
        .with_timer(Uptime::default())
        .with_ansi(io::stderr().is_terminal())
        .with_span_events(FmtSpan::CLOSE)
        .with_filter(
            EnvFilter::builder()
                .with_default_directive(LevelFilter::WARN.into())
                .with_env_var("CAIRO_LS_LOG")
                .from_env_lossy(),
        );

    fn env_to_bool(os: Option<OsString>) -> bool {
        matches!(os.as_ref().and_then(|os| os.to_str()), Some("1") | Some("true"))
    }

    let profile_layer = if env_to_bool(env::var_os("CAIRO_LS_PROFILE")) {
        let mut path = PathBuf::from(format!(
            "./cairols-profile-{}.json",
            SystemTime::UNIX_EPOCH.elapsed().unwrap().as_micros()
        ));

        // Create the file now, so that we early panic, and `fs::canonicalize` will work.
        let profile_file = fs::File::create(&path).expect("Failed to create profile file.");

        // Try to canonicalize the path, so that it's easier to find the file from logs.
        if let Ok(canonical) = fs::canonicalize(&path) {
            path = canonical;
        }

        eprintln!("this LS run will output tracing profile to: {}", path.display());
        eprintln!(
            "open that file with https://ui.perfetto.dev (or chrome://tracing) to analyze it"
        );

        let (profile_layer, profile_layer_guard) = ChromeLayerBuilder::new()
            .writer(profile_file)
            .trace_style(TraceStyle::Async)
            .include_args(true)
            .build();

        guard = Some(profile_layer_guard);
        Some(profile_layer)
    } else {
        None
    };

    tracing::subscriber::set_global_default(
        tracing_subscriber::registry().with(fmt_layer).with(profile_layer),
    )
    .expect("Could not set up global logger.");

    guard
}

fn configured_db() -> RootDatabase {
    let db = RootDatabase::builder()
        // TODO(mkaput): Cfg items should be pulled from Scarb metadata.
        .with_cfg(CfgSet::from_iter([Cfg::name("test"), Cfg::kv("target", "test")]))
        .with_plugin_suite(starknet_plugin_suite())
        .with_plugin_suite(test_plugin_suite())
        .build()
        .expect("Failed to initialize Cairo compiler database.");
    db
}

/// Makes sure that all open files exist in the new db, with their current changes.
#[tracing::instrument(level = "trace", skip_all)]
fn ensure_exists_in_db(
    new_db: &mut RootDatabase,
    old_db: &RootDatabase,
    open_files: impl Iterator<Item = Url>,
) {
    let overrides = old_db.file_overrides();
    let mut new_overrides: OrderedHashMap<FileId, Arc<String>> = Default::default();
    for uri in open_files {
        let file_id = old_db.file_for_url(&uri);
        let new_file_id = new_db.intern_file(old_db.lookup_intern_file(file_id));
        if let Some(content) = overrides.get(&file_id) {
            new_overrides.insert(new_file_id, content.clone());
        }
    }
    new_db.set_file_overrides(Arc::new(new_overrides));
}

#[derive(Clone, Default, PartialEq, Eq)]
pub struct FileDiagnostics {
    pub parser: Diagnostics<ParserDiagnostic>,
    pub semantic: Diagnostics<SemanticDiagnostic>,
    pub lowering: Diagnostics<LoweringDiagnostic>,
}
impl std::panic::UnwindSafe for FileDiagnostics {}
#[derive(Clone, Default)]
pub struct State {
    pub file_diagnostics: HashMap<Url, FileDiagnostics>,
    pub open_files: HashSet<Url>,
}
impl std::panic::UnwindSafe for State {}

pub struct Backend {
    pub client: Client,
    // TODO(spapini): Remove this once we support ParallelDatabase.
    // State mutex should only be taken after db mutex is taken, to avoid deadlocks.
    pub db_mutex: tokio::sync::Mutex<RootDatabase>,
    pub state_mutex: tokio::sync::Mutex<State>,
    pub scarb: ScarbService,
    last_replace: tokio::sync::Mutex<SystemTime>,
    db_replace_interval: Duration,
}

impl Backend {
    pub fn new(client: Client, db: RootDatabase) -> Self {
        let scarb = ScarbService::new(&client);
        Self {
            client,
            db_mutex: db.into(),
            state_mutex: State::default().into(),
            scarb,
            last_replace: tokio::sync::Mutex::new(SystemTime::now()),
            db_replace_interval: Duration::from_secs(
                env::var("CAIRO_LSP_DB_REPLACE_INTERVAL")
                    .ok()
                    .and_then(|value| value.parse::<u64>().ok())
                    .unwrap_or(DEFAULT_CAIRO_LSP_DB_REPLACE_INTERVAL),
            ),
        }
    }

    /// Runs a function with a database snapshot.
    /// Catches panics and returns Err.
    async fn with_db<F, T>(&self, f: F) -> LSPResult<T>
    where
        F: FnOnce(&RootDatabase) -> T + std::panic::UnwindSafe,
    {
        let db_mut = self.db_mut().await;
        let db = db_mut.snapshot();
        drop(db_mut);
        std::panic::catch_unwind(AssertUnwindSafe(|| f(&db))).map_err(|_| {
            error!("caught panic in LSP worker thread");
            LSPError::internal_error()
        })
    }

    /// Locks and gets a database instance.
    #[tracing::instrument(level = "trace", skip_all)]
    async fn db_mut(&self) -> tokio::sync::MutexGuard<'_, RootDatabase> {
        self.db_mutex.lock().await
    }

    /// Locks and gets a server state.
    #[tracing::instrument(level = "trace", skip_all)]
    async fn state_mut(&self) -> tokio::sync::MutexGuard<'_, State> {
        self.state_mutex.lock().await
    }

    // TODO(spapini): Consider managing vfs in a different way, using the
    // client.send_notification::<UpdateVirtualFile> call.

    /// Refresh diagnostics and send diffs to client.
    #[tracing::instrument(level = "debug", skip_all)]
    async fn refresh_diagnostics(&self) -> LSPResult<()> {
        let open_files = self.state_mut().await.open_files.clone();

        // First, refresh diagnostics for each open file.
        async {
            for uri in &open_files {
                self.refresh_file_diagnostics(uri).await;
            }
        }
        .instrument(trace_span!("refresh_open_files_diagnostics"))
        .await;

        // Second, refresh diagnostics for the rest of the compilation unit.
        let files_set = async {
            let db = self.db_mut().await;
            let mut files_set = HashSet::new();
            for crate_id in db.crates() {
                for module_id in db.crate_modules(crate_id).iter() {
                    for file_id in db.module_files(*module_id).unwrap_or_default().iter() {
                        files_set.insert(db.url_for_file(*file_id));
                    }
                }
            }
            files_set
        }
        .instrument(trace_span!("get_all_files"))
        .await;

        async {
            for uri in files_set.iter().filter(|uri| !open_files.contains(uri)) {
                self.refresh_file_diagnostics(uri).await;
            }
        }
        .instrument(trace_span!("refresh_closed_files_diagnostics"))
        .await;

        // Finally, clear old diagnostics.
        async {
            let mut removed_files = Vec::new();
            self.state_mut().await.file_diagnostics.retain(|uri, _| {
                let retain = files_set.contains(uri);
                if !retain {
                    removed_files.push(uri.clone());
                }
                retain
            });
            for uri in removed_files {
                self.client
                    .publish_diagnostics(uri, Vec::new(), None)
                    .instrument(trace_span!("publish_diagnostics"))
                    .await;
            }
        }
        .instrument(trace_span!("clear_old_diagnostics"))
        .await;

        // After handling of all diagnostics attempting to swap the database to reduce memory
        // consumption.
        self.maybe_swap_database().await
    }

    /// Refresh diagnostics for a single file.
    #[tracing::instrument(level = "trace", skip_all, fields(%uri))]
    async fn refresh_file_diagnostics(&self, uri: &Url) {
        let db = self.db_mut().await;

        let file_id = db.file_for_url(uri);
        let new_file_diagnostics = FileDiagnostics {
            parser: trace_span!("file_syntax_diagnostics")
                .in_scope(|| db.file_syntax_diagnostics(file_id)),
            semantic: trace_span!("file_semantic_diagnostics")
                .in_scope(|| db.file_semantic_diagnostics(file_id).unwrap_or_default()),
            lowering: trace_span!("file_lowering_diagnostics")
                .in_scope(|| db.file_lowering_diagnostics(file_id).unwrap_or_default()),
        };

        let mut state = self.state_mut().await;

        // Since we are using Arcs, this comparison should be efficient.
        if let Some(old_file_diagnostics) = state.file_diagnostics.get(uri) {
            if old_file_diagnostics == &new_file_diagnostics {
                return;
            }
        }
        state.file_diagnostics.insert(uri.clone(), new_file_diagnostics.clone());

        drop(state);

        let mut diags = Vec::new();
        map_cairo_diagnostics_to_lsp((*db).upcast(), &mut diags, &new_file_diagnostics.parser);
        map_cairo_diagnostics_to_lsp((*db).upcast(), &mut diags, &new_file_diagnostics.semantic);
        map_cairo_diagnostics_to_lsp((*db).upcast(), &mut diags, &new_file_diagnostics.lowering);

        // Drop database snapshot before we wait for the client responding to our notification.
        drop(db);

        self.client
            .publish_diagnostics(uri.clone(), diags, None)
            .instrument(trace_span!("publish_diagnostics"))
            .await;
    }

    /// Checks if enough time passed since last db swap, and if so, swaps the database.
    #[tracing::instrument(level = "trace", skip_all)]
    async fn maybe_swap_database(&self) -> LSPResult<()> {
        let Ok(mut last_replace) = self.last_replace.try_lock() else {
            // Another thread is already swapping the database.
            return Ok(());
        };
        if last_replace.elapsed().unwrap() <= self.db_replace_interval {
            // Not enough time passed since last swap.
            return Ok(());
        }
        let result = self.swap_database().await;
        *last_replace = SystemTime::now();
        result
    }

    /// Perform database swap
    #[tracing::instrument(level = "debug", skip_all)]
    async fn swap_database(&self) -> LSPResult<()> {
        let open_files = self.state_mut().await.open_files.clone();
        debug!("scheduled");
        let mut new_db = self
            .with_db(|db| {
                let mut new_db = configured_db();
                ensure_exists_in_db(&mut new_db, db, open_files.iter().cloned());
                new_db
            })
            .await?;
        debug!("initial setup done");
        self.ensure_diagnostics_queries_up_to_date(&mut new_db, open_files.into_iter()).await;
        debug!("initial compilation done");
        let mut db = self.db_mut().await;
        debug!("starting");
        let state = self.state_mut().await;
        ensure_exists_in_db(&mut new_db, &db, state.open_files.iter().cloned());
        *db = new_db;
        debug!("done");
        Ok(())
    }

    /// Ensures that all diagnostics are up to date.
    #[tracing::instrument(level = "trace", skip_all)]
    async fn ensure_diagnostics_queries_up_to_date(
        &self,
        db: &mut RootDatabase,
        open_files: impl Iterator<Item = Url>,
    ) {
        let query_diags = |db: &RootDatabase, file_id| {
            db.file_syntax_diagnostics(file_id);
            let _ = db.file_semantic_diagnostics(file_id);
            let _ = db.file_lowering_diagnostics(file_id);
        };
        for uri in open_files {
            let file_id = db.file_for_url(&uri);
            if let FileLongId::OnDisk(file_path) = db.lookup_intern_file(file_id) {
                self.detect_crate_for(db, file_path).await;
            }
            query_diags(db, file_id);
        }
        for crate_id in db.crates() {
            for module_id in db.crate_modules(crate_id).iter() {
                for file_id in db.module_files(*module_id).unwrap_or_default().iter().copied() {
                    query_diags(db, file_id);
                }
            }
        }
    }

    #[tracing::instrument(level = "trace", skip_all)]
    pub async fn vfs_provide(
        &self,
        params: ProvideVirtualFileRequest,
    ) -> LSPResult<ProvideVirtualFileResponse> {
        self.with_db(|db| {
            let file_id = db.file_for_url(&params.uri);
            ProvideVirtualFileResponse { content: db.file_content(file_id).map(|s| (*s).clone()) }
        })
        .await
    }

    /// Get corelib path fallback from the client configuration.
    ///
    /// The value is set by the user under the `cairo1.corelibPath` key in client configuration.
    /// The value is not required to be set.
    /// The path may omit the `corelib/src` or `src` suffix.
    #[tracing::instrument(level = "trace", skip_all)]
    async fn get_corelib_fallback_path(&self) -> Option<PathBuf> {
        const CORELIB_CONFIG_SECTION: &str = "cairo1.corelibPath";
        let item = vec![ConfigurationItem {
            scope_uri: None,
            section: Some(CORELIB_CONFIG_SECTION.to_string()),
        }];
        let corelib_response = self.client.configuration(item).await;
        match corelib_response.map_err(Error::from) {
            Ok(value_vec) => {
                if let Some(Value::String(value)) = value_vec.first() {
                    if !value.is_empty() {
                        let root_path: PathBuf = value.into();

                        let mut path = root_path.clone();
                        path.push("corelib");
                        path.push("src");
                        if path.exists() {
                            return Some(path);
                        }

                        let mut path = root_path.clone();
                        path.push("src");
                        if path.exists() {
                            return Some(path);
                        }

                        if root_path.exists() {
                            return Some(root_path);
                        }
                    }
                }
            }
            Err(err) => {
                let err =
                    err.context("Failed to get configuration under `cairo1.corelibPath` key.");
                warn!("{err:?}");
            }
        };
        None
    }

    /// Tries to detect the crate root the config that contains a cairo file, and add it to the
    /// system.
    #[tracing::instrument(level = "trace", skip_all)]
    async fn detect_crate_for(&self, db: &mut RootDatabase, file_path: PathBuf) {
        let corelib_fallback = self.get_corelib_fallback_path().await;
        if self.scarb.is_scarb_project(file_path.clone()) {
            if self.scarb.is_scarb_found() {
                // Carrying out Scarb based setup.
                let corelib = match self.scarb.corelib_path(file_path.clone()).await {
                    Ok(corelib) => corelib,
                    Err(err) => {
                        let err =
                            err.context("Failed to obtain scarb corelib path from manifest file.");
                        warn!("{err:?}");
                        None
                    }
                };
                if let Some(corelib) = corelib.or_else(detect_corelib).or(corelib_fallback) {
                    init_dev_corelib(db, corelib);
                } else {
                    warn!("Failed to find corelib path.");
                }

                match self.scarb.crate_source_paths(file_path).await {
                    Ok(source_paths) => {
                        update_crate_roots(db, source_paths.clone());
                    }
                    Err(err) => {
                        let err =
                            err.context("Failed to obtain scarb metadata from manifest file.");
                        warn!("{err:?}");
                    }
                };
                return;
            } else {
                warn!("Not resolving Scarb metadata from manifest file due to missing Scarb path.");
                self.client.send_notification::<ScarbPathMissing>(()).await;
            }
        }

        // Scarb based setup not possible.
        if let Some(corelib) = detect_corelib().or(corelib_fallback) {
            init_dev_corelib(db, corelib);
        } else {
            warn!("Failed to find corelib path.");
        }

        // Fallback to cairo_project manifest format.
        let mut path = file_path.clone();
        for _ in 0..MAX_CRATE_DETECTION_DEPTH {
            path.pop();
            // Check for a cairo project file.
            if let Ok(config) = ProjectConfig::from_directory(path.as_path()) {
                update_crate_roots_from_project_config(db, &config);
                return;
            };
        }

        // Fallback to a single file.
        if let Err(err) = setup_project(&mut *db, file_path.as_path()) {
            let file_path_s = file_path.to_string_lossy();
            error!("error loading file {file_path_s} as a single crate: {err}");
        }
    }

    /// Reload crate detection for all open files.
    #[tracing::instrument(level = "trace", skip_all)]
    pub async fn reload(&self) -> LSPResult<()> {
        let mut db = self.db_mut().await;
        for uri in self.state_mutex.lock().await.open_files.iter() {
            let file_id = db.file_for_url(uri);
            if let FileLongId::OnDisk(file_path) = db.lookup_intern_file(file_id) {
                self.detect_crate_for(&mut db, file_path).await;
            }
        }
        drop(db);
        self.refresh_diagnostics().await
    }
}

#[derive(Debug)]
pub struct ScarbPathMissing {}

impl Notification for ScarbPathMissing {
    type Params = ();
    const METHOD: &'static str = "scarb/could-not-find-scarb-executable";
}

#[derive(Debug)]
pub struct ScarbResolvingStart {}

impl Notification for ScarbResolvingStart {
    type Params = ();
    const METHOD: &'static str = "scarb/resolving-start";
}

#[derive(Debug)]
pub struct ScarbResolvingFinish {}

impl Notification for ScarbResolvingFinish {
    type Params = ();
    const METHOD: &'static str = "scarb/resolving-finish";
}

pub enum ServerCommands {
    Reload,
}

impl TryFrom<String> for ServerCommands {
    type Error = anyhow::Error;

    fn try_from(value: String) -> anyhow::Result<Self> {
        match value.as_str() {
            "cairo1.reload" => Ok(ServerCommands::Reload),
            _ => bail!("Unrecognized command: {value}"),
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    #[tracing::instrument(level = "debug", skip_all)]
    async fn initialize(&self, _: InitializeParams) -> LSPResult<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                    all_commit_characters: None,
                    work_done_progress_options: Default::default(),
                    completion_item: None,
                }),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["cairo1.reload".to_string()],
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
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                ..ServerCapabilities::default()
            },
        })
    }

    #[tracing::instrument(level = "debug", skip_all)]
    async fn initialized(&self, _: InitializedParams) {
        // Register patterns for client file watcher.
        // This is used to detect changes to Scarb.toml and invalidate .cairo files.
        let registration_options = DidChangeWatchedFilesRegistrationOptions {
            watchers: vec!["/**/*.cairo", "/**/Scarb.toml"]
                .into_iter()
                .map(|glob_pattern| FileSystemWatcher {
                    glob_pattern: GlobPattern::String(glob_pattern.to_string()),
                    kind: None,
                })
                .collect(),
        };
        let registration = Registration {
            id: "workspace/didChangeWatchedFiles".to_string(),
            method: "workspace/didChangeWatchedFiles".to_string(),
            register_options: Some(serde_json::to_value(registration_options).unwrap()),
        };
        let result = self.client.register_capability(vec![registration]).await;
        if let Err(err) = result {
            warn!("Failed to register workspace/didChangeWatchedFiles event: {:#?}", err);
        }
    }

    async fn shutdown(&self) -> LSPResult<()> {
        Ok(())
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {}

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {}

    #[tracing::instrument(level = "debug", skip_all)]
    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        // Invalidate changed cairo files.
        let mut db = self.db_mut().await;
        for change in &params.changes {
            if is_cairo_file_path(&change.uri) {
                let file = db.file_for_url(&change.uri);
                PrivRawFileContentQuery.in_db_mut(db.as_files_group_mut()).invalidate(&file);
            }
        }
        drop(db);
        // Reload workspace if Scarb.toml changed.
        for change in params.changes {
            if is_scarb_manifest_path(&change.uri) {
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
        let mut db = self.db_mut().await;
        let uri = params.text_document.uri;

        // Try to detect the crate for physical files.
        // The crate for virtual files is already known.
        if uri.scheme() == "file" {
            let Ok(path) = uri.to_file_path() else {
                return;
            };
            self.detect_crate_for(&mut db, path).await;
        }

        let file_id = db.file_for_url(&uri);
        self.state_mut().await.open_files.insert(uri);
        db.override_file_content(file_id, Some(Arc::new(params.text_document.text)));
        drop(db);
        self.refresh_diagnostics().await.ok();
    }

    #[tracing::instrument(level = "debug", skip_all, fields(uri = %params.text_document.uri))]
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let text =
            if let [TextDocumentContentChangeEvent { text, .. }] = &params.content_changes[..] {
                text
            } else {
                error!("unexpected format of document change");
                return;
            };
        let mut db = self.db_mut().await;
        let uri = params.text_document.uri;
        let file = db.file_for_url(&uri);
        db.override_file_content(file, Some(Arc::new(text.into())));
        drop(db);
        self.refresh_diagnostics().await.ok();
    }

    #[tracing::instrument(level = "debug", skip_all, fields(uri = %params.text_document.uri))]
    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let mut db = self.db_mut().await;
        let file = db.file_for_url(&params.text_document.uri);
        PrivRawFileContentQuery.in_db_mut(db.as_files_group_mut()).invalidate(&file);
        db.override_file_content(file, None);
    }

    #[tracing::instrument(level = "debug", skip_all, fields(uri = %params.text_document.uri))]
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let mut db = self.db_mut().await;
        self.state_mut().await.open_files.remove(&params.text_document.uri);
        let file = db.file_for_url(&params.text_document.uri);
        db.override_file_content(file, None);
        drop(db);
        self.refresh_diagnostics().await.ok();
    }

    #[tracing::instrument(level = "trace", skip_all)]
    async fn completion(&self, params: CompletionParams) -> LSPResult<Option<CompletionResponse>> {
        self.with_db(|db| ide::completion::complete(params, db)).await
    }

    #[tracing::instrument(level = "trace", skip_all)]
    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> LSPResult<Option<SemanticTokensResult>> {
        self.with_db(|db| ide::semantic_highlighting::semantic_highlight_full(params, db)).await
    }

    #[tracing::instrument(level = "trace", skip_all)]
    async fn formatting(
        &self,
        params: DocumentFormattingParams,
    ) -> LSPResult<Option<Vec<TextEdit>>> {
        self.with_db(|db| ide::formatter::format(params, db)).await
    }

    #[tracing::instrument(level = "trace", skip_all)]
    async fn hover(&self, params: HoverParams) -> LSPResult<Option<Hover>> {
        self.with_db(|db| ide::hover::hover(params, db)).await
    }

    #[tracing::instrument(level = "trace", skip_all)]
    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> LSPResult<Option<GotoDefinitionResponse>> {
        self.with_db(|db| ide::navigation::goto_definition::goto_definition(params, db)).await
    }

    #[tracing::instrument(level = "trace", skip_all)]
    async fn code_action(&self, params: CodeActionParams) -> LSPResult<Option<CodeActionResponse>> {
        self.with_db(|db| ide::code_actions::code_actions(params, db)).await
    }
}

#[tracing::instrument(level = "trace", skip_all)]
fn find_definition(
    db: &RootDatabase,
    file: FileId,
    identifier: &ast::TerminalIdentifier,
    lookup_items: &[LookupItemId],
) -> Option<SyntaxStablePtrId> {
    if let Some(parent) = identifier.as_syntax_node().parent() {
        if parent.kind(db) == SyntaxKind::ItemModule {
            let containing_module_id =
                find_node_module(db, file, parent.clone()).on_none(|| {
                    error!("`find_definition` failed: could not find module");
                })?;

            let submodule_id = db.intern_submodule(SubmoduleLongId(
                ModuleFileId(containing_module_id, FileIndex(0)),
                ast::ItemModule::from_syntax_node(db, parent).stable_ptr(),
            ));
            return Some(resolved_generic_item_def(
                db.upcast(),
                ResolvedGenericItem::Module(ModuleId::Submodule(submodule_id)),
            ));
        }
    }
    for lookup_item_id in lookup_items.iter().copied() {
        if let Some(item) =
            db.lookup_resolved_generic_item_by_ptr(lookup_item_id, identifier.stable_ptr())
        {
            return Some(resolved_generic_item_def(db.upcast(), item));
        } else if let Some(item) =
            db.lookup_resolved_concrete_item_by_ptr(lookup_item_id, identifier.stable_ptr())
        {
            return resolved_concrete_item_def(db.upcast(), item);
        }
    }
    None
}

#[tracing::instrument(level = "trace", skip_all)]
fn resolved_concrete_item_def(
    db: &dyn SemanticGroup,
    item: ResolvedConcreteItem,
) -> Option<SyntaxStablePtrId> {
    match item {
        ResolvedConcreteItem::Type(ty) => {
            if let TypeLongId::GenericParameter(param) = db.lookup_intern_type(ty) {
                Some(param.untyped_stable_ptr(db.upcast()))
            } else {
                None
            }
        }
        ResolvedConcreteItem::Impl(ImplId::GenericParameter(param)) => {
            Some(param.untyped_stable_ptr(db.upcast()))
        }
        _ => None,
    }
}

#[tracing::instrument(level = "trace", skip_all)]
fn resolved_generic_item_def(db: &dyn DefsGroup, item: ResolvedGenericItem) -> SyntaxStablePtrId {
    match item {
        ResolvedGenericItem::Constant(item) => item.untyped_stable_ptr(db),
        ResolvedGenericItem::Module(module_id) => {
            // Check if the module is an inline submodule.
            if let ModuleId::Submodule(submodule_id) = module_id {
                if let ast::MaybeModuleBody::Some(submodule_id) =
                    submodule_id.stable_ptr(db.upcast()).lookup(db.upcast()).body(db.upcast())
                {
                    // Inline module.
                    return submodule_id.stable_ptr().untyped();
                }
            }
            let module_file = db.module_main_file(module_id).unwrap();
            let file_syntax = db.file_module_syntax(module_file).unwrap();
            file_syntax.as_syntax_node().stable_ptr()
        }
        ResolvedGenericItem::GenericFunction(item) => {
            let title = match item {
                GenericFunctionId::Free(id) => FunctionTitleId::Free(id),
                GenericFunctionId::Extern(id) => FunctionTitleId::Extern(id),
                GenericFunctionId::Impl(id) => {
                    // Note: Only the trait title is returned.
                    FunctionTitleId::Trait(id.function)
                }
            };
            title.untyped_stable_ptr(db)
        }
        ResolvedGenericItem::GenericType(generic_type) => generic_type.untyped_stable_ptr(db),
        ResolvedGenericItem::GenericTypeAlias(type_alias) => type_alias.untyped_stable_ptr(db),
        ResolvedGenericItem::GenericImplAlias(impl_alias) => impl_alias.untyped_stable_ptr(db),
        ResolvedGenericItem::Variant(variant) => variant.id.stable_ptr(db).untyped(),
        ResolvedGenericItem::Trait(trt) => trt.stable_ptr(db).untyped(),
        ResolvedGenericItem::Impl(imp) => imp.stable_ptr(db).untyped(),
        ResolvedGenericItem::TraitFunction(trait_function) => {
            trait_function.stable_ptr(db).untyped()
        }
        ResolvedGenericItem::Variable(_item, var) => var.untyped_stable_ptr(db),
    }
}

/// If the ast node is a lookup item, return the corresponding id. Otherwise, return None.
/// See [LookupItemId].
#[tracing::instrument(level = "trace", skip_all)]
fn lookup_item_from_ast(
    db: &dyn SemanticGroup,
    module_file_id: ModuleFileId,
    node: SyntaxNode,
) -> Vec<LookupItemId> {
    let syntax_db = db.upcast();
    // TODO(spapini): Handle trait items.
    match node.kind(syntax_db) {
        SyntaxKind::ItemConstant => vec![LookupItemId::ModuleItem(ModuleItemId::Constant(
            db.intern_constant(ConstantLongId(
                module_file_id,
                ast::ItemConstant::from_syntax_node(syntax_db, node).stable_ptr(),
            )),
        ))],
        SyntaxKind::FunctionWithBody => {
            if is_grandparent_of_kind(syntax_db, &node, SyntaxKind::ImplBody) {
                vec![LookupItemId::ImplItem(ImplItemId::Function(db.intern_impl_function(
                    ImplFunctionLongId(
                        module_file_id,
                        ast::FunctionWithBody::from_syntax_node(syntax_db, node).stable_ptr(),
                    ),
                )))]
            } else {
                vec![LookupItemId::ModuleItem(ModuleItemId::FreeFunction(db.intern_free_function(
                    FreeFunctionLongId(
                        module_file_id,
                        ast::FunctionWithBody::from_syntax_node(syntax_db, node).stable_ptr(),
                    ),
                )))]
            }
        }
        SyntaxKind::ItemExternFunction => vec![LookupItemId::ModuleItem(
            ModuleItemId::ExternFunction(db.intern_extern_function(ExternFunctionLongId(
                module_file_id,
                ast::ItemExternFunction::from_syntax_node(syntax_db, node).stable_ptr(),
            ))),
        )],
        SyntaxKind::ItemExternType => vec![LookupItemId::ModuleItem(ModuleItemId::ExternType(
            db.intern_extern_type(ExternTypeLongId(
                module_file_id,
                ast::ItemExternType::from_syntax_node(syntax_db, node).stable_ptr(),
            )),
        ))],
        SyntaxKind::ItemTrait => {
            vec![LookupItemId::ModuleItem(ModuleItemId::Trait(db.intern_trait(TraitLongId(
                module_file_id,
                ast::ItemTrait::from_syntax_node(syntax_db, node).stable_ptr(),
            ))))]
        }
        SyntaxKind::TraitItemFunction => {
            vec![LookupItemId::TraitItem(TraitItemId::Function(db.intern_trait_function(
                TraitFunctionLongId(
                    module_file_id,
                    ast::TraitItemFunction::from_syntax_node(syntax_db, node).stable_ptr(),
                ),
            )))]
        }
        SyntaxKind::ItemImpl => {
            vec![LookupItemId::ModuleItem(ModuleItemId::Impl(db.intern_impl(ImplDefLongId(
                module_file_id,
                ast::ItemImpl::from_syntax_node(syntax_db, node).stable_ptr(),
            ))))]
        }
        SyntaxKind::ItemStruct => {
            vec![LookupItemId::ModuleItem(ModuleItemId::Struct(db.intern_struct(StructLongId(
                module_file_id,
                ast::ItemStruct::from_syntax_node(syntax_db, node).stable_ptr(),
            ))))]
        }
        SyntaxKind::ItemEnum => {
            vec![LookupItemId::ModuleItem(ModuleItemId::Enum(db.intern_enum(EnumLongId(
                module_file_id,
                ast::ItemEnum::from_syntax_node(syntax_db, node).stable_ptr(),
            ))))]
        }
        SyntaxKind::ItemUse => {
            // Item use is not a lookup item, so we need to collect all UseLeaf, which are lookup
            // items.
            let item_use = ast::ItemUse::from_syntax_node(db.upcast(), node);
            let path_leaves = get_all_path_leaves(db.upcast(), item_use.use_path(syntax_db));
            let mut res = Vec::new();
            for path_leaf in path_leaves {
                let use_long_id = UseLongId(module_file_id, path_leaf.stable_ptr());
                let lookup_item_id =
                    LookupItemId::ModuleItem(ModuleItemId::Use(db.intern_use(use_long_id)));
                res.push(lookup_item_id);
            }
            res
        }
        SyntaxKind::ItemTypeAlias => vec![LookupItemId::ModuleItem(ModuleItemId::TypeAlias(
            db.intern_module_type_alias(ModuleTypeAliasLongId(
                module_file_id,
                ast::ItemTypeAlias::from_syntax_node(syntax_db, node).stable_ptr(),
            )),
        ))],
        SyntaxKind::ItemImplAlias => vec![LookupItemId::ModuleItem(ModuleItemId::ImplAlias(
            db.intern_impl_alias(ImplAliasLongId(
                module_file_id,
                ast::ItemImplAlias::from_syntax_node(syntax_db, node).stable_ptr(),
            )),
        ))],
        _ => vec![],
    }
}

/// Given a position in a file, return the syntax node for the token at that position, and all the
/// lookup items above this node.
#[tracing::instrument(level = "trace", skip_all)]
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
        error!("`get_node_and_lookup_items` failed: file '{filename}' does not exist");
    })?;

    // Get file summary and content.
    let file_summary = db.file_summary(file).on_none(|| {
        error!("`get_node_and_lookup_items` failed: file '{filename}' does not exist");
    })?;
    let content = db.file_content(file).on_none(|| {
        error!("`get_node_and_lookup_items` failed: file '{filename}' does not exist");
    })?;

    // Find offset for position.
    let offset = position_to_offset(file_summary, position, &content)?;
    let node = syntax.lookup_offset(syntax_db, offset);

    // Find module.
    let module_id = find_node_module(db, file, node.clone()).on_none(|| {
        error!("`get_node_and_lookup_items` failed: failed to find module");
    })?;
    let file_index = FileIndex(0);
    let module_file_id = ModuleFileId(module_id, file_index);

    // Find containing function.
    let mut item_node = node.clone();
    loop {
        for item in lookup_item_from_ast(db, module_file_id, item_node.clone()) {
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

#[tracing::instrument(level = "trace", skip_all)]
fn position_to_offset(
    file_summary: Arc<FileSummary>,
    position: Position,
    content: &str,
) -> Option<TextOffset> {
    let mut offset = *file_summary.line_offsets.get(position.line as usize).on_none(|| {
        error!("hover failed: position out of bounds");
    })?;
    let mut chars_it = offset.take_from(content).chars();
    for _ in 0..position.character {
        let c = chars_it.next().on_none(|| {
            error!("position does not exist");
        })?;
        offset = offset.add_width(TextWidth::from_char(c));
    }
    Some(offset)
}

#[tracing::instrument(level = "trace", skip_all)]
fn find_node_module(
    db: &dyn SemanticGroup,
    main_file: FileId,
    mut node: SyntaxNode,
) -> Option<ModuleId> {
    let mut module = db.file_modules(main_file).unwrap_or_default().iter().copied().next()?;
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

#[tracing::instrument(level = "trace", skip_all)]
fn update_crate_roots(
    db: &mut dyn SemanticGroup,
    source_paths: Vec<(CrateLongId, PathBuf, CrateSettings)>,
) {
    let source_paths = source_paths
        .into_iter()
        .filter_map(|(crate_long_id, source_path, crate_settings)| {
            let file_stem =
                source_path.clone().file_stem().map(|x| x.to_string_lossy().to_string());

            let crate_root: Option<PathBuf> = if !source_path.is_dir() {
                source_path.clone().parent().map(|x| x.to_path_buf())
            } else {
                Some(source_path.clone())
            };

            match (crate_root, file_stem) {
                (Some(crate_root), Some(file_stem)) => {
                    let crate_id = db.intern_crate(crate_long_id);
                    Some((crate_id, crate_root, crate_settings, file_stem))
                }
                _ => None,
            }
        })
        .collect::<Vec<_>>();

    for (crate_id, crate_root, settings, _file_stem) in source_paths.clone() {
        let crate_root = Directory::Real(crate_root);
        db.set_crate_config(crate_id, Some(CrateConfiguration { root: crate_root, settings }));
    }

    let source_paths = source_paths
        .into_iter()
        .filter(|(_crate_id, _crate_root, _edition, file_stem)| *file_stem != "lib")
        .map(|(crate_id, _crate_root, _edition, file_stem)| (crate_id, file_stem))
        .collect::<Vec<_>>();

    inject_virtual_wrapper_lib(db, source_paths);
}

/// Generates a wrapper lib file for appropriate compilation units.
///
/// This approach allows compiling crates that do not define `lib.cairo` file.
/// For example, single file crates can be created this way.
/// The actual single file module is defined as `mod` item in created lib file.
#[tracing::instrument(level = "trace", skip_all)]
fn inject_virtual_wrapper_lib(db: &mut dyn SemanticGroup, components: Vec<(CrateId, String)>) {
    for (crate_id, file_stem) in components {
        let module_id = ModuleId::CrateRoot(crate_id);
        let file_id = db.module_main_file(module_id).unwrap();
        // Inject virtual lib file wrapper.
        db.as_files_group_mut()
            .override_file_content(file_id, Some(Arc::new(format!("mod {file_stem};"))));
    }
}

fn is_cairo_file_path(file_path: &Url) -> bool {
    file_path.path().ends_with(".cairo")
}

/// Returns the file id and span of the definition of an expression from its position.
///
/// # Arguments
///
/// * `db` - Preloaded compilation database
/// * `uri` - Uri of the expression position
/// * `position` - Position of the expression
///
/// # Returns
///
/// The [FileId] and [TextSpan] of the expression definition if found.
fn get_definition_location(
    db: &RootDatabase,
    file: FileId,
    position: Position,
) -> Option<(FileId, TextSpan)> {
    let syntax_db = db.upcast();
    let (node, lookup_items) = get_node_and_lookup_items(db, file, position)?;
    if node.kind(syntax_db) != SyntaxKind::TokenIdentifier {
        return None;
    }
    let identifier = ast::TerminalIdentifier::from_syntax_node(syntax_db, node.parent().unwrap());
    let stable_ptr = find_definition(db, file, &identifier, &lookup_items)?;
    let node = stable_ptr.lookup(syntax_db);
    let found_file = stable_ptr.file_id(syntax_db);
    let span = node.span_without_trivia(syntax_db);
    let width = span.width();
    let (file_id, mut span) = get_originating_location(db.upcast(), found_file, span.start_only());
    span.end = span.end.add_width(width);
    Some((file_id, span))
}
