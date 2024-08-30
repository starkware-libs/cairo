//! # CairoLS
//!
//! Implements the LSP protocol over stdin/out.
//!
//! ## Running vanilla
//!
//! This is basically the source code of the `cairo-language-server` and
//! `scarb cairo-language-server` binaries.
//!
//! ```no_run
//! # #![allow(clippy::needless_doctest_main)]
//! fn main() {
//!     cairo_lang_language_server::start();
//! }
//! ```
//!
//! ## Running with customizations
//!
//! Due to the immaturity of various Cairo compiler parts (especially around potentially
//! dynamically-loadable things), for some projects it might be necessary to provide a custom build
//! of CairoLS that includes custom modifications to the compiler.
//! The [`start_with_tricks`] function allows building a customized build of CairoLS that supports
//! project-specific features.
//! See the [`Tricks`] struct documentation for available customizations.
//!
//! ```no_run
//! # #![allow(clippy::needless_doctest_main)]
//! use cairo_lang_language_server::Tricks;
//!
//! # fn dojo_plugin_suite() -> cairo_lang_semantic::plugin::PluginSuite {
//! #    // Returning something realistic, to make sure restrictive trait bounds do compile.
//! #    cairo_lang_starknet::starknet_plugin_suite()
//! # }
//! fn main() {
//!     let mut tricks = Tricks::default();
//!     tricks.extra_plugin_suites = Some(&|| vec![dojo_plugin_suite()]);
//!     cairo_lang_language_server::start_with_tricks(tricks);
//! }
//! ```

use std::collections::HashSet;
use std::io;
use std::panic::{catch_unwind, AssertUnwindSafe, RefUnwindSafe};
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, SystemTime};

use anyhow::{bail, Context};
use cairo_lang_compiler::db::validate_corelib;
use cairo_lang_compiler::project::{setup_project, update_crate_roots_from_project_config};
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    FunctionTitleId, LanguageElementId, LookupItemId, MemberId, ModuleId, SubmoduleLongId,
    TraitItemId,
};
use cairo_lang_diagnostics::ToOption;
use cairo_lang_filesystem::db::{
    get_originating_location, AsFilesGroupMut, FilesGroup, FilesGroupEx, PrivRawFileContentQuery,
};
use cairo_lang_filesystem::ids::{FileId, FileLongId};
use cairo_lang_filesystem::span::{TextPosition, TextSpan};
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_project::ProjectConfig;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::function_with_body::SemanticExprLookup;
use cairo_lang_semantic::items::functions::GenericFunctionId;
use cairo_lang_semantic::items::imp::ImplLongId;
use cairo_lang_semantic::lookup_item::LookupItemEx;
use cairo_lang_semantic::plugin::PluginSuite;
use cairo_lang_semantic::resolve::{ResolvedConcreteItem, ResolvedGenericItem};
use cairo_lang_semantic::{Expr, TypeLongId};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{Intern, LookupIntern, Upcast};
use salsa::{Cancelled, ParallelDatabase};
use serde_json::Value;
use state::{FileDiagnostics, Owned, StateSnapshot};
use tokio::sync::Semaphore;
use tokio::task::spawn_blocking;
use tower_lsp::jsonrpc::{Error as LSPError, Result as LSPResult};
use tower_lsp::lsp_types::request::Request;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, ClientSocket, LanguageServer, LspService, Server};
use tracing::{debug, error, info, trace_span, warn, Instrument};

use crate::config::Config;
use crate::ide::semantic_highlighting::SemanticTokenKind;
use crate::lang::db::{AnalysisDatabase, LsSemanticGroup, LsSyntaxGroup};
use crate::lang::diagnostics::lsp::map_cairo_diagnostics_to_lsp;
use crate::lang::lsp::LsProtoGroup;
use crate::lsp::client_capabilities::ClientCapabilitiesExt;
use crate::lsp::ext::CorelibVersionMismatch;
use crate::project::scarb::update_crate_roots;
use crate::project::unmanaged_core_crate::try_to_init_unmanaged_core;
use crate::project::ProjectManifestPath;
use crate::server::notifier::Notifier;
use crate::state::State;
use crate::toolchain::scarb::ScarbToolchain;
use crate::vfs::{ProvideVirtualFileRequest, ProvideVirtualFileResponse};

mod config;
mod env_config;
mod ide;
mod lang;
pub mod lsp;
mod markdown;
mod project;
mod server;
mod state;
mod toolchain;
mod vfs;

/// Carries various customizations that can be applied to CairoLS.
///
/// See [the top-level documentation][lib] documentation for usage examples.
///
/// [lib]: crate#running-with-customizations
#[non_exhaustive]
#[derive(Default, Clone)]
pub struct Tricks {
    /// A function that returns a list of additional compiler plugin suites to be loaded in the
    /// language server database.
    pub extra_plugin_suites:
        Option<&'static (dyn Fn() -> Vec<PluginSuite> + Send + Sync + RefUnwindSafe)>,
}

/// Starts the language server.
///
/// See [the top-level documentation][lib] documentation for usage examples.
///
/// [lib]: crate#running-vanilla
pub fn start() {
    start_with_tricks(Tricks::default());
}

/// Number of LSP requests that can be processed concurrently.
/// Higher number than default tower_lsp::DEFAULT_MAX_CONCURRENCY = 4.
/// This is increased because we don't have to limit requests this way now.
/// Cancellation will skip requests that are no longer relevant so only latest ones will be
/// processed. Effectively there will be similar number of requests processed at once, but under
/// heavy load these will be more actual ones.
const REQUESTS_PROCESSED_CONCURRENTLY: usize = 100;

/// Starts the language server with customizations.
///
/// See [the top-level documentation][lib] documentation for usage examples.
///
/// [lib]: crate#running-with-customizations
#[tokio::main]
pub async fn start_with_tricks(tricks: Tricks) {
    let _log_guard = init_logging();

    info!("language server starting");
    env_config::report_to_logs();

    let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());

    let (service, socket) = Backend::build_service(tricks);
    Server::new(stdin, stdout, socket)
        .concurrency_level(REQUESTS_PROCESSED_CONCURRENTLY)
        .serve(service)
        .await;

    info!("language server stopped");
}

/// Special function to run the language server in end-to-end tests.
#[cfg(feature = "testing")]
pub fn build_service_for_e2e_tests() -> (LspService<impl LanguageServer>, ClientSocket) {
    Backend::build_service(Tricks::default())
}

/// Initialize logging infrastructure for the language server.
///
/// Returns a guard that should be dropped when the LS ends, to flush log files.
fn init_logging() -> Option<impl Drop> {
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
                .with_env_var(env_config::CAIRO_LS_LOG)
                .from_env_lossy(),
        );

    let profile_layer = if env_config::tracing_profile() {
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

/// Makes sure that all open files exist in the new db, with their current changes.
#[tracing::instrument(level = "trace", skip_all)]
fn ensure_exists_in_db(
    new_db: &mut AnalysisDatabase,
    old_db: &AnalysisDatabase,
    open_files: impl Iterator<Item = Url>,
) {
    let overrides = old_db.file_overrides();
    let mut new_overrides: OrderedHashMap<FileId, Arc<str>> = Default::default();
    for uri in open_files {
        let Some(file_id) = old_db.file_for_url(&uri) else { continue };
        let new_file_id = file_id.lookup_intern(old_db).intern(new_db);
        if let Some(content) = overrides.get(&file_id) {
            new_overrides.insert(new_file_id, content.clone());
        }
    }
    new_db.set_file_overrides(Arc::new(new_overrides));
}

struct Backend {
    client: Client,
    tricks: Tricks,
    // Lock making sure there is at most a single "diagnostic refresh" thread.
    refresh_lock: tokio::sync::Mutex<()>,
    // Semaphore making sure there are at most one worker and one waiter for refresh.
    refresh_waiters_semaphore: tokio::sync::Semaphore,
    state_mutex: tokio::sync::Mutex<State>,
    scarb_toolchain: ScarbToolchain,
    last_replace: tokio::sync::Mutex<SystemTime>,
    db_replace_interval: Duration,
}

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

impl Backend {
    fn build_service(tricks: Tricks) -> (LspService<Self>, ClientSocket) {
        LspService::build(|client| Self::new(client, tricks))
            .custom_method("vfs/provide", Self::vfs_provide)
            .custom_method(lsp::ext::ViewAnalyzedCrates::METHOD, Self::view_analyzed_crates)
            .custom_method(lsp::ext::ExpandMacro::METHOD, Self::expand_macro)
            .finish()
    }

    fn new(client: Client, tricks: Tricks) -> Self {
        let db = AnalysisDatabase::new(&tricks);
        let notifier = Notifier::new(&client);
        let scarb_toolchain = ScarbToolchain::new(&notifier);
        Self {
            client,
            tricks,
            refresh_lock: Default::default(),
            refresh_waiters_semaphore: Semaphore::new(2),
            state_mutex: State::new(db).into(),
            scarb_toolchain,
            last_replace: tokio::sync::Mutex::new(SystemTime::now()),
            db_replace_interval: env_config::db_replace_interval(),
        }
    }

    /// Catches panics and returns Err.
    async fn catch_panics<F, T>(&self, f: F) -> LSPResult<T>
    where
        F: FnOnce() -> T + Send + 'static,
        T: Send + 'static,
    {
        spawn_blocking(move || {
            catch_unwind(AssertUnwindSafe(f)).map_err(|err| {
                if err.is::<Cancelled>() {
                    debug!("LSP worker thread was cancelled");
                    LSPError::request_cancelled()
                } else {
                    error!("caught panic in LSP worker thread");
                    LSPError::internal_error()
                }
            })
        })
        .await
        .unwrap_or_else(|_| {
            error!("failed to join LSP worker thread");
            Err(LSPError::internal_error())
        })
    }

    /// Locks and gets a server state.
    #[tracing::instrument(level = "trace", skip_all)]
    async fn with_state_mut<F, T>(&self, f: F) -> T
    where
        F: FnOnce(&mut State) -> T,
    {
        let mut state = self.state_mutex.lock().await;

        f(&mut state)
    }

    /// Locks and produces server state snapshot.
    #[tracing::instrument(level = "trace", skip_all)]
    async fn state_snapshot(&self) -> StateSnapshot {
        self.with_state_mut(|state| state.snapshot()).await
    }

    /// Locks and produces db snapshot.
    #[tracing::instrument(level = "trace", skip_all)]
    async fn db_snapshot(&self) -> salsa::Snapshot<AnalysisDatabase> {
        self.with_state_mut(|state| state.db.snapshot()).await
    }

    /// Refresh diagnostics and send diffs to client.
    #[tracing::instrument(level = "debug", skip_all)]
    async fn refresh_diagnostics(&self) -> LSPResult<()> {
        // Making sure only a single thread is refreshing diagnostics at a time, and that at most
        // one thread is waiting to start refreshing. This allows changed to be grouped
        // together before querying the database, as well as releasing extra threads waiting to
        // start diagnostics updates.
        // TODO(orizi): Consider removing when request cancellation is supported.
        let Ok(waiter_permit) = self.refresh_waiters_semaphore.try_acquire() else { return Ok(()) };
        let refresh_lock = self.refresh_lock.lock().await;
        let open_files = self.state_snapshot().await.open_files;

        // First, refresh diagnostics for each open file.
        async {
            for uri in &*open_files {
                self.refresh_file_diagnostics(uri).await;
            }
        }
        .instrument(trace_span!("refresh_open_files_diagnostics"))
        .await;

        // Second, refresh diagnostics for the rest of the compilation unit.
        let files_set = async {
            let db = self.db_snapshot().await;
            let mut files_set = HashSet::new();
            for crate_id in db.crates() {
                for module_files in db
                    .crate_modules(crate_id)
                    .iter()
                    .filter_map(|module_id| db.module_files(*module_id).ok())
                {
                    for file_id in module_files.iter() {
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
            self.with_state_mut(|s| {
                s.file_diagnostics.retain(|uri, _| {
                    let retain = files_set.contains(uri);
                    if !retain {
                        removed_files.push(uri.clone());
                    }
                    retain
                });
            })
            .await;
            for uri in removed_files {
                self.client
                    .publish_diagnostics(uri, Vec::new(), None)
                    .instrument(trace_span!("publish_diagnostics"))
                    .await;
            }
        }
        .instrument(trace_span!("clear_old_diagnostics"))
        .await;

        // Release locks prior to potentially swapping the database.
        drop(refresh_lock);
        drop(waiter_permit);
        // After handling of all diagnostics attempting to swap the database to reduce memory
        // consumption.
        self.maybe_swap_database().await
    }

    /// Refresh diagnostics for a single file.
    #[tracing::instrument(level = "trace", skip_all, fields(%uri))]
    async fn refresh_file_diagnostics(&self, uri: &Url) {
        let state = self.state_snapshot().await;
        let db = state.db;
        let config = state.config;

        let Some(file_id) = db.file_for_url(uri) else { return };

        macro_rules! diags {
            ($db:ident. $query:ident($file_id:expr), $f:expr) => {
                trace_span!(stringify!($query)).in_scope(|| {
                    catch_unwind(AssertUnwindSafe(|| $db.$query($file_id)))
                        .map($f)
                        .inspect_err(|_| {
                            error!("caught panic when computing diagnostics for {uri}");
                        })
                        .unwrap_or_default()
                })
            };
        }

        let new_file_diagnostics = FileDiagnostics {
            parser: diags!(db.file_syntax_diagnostics(file_id), |r| r),
            semantic: diags!(db.file_semantic_diagnostics(file_id), Result::unwrap_or_default),
            lowering: diags!(db.file_lowering_diagnostics(file_id), Result::unwrap_or_default),
        };

        self.with_state_mut(|state| {
            // Since we are using Arcs, this comparison should be efficient.
            if let Some(old_file_diagnostics) = state.file_diagnostics.get(uri) {
                if old_file_diagnostics == &new_file_diagnostics {
                    return;
                }
            }
            state.file_diagnostics.insert(uri.clone(), new_file_diagnostics.clone());
        })
        .await;

        let mut diags = Vec::new();
        let trace_macro_diagnostics = config.trace_macro_diagnostics;
        map_cairo_diagnostics_to_lsp(
            (*db).upcast(),
            &mut diags,
            &new_file_diagnostics.parser,
            trace_macro_diagnostics,
        );
        map_cairo_diagnostics_to_lsp(
            (*db).upcast(),
            &mut diags,
            &new_file_diagnostics.semantic,
            trace_macro_diagnostics,
        );
        map_cairo_diagnostics_to_lsp(
            (*db).upcast(),
            &mut diags,
            &new_file_diagnostics.lowering,
            trace_macro_diagnostics,
        );

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
        let state = self.state_snapshot().await;
        let open_files = state.open_files;
        let config = &state.config;

        debug!("scheduled");
        let mut new_db = self
            .catch_panics({
                let open_files = open_files.clone();
                let tricks = self.tricks.clone();

                move || {
                    let mut new_db = AnalysisDatabase::new(&tricks);
                    ensure_exists_in_db(&mut new_db, &state.db, open_files.iter().cloned());
                    new_db
                }
            })
            .await?;
        debug!("initial setup done");
        self.ensure_diagnostics_queries_up_to_date(&mut new_db, config, open_files.iter().cloned())
            .await;
        debug!("initial compilation done");
        debug!("starting");
        self.with_state_mut(|state| {
            ensure_exists_in_db(&mut new_db, &state.db, state.open_files.iter().cloned());
            state.db = new_db;
        })
        .await;

        debug!("done");
        Ok(())
    }

    /// Ensures that all diagnostics are up to date.
    #[tracing::instrument(level = "trace", skip_all)]
    async fn ensure_diagnostics_queries_up_to_date(
        &self,
        db: &mut AnalysisDatabase,
        config: &Config,
        open_files: impl Iterator<Item = Url>,
    ) {
        let query_diags = |db: &AnalysisDatabase, file_id| {
            db.file_syntax_diagnostics(file_id);
            let _ = db.file_semantic_diagnostics(file_id);
            let _ = db.file_lowering_diagnostics(file_id);
        };
        for uri in open_files {
            let Some(file_id) = db.file_for_url(&uri) else { continue };
            if let FileLongId::OnDisk(file_path) = file_id.lookup_intern(db) {
                self.detect_crate_for(db, config, file_path).await;
            }
            query_diags(db, file_id);
        }
        for crate_id in db.crates() {
            for module_files in db
                .crate_modules(crate_id)
                .iter()
                .filter_map(|module_id| db.module_files(*module_id).ok())
            {
                for file_id in module_files.iter().copied() {
                    query_diags(db, file_id);
                }
            }
        }
    }

    #[tracing::instrument(level = "trace", skip_all)]
    async fn view_analyzed_crates(&self) -> LSPResult<String> {
        let db = self.db_snapshot().await;
        self.catch_panics(move || lang::inspect::crates::inspect_analyzed_crates(&db)).await
    }

    #[tracing::instrument(level = "trace", skip_all)]
    async fn expand_macro(&self, params: TextDocumentPositionParams) -> LSPResult<Option<String>> {
        let db = self.db_snapshot().await;
        self.catch_panics(move || ide::macros::expand::expand_macro(&db, &params)).await
    }

    #[tracing::instrument(level = "trace", skip_all)]
    async fn vfs_provide(
        &self,
        params: ProvideVirtualFileRequest,
    ) -> LSPResult<ProvideVirtualFileResponse> {
        let db = self.db_snapshot().await;
        self.catch_panics(move || {
            let content = db
                .file_for_url(&params.uri)
                .and_then(|file_id| db.file_content(file_id))
                .map(|content| content.to_string());
            ProvideVirtualFileResponse { content }
        })
        .await
    }

    /// Tries to detect the crate root the config that contains a cairo file, and add it to the
    /// system.
    #[tracing::instrument(level = "trace", skip_all)]
    async fn detect_crate_for(
        &self,
        db: &mut AnalysisDatabase,
        config: &Config,
        file_path: PathBuf,
    ) {
        match ProjectManifestPath::discover(&file_path) {
            Some(ProjectManifestPath::Scarb(manifest_path)) => {
                let Ok(metadata) = spawn_blocking({
                    let scarb = self.scarb_toolchain.clone();
                    move || {
                        scarb
                            .metadata(&manifest_path)
                            .with_context(|| {
                                format!(
                                    "failed to refresh scarb workspace: {}",
                                    manifest_path.display()
                                )
                            })
                            .inspect_err(|e| {
                                // TODO(mkaput): Send a notification to the language client.
                                warn!("{e:?}");
                            })
                            .ok()
                    }
                })
                .await
                else {
                    error!("scarb invoking thread panicked");
                    return;
                };

                if let Some(metadata) = metadata {
                    update_crate_roots(&metadata, db);
                } else {
                    // Try to set up a corelib at least.
                    try_to_init_unmanaged_core(db, config, &self.scarb_toolchain);
                }

                if let Err(result) = validate_corelib(db) {
                    self.client
                        .send_notification::<CorelibVersionMismatch>(result.to_string())
                        .await;
                }
            }

            Some(ProjectManifestPath::CairoProject(config_path)) => {
                // The base path of ProjectConfig must be absolute to ensure that all paths in Salsa
                // DB will also be absolute.
                assert!(config_path.is_absolute());

                try_to_init_unmanaged_core(db, config, &self.scarb_toolchain);

                if let Ok(config) = ProjectConfig::from_file(&config_path) {
                    update_crate_roots_from_project_config(db, &config);
                };
            }

            None => {
                try_to_init_unmanaged_core(db, config, &self.scarb_toolchain);

                if let Err(err) = setup_project(&mut *db, file_path.as_path()) {
                    let file_path_s = file_path.to_string_lossy();
                    error!("error loading file {file_path_s} as a single crate: {err}");
                }
            }
        }
    }

    /// Reload crate detection for all open files.
    #[tracing::instrument(level = "trace", skip_all)]
    async fn reload(&self) -> LSPResult<()> {
        self.reload_config().await;

        state_mut_async! {state, self,
            let db = &mut state.db;

            for uri in state.open_files.iter() {
                let Some(file_id) = db.file_for_url(uri) else { continue };
                if let FileLongId::OnDisk(file_path) = db.lookup_intern_file(file_id) {
                    self.detect_crate_for(db, &state.config, file_path).await;
                }
            }
        }
        .await;

        self.refresh_diagnostics().await
    }

    /// Reload the [`Config`] and all its dependencies.
    async fn reload_config(&self) {
        state_mut_async! {state, self,
            state.config.reload(&self.client, &state.client_capabilities).await;
        }
        .await;

        self.refresh_diagnostics().await.ok();
    }
}

enum ServerCommands {
    Reload,
}

impl TryFrom<String> for ServerCommands {
    type Error = anyhow::Error;

    fn try_from(value: String) -> anyhow::Result<Self> {
        match value.as_str() {
            "cairo.reload" => Ok(ServerCommands::Reload),
            _ => bail!("Unrecognized command: {value}"),
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    #[tracing::instrument(level = "debug", skip_all)]
    async fn initialize(&self, params: InitializeParams) -> LSPResult<InitializeResult> {
        self.with_state_mut(|state| {
            state.client_capabilities = Owned::new(Arc::new(params.capabilities));
        })
        .await;

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
                    commands: vec!["cairo.reload".to_string()],
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
        // Initialize the configuration.
        self.reload_config().await;

        let client_capabilities = self.state_snapshot().await.client_capabilities;

        if client_capabilities.did_change_watched_files_dynamic_registration() {
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
    }

    async fn shutdown(&self) -> LSPResult<()> {
        Ok(())
    }

    #[tracing::instrument(level = "debug", skip_all)]
    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {}

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

        // Reload workspace if Scarb.toml changed.
        for change in params.changes {
            let changed_file_path = change.uri.to_file_path().unwrap_or_default();
            let changed_file_name = changed_file_path.file_name().unwrap_or_default();
            if changed_file_name == "Scarb.toml" {
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

/// Extracts [`MemberId`] if the [`ast::TerminalIdentifier`] points to
/// right-hand side of access member expression e.g., to `xyz` in `self.xyz`.
fn try_extract_member(
    db: &AnalysisDatabase,
    identifier: &ast::TerminalIdentifier,
    lookup_items: &[LookupItemId],
) -> Option<MemberId> {
    let syntax_node = identifier.as_syntax_node();
    let binary_expr_syntax_node =
        db.first_ancestor_of_kind(syntax_node.clone(), SyntaxKind::ExprBinary)?;
    let binary_expr = ast::ExprBinary::from_syntax_node(db, binary_expr_syntax_node);

    let function_with_body = lookup_items.first()?.function_with_body()?;

    let expr_id =
        db.lookup_expr_by_ptr(function_with_body, binary_expr.stable_ptr().into()).ok()?;
    let semantic_expr = db.expr_semantic(function_with_body, expr_id);

    if let Expr::MemberAccess(expr_member_access) = semantic_expr {
        let pointer_to_rhs = binary_expr.rhs(db).stable_ptr().untyped();

        let mut current_node = syntax_node;
        // Check if the terminal identifier points to a member, not a struct variable.
        while pointer_to_rhs != current_node.stable_ptr() {
            // If we found the node with the binary expression, then we are sure we won't find the
            // node with the member.
            if current_node.stable_ptr() == binary_expr.stable_ptr().untyped() {
                return None;
            }
            current_node = current_node.parent().unwrap();
        }

        Some(expr_member_access.member)
    } else {
        None
    }
}

/// Either [`ResolvedGenericItem`], [`ResolvedConcreteItem`] or [`MemberId`].
enum ResolvedItem {
    Generic(ResolvedGenericItem),
    Concrete(ResolvedConcreteItem),
    Member(MemberId),
}

// TODO(mkaput): Move this to crate::lang::inspect::defs and make private.
#[tracing::instrument(level = "trace", skip_all)]
fn find_definition(
    db: &AnalysisDatabase,
    identifier: &ast::TerminalIdentifier,
    lookup_items: &[LookupItemId],
) -> Option<(ResolvedItem, SyntaxStablePtrId)> {
    if let Some(parent) = identifier.as_syntax_node().parent() {
        if parent.kind(db) == SyntaxKind::ItemModule {
            let Some(containing_module_file_id) = db.find_module_file_containing_node(&parent)
            else {
                error!("`find_definition` failed: could not find module");
                return None;
            };

            let submodule_id = SubmoduleLongId(
                containing_module_file_id,
                ast::ItemModule::from_syntax_node(db, parent).stable_ptr(),
            )
            .intern(db);
            let item = ResolvedGenericItem::Module(ModuleId::Submodule(submodule_id));
            return Some((
                ResolvedItem::Generic(item.clone()),
                resolved_generic_item_def(db, item)?,
            ));
        }
    }

    if let Some(member_id) = try_extract_member(db, identifier, lookup_items) {
        return Some((ResolvedItem::Member(member_id), member_id.untyped_stable_ptr(db)));
    }

    for lookup_item_id in lookup_items.iter().copied() {
        if let Some(item) =
            db.lookup_resolved_generic_item_by_ptr(lookup_item_id, identifier.stable_ptr())
        {
            return Some((
                ResolvedItem::Generic(item.clone()),
                resolved_generic_item_def(db, item)?,
            ));
        }

        if let Some(item) =
            db.lookup_resolved_concrete_item_by_ptr(lookup_item_id, identifier.stable_ptr())
        {
            let stable_ptr = resolved_concrete_item_def(db.upcast(), item.clone())?;
            return Some((ResolvedItem::Concrete(item), stable_ptr));
        }
    }

    // Skip variable definition, otherwise we would get parent ModuleItem for variable.
    if db.first_ancestor_of_kind(identifier.as_syntax_node(), SyntaxKind::StatementLet).is_none() {
        let item = match lookup_items.first().copied()? {
            LookupItemId::ModuleItem(item) => {
                ResolvedGenericItem::from_module_item(db, item).to_option()?
            }
            LookupItemId::TraitItem(trait_item) => {
                if let TraitItemId::Function(trait_fn) = trait_item {
                    ResolvedGenericItem::TraitFunction(trait_fn)
                } else {
                    ResolvedGenericItem::Trait(trait_item.trait_id(db))
                }
            }
            LookupItemId::ImplItem(impl_item) => {
                ResolvedGenericItem::Impl(impl_item.impl_def_id(db))
            }
        };

        Some((ResolvedItem::Generic(item.clone()), resolved_generic_item_def(db, item)?))
    } else {
        None
    }
}

#[tracing::instrument(level = "trace", skip_all)]
fn resolved_concrete_item_def(
    db: &AnalysisDatabase,
    item: ResolvedConcreteItem,
) -> Option<SyntaxStablePtrId> {
    match item {
        ResolvedConcreteItem::Type(ty) => {
            if let TypeLongId::GenericParameter(param) = ty.lookup_intern(db) {
                Some(param.untyped_stable_ptr(db.upcast()))
            } else {
                None
            }
        }
        ResolvedConcreteItem::Impl(imp) => {
            if let ImplLongId::GenericParameter(param) = imp.lookup_intern(db) {
                Some(param.untyped_stable_ptr(db.upcast()))
            } else {
                None
            }
        }
        _ => None,
    }
}

#[tracing::instrument(level = "trace", skip_all)]
fn resolved_generic_item_def(
    db: &AnalysisDatabase,
    item: ResolvedGenericItem,
) -> Option<SyntaxStablePtrId> {
    let defs_db = db.upcast();
    Some(match item {
        ResolvedGenericItem::GenericConstant(item) => item.untyped_stable_ptr(defs_db),
        ResolvedGenericItem::Module(module_id) => {
            // Check if the module is an inline submodule.
            if let ModuleId::Submodule(submodule_id) = module_id {
                if let ast::MaybeModuleBody::Some(submodule_id) =
                    submodule_id.stable_ptr(defs_db).lookup(db.upcast()).body(db.upcast())
                {
                    // Inline module.
                    return Some(submodule_id.stable_ptr().untyped());
                }
            }
            let module_file = db.module_main_file(module_id).ok()?;
            let file_syntax = db.file_module_syntax(module_file).ok()?;
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
                GenericFunctionId::Trait(id) => FunctionTitleId::Trait(id.trait_function(db)),
            };
            title.untyped_stable_ptr(defs_db)
        }
        ResolvedGenericItem::GenericType(generic_type) => generic_type.untyped_stable_ptr(defs_db),
        ResolvedGenericItem::GenericTypeAlias(type_alias) => type_alias.untyped_stable_ptr(defs_db),
        ResolvedGenericItem::GenericImplAlias(impl_alias) => impl_alias.untyped_stable_ptr(defs_db),
        ResolvedGenericItem::Variant(variant) => variant.id.stable_ptr(defs_db).untyped(),
        ResolvedGenericItem::Trait(trt) => trt.stable_ptr(defs_db).untyped(),
        ResolvedGenericItem::Impl(imp) => imp.stable_ptr(defs_db).untyped(),
        ResolvedGenericItem::TraitFunction(trait_function) => {
            trait_function.stable_ptr(defs_db).untyped()
        }
        ResolvedGenericItem::Variable(var) => var.untyped_stable_ptr(defs_db),
    })
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
    db: &AnalysisDatabase,
    file: FileId,
    position: TextPosition,
) -> Option<(FileId, TextSpan)> {
    let identifier = db.find_identifier_at_position(file, position)?;

    let syntax_db = db.upcast();
    let node = db.find_syntax_node_at_position(file, position)?;
    let lookup_items = db.collect_lookup_items_stack(&node)?;
    let (_, stable_ptr) = find_definition(db, &identifier, &lookup_items)?;
    let node = stable_ptr.lookup(syntax_db);
    let found_file = stable_ptr.file_id(syntax_db);
    let span = node.span_without_trivia(syntax_db);
    let width = span.width();
    let (file_id, mut span) = get_originating_location(db.upcast(), found_file, span.start_only());
    span.end = span.end.add_width(width);
    Some((file_id, span))
}
