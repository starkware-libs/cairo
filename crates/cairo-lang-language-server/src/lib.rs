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

use std::collections::{HashMap, HashSet};
use std::io;
use std::panic::{catch_unwind, AssertUnwindSafe, RefUnwindSafe};
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, SystemTime};

use anyhow::Context;
use cairo_lang_compiler::db::validate_corelib;
use cairo_lang_compiler::project::{setup_project, update_crate_roots_from_project_config};
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_diagnostics::Diagnostics;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{FileId, FileLongId};
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::diagnostic::LoweringDiagnostic;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_project::ProjectConfig;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::plugin::PluginSuite;
use cairo_lang_semantic::SemanticDiagnostic;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{Intern, LookupIntern, Upcast};
use itertools::Itertools;
use salsa::{Cancelled, ParallelDatabase};
use state::{FileDiagnostics, StateSnapshot};
use tokio::sync::Semaphore;
use tokio::task::spawn_blocking;
use tower_lsp::jsonrpc::{Error as LSPError, Result as LSPResult};
use tower_lsp::lsp_types::request::Request;
use tower_lsp::lsp_types::{TextDocumentPositionParams, Url};
use tower_lsp::{Client, ClientSocket, LanguageServer, LspService, Server};
use tracing::{debug, error, info, trace_span, warn, Instrument};

use crate::config::Config;
use crate::lang::db::AnalysisDatabase;
use crate::lang::diagnostics::lsp::map_cairo_diagnostics_to_lsp;
use crate::lang::lsp::LsProtoGroup;
use crate::lsp::ext::{
    CorelibVersionMismatch, ProvideVirtualFileRequest, ProvideVirtualFileResponse,
};
use crate::project::scarb::update_crate_roots;
use crate::project::unmanaged_core_crate::try_to_init_unmanaged_core;
use crate::project::ProjectManifestPath;
use crate::server::notifier::Notifier;
use crate::state::State;
use crate::toolchain::scarb::ScarbToolchain;

mod config;
mod env_config;
mod ide;
mod lang;
pub mod lsp;
mod project;
mod server;
mod state;
mod toolchain;

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
            .custom_method(lsp::ext::ProvideVirtualFile::METHOD, Self::vfs_provide)
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
                // Salsa is broken and sometimes when cancelled throws regular assert instead of
                // [`Cancelled`]. Catch this case too.
                if err.is::<Cancelled>()
                    || err.downcast_ref::<&str>().is_some_and(|msg| {
                        msg.contains(
                            "assertion failed: old_memo.revisions.changed_at <= \
                             revisions.changed_at",
                        )
                    })
                {
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

        let mut files_with_set_diagnostics: HashSet<Url> = HashSet::default();
        let mut processed_modules: HashSet<ModuleId> = HashSet::default();

        let open_files_ids: HashSet<FileId> = async {
            let state_snapshot = self.state_snapshot().await;
            let open_files = state_snapshot.open_files.iter();
            open_files.filter_map(|url| state_snapshot.db.file_for_url(url)).collect()
        }
        .instrument(trace_span!("get_open_files_ids"))
        .await;

        let open_files_modules = self.get_files_modules(open_files_ids.iter()).await;

        // Refresh open files modules first for better UX
        async {
            for (file, file_modules_ids) in open_files_modules {
                self.refresh_file_diagnostics(
                    &file,
                    &file_modules_ids,
                    &mut processed_modules,
                    &mut files_with_set_diagnostics,
                )
                .await;
            }
        }
        .instrument(trace_span!("refresh_open_files_modules"))
        .await;

        let rest_of_files = async {
            let mut rest_of_files: HashSet<FileId> = HashSet::default();
            let db = self.db_snapshot().await;
            for crate_id in db.crates() {
                for module_id in db.crate_modules(crate_id).iter() {
                    if let Ok(module_files) = db.module_files(*module_id) {
                        let unprocessed_files =
                            module_files.iter().filter(|file| !open_files_ids.contains(file));
                        rest_of_files.extend(unprocessed_files);
                    }
                }
            }
            rest_of_files
        }
        .instrument(trace_span!("get_rest_of_files"))
        .await;

        let rest_of_files_modules = self.get_files_modules(rest_of_files.iter()).await;

        // Refresh rest of files after, since they are not viewed currently
        async {
            for (file, file_modules_ids) in rest_of_files_modules {
                self.refresh_file_diagnostics(
                    &file,
                    &file_modules_ids,
                    &mut processed_modules,
                    &mut files_with_set_diagnostics,
                )
                .await;
            }
        }
        .instrument(trace_span!("refresh_other_files_modules"))
        .await;

        // Clear old diagnostics
        async {
            let mut removed_files = Vec::new();
            self.with_state_mut(|s| {
                s.file_diagnostics.retain(|uri, _| {
                    let retain = files_with_set_diagnostics.contains(uri);
                    if !retain {
                        removed_files.push(uri.clone());
                    }
                    retain
                });
            })
            .await;

            for file in removed_files {
                self.client
                    .publish_diagnostics(file, Vec::new(), None)
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
    async fn refresh_file_diagnostics(
        &self,
        file: &FileId,
        modules_ids: &Vec<ModuleId>,
        processed_modules: &mut HashSet<ModuleId>,
        files_with_set_diagnostics: &mut HashSet<Url>,
    ) {
        let state = self.state_snapshot().await;
        let db = state.db;
        let config = state.config;
        let file_url = db.url_for_file(*file);
        let mut semantic_file_diagnostics: Vec<SemanticDiagnostic> = vec![];
        let mut lowering_file_diagnostics: Vec<LoweringDiagnostic> = vec![];

        macro_rules! diags {
            ($db:ident. $query:ident($file_id:expr), $f:expr) => {
                trace_span!(stringify!($query)).in_scope(|| {
                    catch_unwind(AssertUnwindSafe(|| $db.$query($file_id)))
                        .map($f)
                        .inspect_err(|_| {
                            error!("caught panic when computing diagnostics for file {file_url}");
                        })
                        .unwrap_or_default()
                })
            };
        }

        for module_id in modules_ids {
            if !processed_modules.contains(module_id) {
                semantic_file_diagnostics.extend(
                    diags!(db.module_semantic_diagnostics(*module_id), Result::unwrap_or_default)
                        .get_all(),
                );
                lowering_file_diagnostics.extend(
                    diags!(db.module_lowering_diagnostics(*module_id), Result::unwrap_or_default)
                        .get_all(),
                );

                processed_modules.insert(*module_id);
            }
        }

        let parser_file_diagnostics = diags!(db.file_syntax_diagnostics(*file), |r| r);

        let new_file_diagnostics = FileDiagnostics {
            parser: parser_file_diagnostics,
            semantic: Diagnostics::from_iter(semantic_file_diagnostics),
            lowering: Diagnostics::from_iter(lowering_file_diagnostics),
        };

        if !new_file_diagnostics.is_empty() {
            files_with_set_diagnostics.insert(file_url.clone());
        }

        // Since we are using Arcs, this comparison should be efficient.
        let skip_update = self
            .with_state_mut(|state| {
                if let Some(old_file_diagnostics) = state.file_diagnostics.get(&file_url) {
                    if old_file_diagnostics == &new_file_diagnostics {
                        return true;
                    }
                }

                state.file_diagnostics.insert(file_url.clone(), new_file_diagnostics.clone());
                false
            })
            .await;

        if skip_update {
            return;
        }

        let mut diags = Vec::new();
        let trace_macro_diagnostics = config.trace_macro_diagnostics;
        map_cairo_diagnostics_to_lsp(
            (*db).upcast(),
            &mut diags,
            &new_file_diagnostics.parser,
            file,
            trace_macro_diagnostics,
        );
        map_cairo_diagnostics_to_lsp(
            (*db).upcast(),
            &mut diags,
            &new_file_diagnostics.semantic,
            file,
            trace_macro_diagnostics,
        );
        map_cairo_diagnostics_to_lsp(
            (*db).upcast(),
            &mut diags,
            &new_file_diagnostics.lowering,
            file,
            trace_macro_diagnostics,
        );

        // Drop database snapshot before we wait for the client responding to our notification.
        drop(db);

        self.client
            .publish_diagnostics(file_url, diags, None)
            .instrument(trace_span!("publish_diagnostics"))
            .await;
    }

    /// Gets the mapping of files to their respective modules.
    async fn get_files_modules(
        &self,
        files_ids: impl Iterator<Item = &FileId>,
    ) -> HashMap<FileId, Vec<ModuleId>> {
        let state_snapshot = self.state_snapshot().await;
        let mut result = HashMap::default();
        for file_id in files_ids {
            if let Ok(file_modules) = state_snapshot.db.file_modules(*file_id) {
                result.insert(*file_id, file_modules.iter().cloned().collect_vec());
            }
        }
        result
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
