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

use std::io;
use std::num::NonZeroUsize;
use std::panic::RefUnwindSafe;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::sync::Arc;
use std::time::SystemTime;

use anyhow::{Context, Result};
use cairo_lang_compiler::db::validate_corelib;
use cairo_lang_compiler::project::{setup_project, update_crate_roots_from_project_config};
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{FileId, FileLongId};
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_project::ProjectConfig;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::plugin::PluginSuite;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{Intern, LookupIntern};
use lsp_server::Message;
use lsp_types::{ClientCapabilities, Registration, RegistrationParams, Url};
use tracing::{debug, error, info, warn};

use crate::config::Config;
use crate::lang::db::AnalysisDatabase;
use crate::lang::diagnostics::Diagnostics;
use crate::lang::lsp::LsProtoGroup;
use crate::lsp::capabilities::server::{
    collect_dynamic_registrations, collect_server_capabilities,
};
use crate::lsp::ext::CorelibVersionMismatch;
use crate::lsp::result::LSPResult;
use crate::project::scarb::update_crate_roots;
use crate::project::unmanaged_core_crate::try_to_init_unmanaged_core;
use crate::project::ProjectManifestPath;
use crate::server::client::{Client, Notifier, Requester};
use crate::server::connection::{ClientSender, Connection, ConnectionInitializer};
use crate::server::panic::catch_cancellation;
use crate::server::schedule::task::BackgroundSchedule;
use crate::server::schedule::{event_loop_thread, thread, JoinHandle, Scheduler, Task};
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
pub fn start() -> ExitCode {
    start_with_tricks(Tricks::default())
}

/// Starts the language server with customizations.
///
/// See [the top-level documentation][lib] documentation for usage examples.
///
/// [lib]: crate#running-with-customizations
pub fn start_with_tricks(tricks: Tricks) -> ExitCode {
    let _log_guard = init_logging();

    info!("language server starting");
    env_config::report_to_logs();

    let exit_code = match Backend::new(tricks) {
        Ok(backend) => {
            if let Err(err) = backend.run().map(|handle| handle.join()) {
                error!("language server encountered an unrecoverable error: {err}");
                ExitCode::from(1)
            } else {
                ExitCode::from(0)
            }
        }
        Err(err) => {
            error!("language server failed during initialization: {err}");
            ExitCode::from(1)
        }
    };

    info!("language server stopped");
    exit_code
}

/// Special function to run the language server in end-to-end tests.
#[cfg(feature = "testing")]
pub fn build_service_for_e2e_tests()
-> (Box<dyn FnOnce() -> BackendForTesting + Send>, lsp_server::Connection) {
    BackendForTesting::new_for_testing(Default::default())
}

/// Initialize logging infrastructure for the language server.
///
/// Returns a guard that should be dropped when the LS ends, to flush log files.
fn init_logging() -> Option<impl Drop> {
    use std::fs;
    use std::io::IsTerminal;

    use tracing_chrome::ChromeLayerBuilder;
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

        let (profile_layer, profile_layer_guard) =
            ChromeLayerBuilder::new().writer(profile_file).include_args(true).build();

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
fn ensure_exists_in_db<'a>(
    new_db: &mut AnalysisDatabase,
    old_db: &AnalysisDatabase,
    open_files: impl Iterator<Item = &'a Url>,
) {
    let overrides = old_db.file_overrides();
    let mut new_overrides: OrderedHashMap<FileId, Arc<str>> = Default::default();
    for uri in open_files {
        let Some(file_id) = old_db.file_for_url(uri) else { continue };
        let new_file_id = file_id.lookup_intern(old_db).intern(new_db);
        if let Some(content) = overrides.get(&file_id) {
            new_overrides.insert(new_file_id, content.clone());
        }
    }
    new_db.set_file_overrides(Arc::new(new_overrides));
}

struct Backend {
    connection: Connection,
    state: State,
}

#[cfg(feature = "testing")]
pub struct BackendForTesting(Backend);

#[cfg(feature = "testing")]
impl BackendForTesting {
    fn new_for_testing(
        tricks: Tricks,
    ) -> (Box<dyn FnOnce() -> BackendForTesting + Send>, lsp_server::Connection) {
        let (connection_initializer, client) = ConnectionInitializer::memory();

        let init = Box::new(|| {
            BackendForTesting(Backend::new_inner(tricks, connection_initializer).unwrap())
        });

        (init, client)
    }

    pub fn run_for_tests(self) -> Result<JoinHandle<Result<()>>> {
        let Backend { mut state, connection } = self.0;

        event_loop_thread(move || {
            let scheduler = Backend::initial_setup(&mut state, &connection);

            let result = Backend::event_loop(&connection, scheduler);

            if let Err(err) = connection.close() {
                error!("failed to close connection to the language server: {err:?}");
            }

            result
        })
    }
}

impl Backend {
    fn new(tricks: Tricks) -> Result<Self> {
        let connection_initializer = ConnectionInitializer::stdio();

        Self::new_inner(tricks, connection_initializer)
    }

    fn new_inner(tricks: Tricks, connection_initializer: ConnectionInitializer) -> Result<Self> {
        let (id, init_params) = connection_initializer.initialize_start()?;

        let client_capabilities = init_params.capabilities;
        let server_capabilities = collect_server_capabilities(&client_capabilities);

        let connection = connection_initializer.initialize_finish(id, server_capabilities)?;
        let state = Self::create_state(connection.make_sender(), client_capabilities, tricks);

        Ok(Self { connection, state })
    }

    fn create_state(
        sender: ClientSender,
        client_capabilities: ClientCapabilities,
        tricks: Tricks,
    ) -> State {
        let db = AnalysisDatabase::new(&tricks);
        let notifier = Client::new(sender).notifier();
        let scarb_toolchain = ScarbToolchain::new(notifier);

        State::new(db, client_capabilities, scarb_toolchain, tricks)
    }

    fn run(self) -> Result<JoinHandle<Result<()>>> {
        let Self { mut state, connection } = self;

        event_loop_thread(move || {
            let scheduler = Self::initial_setup(&mut state, &connection);

            let result = Self::event_loop(&connection, scheduler);

            if let Err(err) = connection.close() {
                error!("failed to close connection to the language server: {err:?}");
            }

            result
        })
    }

    fn initial_setup<'a>(state: &'a mut State, connection: &'_ Connection) -> Scheduler<'a> {
        let four = NonZeroUsize::new(4).unwrap();
        // If this is not set we can deadlock.
        let min_threads = NonZeroUsize::new(
            1 + // Diagnostics main thread.
            1 + // Diagnostics background thread.
            1, // Background thread for other jobs.
        )
        .unwrap();

        let worker_threads =
            std::thread::available_parallelism().unwrap_or(four).max(four).min(min_threads);

        let dynamic_registrations = collect_dynamic_registrations(&state.client_capabilities);

        let background_pool = thread::SharedPool::new(worker_threads);

        let mut scheduler =
            Scheduler::new(state, background_pool.clone(), connection.make_sender());

        // The dynamic registration and config-reload happen here instead of as a reaction to
        // `initialized` notification as it is more convenient implementation-wise.
        // Ideally, we would wait for responses to these requests here instead of in the event loop,
        // but we did not find a suitable way to do that.
        if let Err(error) =
            Self::register_dynamic_capabilities(&mut scheduler, dynamic_registrations)
        {
            error!(
                "failed to register dynamic capabilities, some features may not work properly: \
                 {error:?}"
            )
        }

        // Reloading config has to be done as a sync task to access mutable state that is borrowed
        // by scheduler.
        scheduler.local(|state, _notifier, requester, _responder| {
            Self::reload_config(state, requester).ok();
        });

        let (diagnostics_post_hook, diagnostics_main_job) = Diagnostics::tasks(background_pool);

        scheduler.background(BackgroundSchedule::Worker, diagnostics_main_job);

        // Important: swap should occur before using snapshots in diagnostics.
        scheduler.sync_task_post_hook(Self::maybe_swap_database);
        // Important: register this after all requests during initialization so we don't trigger
        // diagnostics refresh too early (better UX).
        scheduler.sync_task_post_hook(diagnostics_post_hook);

        scheduler
    }

    fn register_dynamic_capabilities(
        scheduler: &mut Scheduler<'_>,
        registrations: Vec<Registration>,
    ) -> Result<()> {
        let response_handler = |()| {
            debug!("configuration file watcher successfully registered");
            Task::nothing()
        };

        scheduler.request::<lsp_types::request::RegisterCapability>(
            RegistrationParams { registrations },
            response_handler,
        )
    }

    // +--------------------------------------------------+
    // | Function code adopted from:                      |
    // | Repository: https://github.com/astral-sh/ruff    |
    // | File: `crates/ruff_server/src/server.rs`         |
    // | Commit: 46a457318d8d259376a2b458b3f814b9b795fe69 |
    // +--------------------------------------------------+
    fn event_loop(connection: &Connection, mut scheduler: Scheduler<'_>) -> Result<()> {
        for msg in connection.incoming() {
            if connection.handle_shutdown(&msg)? {
                break;
            }
            let task = match msg {
                Message::Request(req) => server::request(req),
                Message::Notification(notification) => server::notification(notification),
                Message::Response(response) => scheduler.response(response),
            };
            scheduler.dispatch(task);
        }

        Ok(())
    }

    /// Checks if enough time passed since last db swap, and if so, swaps the database.
    #[tracing::instrument(level = "trace", skip_all)]
    fn maybe_swap_database(state: &mut State, notifier: Notifier) {
        if state.last_replace.elapsed().unwrap() <= state.db_replace_interval {
            // Not enough time passed since last swap.
            return;
        }

        Backend::swap_database(state, &notifier);

        state.last_replace = SystemTime::now();
    }

    /// Perform database swap
    #[tracing::instrument(level = "debug", skip_all)]
    fn swap_database(state: &mut State, notifier: &Notifier) {
        debug!("scheduled");
        let Ok(mut new_db) = catch_cancellation(|| {
            let mut new_db = AnalysisDatabase::new(&state.tricks);
            ensure_exists_in_db(&mut new_db, &state.db, state.open_files.iter());
            new_db
        }) else {
            return;
        };
        debug!("initial setup done");
        Backend::ensure_diagnostics_queries_up_to_date(
            &mut new_db,
            &state.scarb_toolchain,
            &state.config,
            state.open_files.iter(),
            notifier,
        );
        debug!("initial compilation done");
        debug!("starting");

        ensure_exists_in_db(&mut new_db, &state.db, state.open_files.iter());
        state.db = new_db;

        debug!("done");
    }

    /// Ensures that all diagnostics are up to date.
    #[tracing::instrument(level = "trace", skip_all)]
    fn ensure_diagnostics_queries_up_to_date<'a>(
        db: &mut AnalysisDatabase,
        scarb_toolchain: &ScarbToolchain,
        config: &Config,
        open_files: impl Iterator<Item = &'a Url>,
        notifier: &Notifier,
    ) {
        let query_diags = |db: &AnalysisDatabase, file_id| {
            db.file_syntax_diagnostics(file_id);
            let _ = db.file_semantic_diagnostics(file_id);
            let _ = db.file_lowering_diagnostics(file_id);
        };
        for uri in open_files {
            let Some(file_id) = db.file_for_url(uri) else { continue };
            if let FileLongId::OnDisk(file_path) = file_id.lookup_intern(db) {
                Backend::detect_crate_for(db, scarb_toolchain, config, &file_path, notifier);
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

    /// Tries to detect the crate root the config that contains a cairo file, and add it to the
    /// system.
    #[tracing::instrument(level = "trace", skip_all)]
    fn detect_crate_for(
        db: &mut AnalysisDatabase,
        scarb_toolchain: &ScarbToolchain,
        config: &Config,
        file_path: &Path,
        notifier: &Notifier,
    ) {
        match ProjectManifestPath::discover(file_path) {
            Some(ProjectManifestPath::Scarb(manifest_path)) => {
                let metadata = scarb_toolchain
                    .metadata(&manifest_path)
                    .with_context(|| {
                        format!("failed to refresh scarb workspace: {}", manifest_path.display())
                    })
                    .inspect_err(|e| {
                        // TODO(mkaput): Send a notification to the language client.
                        warn!("{e:?}");
                    })
                    .ok();

                if let Some(metadata) = metadata {
                    update_crate_roots(&metadata, db);
                } else {
                    // Try to set up a corelib at least.
                    try_to_init_unmanaged_core(db, config, scarb_toolchain);
                }

                if let Err(result) = validate_corelib(db) {
                    notifier.notify::<CorelibVersionMismatch>(result.to_string());
                }
            }

            Some(ProjectManifestPath::CairoProject(config_path)) => {
                // The base path of ProjectConfig must be absolute to ensure that all paths in Salsa
                // DB will also be absolute.
                assert!(config_path.is_absolute());

                try_to_init_unmanaged_core(db, config, scarb_toolchain);

                if let Ok(config) = ProjectConfig::from_file(&config_path) {
                    update_crate_roots_from_project_config(db, &config);
                };
            }

            None => {
                try_to_init_unmanaged_core(db, config, scarb_toolchain);

                if let Err(err) = setup_project(&mut *db, file_path) {
                    let file_path_s = file_path.to_string_lossy();
                    error!("error loading file {file_path_s} as a single crate: {err}");
                }
            }
        }
    }

    /// Reload crate detection for all open files.
    #[tracing::instrument(level = "trace", skip_all)]
    fn reload(
        state: &mut State,
        notifier: &Notifier,
        requester: &mut Requester<'_>,
    ) -> LSPResult<()> {
        Backend::reload_config(state, requester)?;

        for uri in state.open_files.iter() {
            let Some(file_id) = state.db.file_for_url(uri) else { continue };
            if let FileLongId::OnDisk(file_path) = state.db.lookup_intern_file(file_id) {
                Backend::detect_crate_for(
                    &mut state.db,
                    &state.scarb_toolchain,
                    &state.config,
                    &file_path,
                    notifier,
                );
            }
        }

        Ok(())
    }

    /// Reload the [`Config`] and all its dependencies.
    fn reload_config(state: &mut State, requester: &mut Requester<'_>) -> LSPResult<()> {
        state.config.reload(requester, &state.client_capabilities)
    }
}
