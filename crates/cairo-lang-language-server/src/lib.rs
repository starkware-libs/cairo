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

use std::panic::RefUnwindSafe;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::time::SystemTime;
use std::{io, panic};

use anyhow::{Context, Result};
use cairo_lang_compiler::db::validate_corelib;
use cairo_lang_compiler::project::{setup_project, update_crate_roots_from_project_config};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::FileLongId;
use cairo_lang_project::ProjectConfig;
use cairo_lang_semantic::plugin::PluginSuite;
use lsp_server::Message;
use lsp_types::RegistrationParams;
use salsa::{Database, Durability};
use tracing::{debug, error, info, warn};

use crate::config::Config;
use crate::lang::db::AnalysisDatabase;
use crate::lang::lsp::LsProtoGroup;
use crate::lsp::capabilities::server::{
    collect_dynamic_registrations, collect_server_capabilities,
};
use crate::lsp::ext::{CorelibVersionMismatch, ScarbMetadataFailed};
use crate::lsp::result::LSPResult;
use crate::project::ProjectManifestPath;
use crate::project::scarb::update_crate_roots;
use crate::project::unmanaged_core_crate::try_to_init_unmanaged_core;
use crate::server::client::{Notifier, Requester, Responder};
use crate::server::connection::{Connection, ConnectionInitializer};
use crate::server::panic::is_cancelled;
use crate::server::schedule::thread::JoinHandle;
use crate::server::schedule::{Scheduler, Task, event_loop_thread};
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
    set_panic_hook();

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
    use tracing_subscriber::filter::{EnvFilter, LevelFilter, Targets};
    use tracing_subscriber::fmt::Layer;
    use tracing_subscriber::fmt::time::Uptime;
    use tracing_subscriber::prelude::*;

    let mut guard = None;

    let fmt_layer = Layer::new()
        .with_writer(io::stderr)
        .with_timer(Uptime::default())
        .with_ansi(io::stderr().is_terminal())
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

        // Filter out less important Salsa logs because they are too verbose,
        // and with them the profile file quickly grows to several GBs of data.
        let profile_layer = profile_layer.with_filter(
            Targets::new().with_default(LevelFilter::TRACE).with_target("salsa", LevelFilter::WARN),
        );

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

/// Sets a special panic hook that skips execution for Salsa cancellation panics.
fn set_panic_hook() {
    let previous_hook = panic::take_hook();
    panic::set_hook(Box::new(move |info| {
        if !is_cancelled(info.payload()) {
            previous_hook(info);
        }
    }))
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
            BackendForTesting(Backend::initialize(tricks, connection_initializer).unwrap())
        });

        (init, client)
    }

    pub fn run_for_tests(self) -> Result<JoinHandle<Result<()>>> {
        self.0.run()
    }
}

impl Backend {
    fn new(tricks: Tricks) -> Result<Self> {
        let connection_initializer = ConnectionInitializer::stdio();

        Self::initialize(tricks, connection_initializer)
    }

<<<<<<< HEAD
    /// Initializes the connection and crate a ready to run [`Backend`] instance.
    ///
    /// As part of the initialization flow, this function exchanges client and server capabilities.
    fn initialize(tricks: Tricks, connection_initializer: ConnectionInitializer) -> Result<Self> {
        let (id, init_params) = connection_initializer.initialize_start()?;
||||||| 14b1d8c15
    // TODO(spapini): Consider managing vfs in a different way, using the
    // client.send_notification::<UpdateVirtualFile> call.

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
=======
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
>>>>>>> origin/dev-v2.8.5

        let client_capabilities = init_params.capabilities;
        let server_capabilities = collect_server_capabilities(&client_capabilities);

        let connection = connection_initializer.initialize_finish(id, server_capabilities)?;
        let state = State::new(connection.make_sender(), client_capabilities, tricks);

        Ok(Self { connection, state })
    }

    /// Runs the main event loop thread and wait for its completion.
    fn run(self) -> Result<JoinHandle<Result<()>>> {
        event_loop_thread(move || {
            let Self { mut state, connection } = self;

            let mut scheduler = Scheduler::new(&mut state, connection.make_sender());

            Self::dispatch_setup_tasks(&mut scheduler);

            // Attempt to swap the database to reduce memory use.
            // Because diagnostics are always refreshed afterwards, the fresh database state will
            // be quickly repopulated.
            scheduler.on_sync_task(Self::maybe_swap_database);

            // Refresh diagnostics each time state changes.
            // Although it is possible to mutate state without affecting the analysis database,
            // we basically never hit such a case in CairoLS in happy paths.
            scheduler.on_sync_task(Self::refresh_diagnostics);

            let result = Self::event_loop(&connection, scheduler);

            // Trigger cancellation in any background tasks that might still be running.
            state.db.salsa_runtime_mut().synthetic_write(Durability::LOW);

            if let Err(err) = connection.close() {
                error!("failed to close connection to the language server: {err:?}");
            }

            result
        })
    }

    /// Runs various setup tasks before entering the main event loop.
    fn dispatch_setup_tasks(scheduler: &mut Scheduler<'_>) {
        scheduler.local(Self::register_dynamic_capabilities);

        scheduler.local(|state, _notifier, requester, _responder| {
            let _ = state.config.reload(requester, &state.client_capabilities);
        });
    }

    fn register_dynamic_capabilities(
        state: &mut State,
        _notifier: Notifier,
        requester: &mut Requester<'_>,
        _responder: Responder,
    ) {
        let registrations = collect_dynamic_registrations(&state.client_capabilities);

        let _ = requester
            .request::<lsp_types::request::RegisterCapability>(
                RegistrationParams { registrations },
                |()| {
                    debug!("configuration file watcher successfully registered");
                    Task::nothing()
                },
            )
            .inspect_err(|e| {
                error!(
                    "failed to register dynamic capabilities, some features may not work \
                     properly: {e:?}"
                )
            });
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

    /// Calls [`lang::db::AnalysisDatabaseSwapper::maybe_swap`] to do its work.
    fn maybe_swap_database(state: &mut State, notifier: Notifier) {
        state.db_swapper.maybe_swap(
            &mut state.db,
            &state.open_files,
            &state.config,
            &state.tricks,
            &notifier,
        );
    }

    /// Calls [`lang::diagnostics::DiagnosticsController::refresh`] to do its work.
    fn refresh_diagnostics(state: &mut State, notifier: Notifier) {
        state.diagnostics_controller.refresh(state.snapshot(), notifier);
    }

    /// Tries to detect the crate root the config that contains a cairo file, and add it to the
    /// system.
    #[tracing::instrument(skip_all)]
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
                    .inspect_err(|err| {
                        warn!("{err:?}");
                        notifier.notify::<ScarbMetadataFailed>(());
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
    fn reload(
        state: &mut State,
        notifier: &Notifier,
        requester: &mut Requester<'_>,
    ) -> LSPResult<()> {
        state.config.reload(requester, &state.client_capabilities)?;

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
}
