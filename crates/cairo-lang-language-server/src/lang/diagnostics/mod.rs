use std::collections::HashSet;
use std::iter::zip;
use std::panic::{AssertUnwindSafe, catch_unwind};

use lsp_types::Url;
use tracing::{error, trace};

use self::file_batches::make_file_batches;
use self::project_diagnostics::ProjectDiagnostics;
use self::refresh::{clear_old_diagnostics, refresh_diagnostics};
use self::trigger::trigger;
use crate::lang::lsp::LsProtoGroup;
use crate::server::client::Notifier;
use crate::server::panic::cancelled_anyhow;
use crate::server::schedule::thread::{self, JoinHandle, ThreadPriority};
use crate::state::{State, StateSnapshot};

mod file_batches;
mod file_diagnostics;
mod lsp;
mod project_diagnostics;
mod refresh;
mod trigger;

/// Schedules refreshing of diagnostics in a background thread.
///
/// This structure *owns* the worker thread and is responsible for its lifecycle.
/// Dropping it will ask the worker to stop and synchronously wait for it to finish.
pub struct DiagnosticsController {
    // NOTE: Member order matters here.
    //   The trigger MUST be dropped before worker's join handle.
    //   Otherwise, the controller thread will never be requested to stop, and the controller's
    //   JoinHandle will never terminate.
    trigger: trigger::Sender<Vec<StateSnapshot>>,
    _thread: JoinHandle,
    parallelism: usize,
}

impl DiagnosticsController {
    /// Creates a new diagnostics controller.
    pub fn new(notifier: Notifier) -> Self {
        let (trigger, receiver) = trigger();
        let (thread, parallelism) = DiagnosticsControllerThread::spawn(receiver, notifier);
        Self { trigger, _thread: thread, parallelism }
    }

    /// Schedules diagnostics refreshing on snapshot(s) of the current state.
    pub fn refresh(&self, state: &State) {
        // It is not possible to clone Salsa snapshots nor share one between threads,
        // thus we explicitly create separate snapshots for all threads involved in advance.
        //
        // We are adding one more snapshot to use by the controller & gc task.
        let state_snapshots = (0..(self.parallelism + 1)).map(|_| state.snapshot()).collect();
        self.trigger.activate(state_snapshots);
    }
}

/// Stores entire state of diagnostics controller's worker thread.
struct DiagnosticsControllerThread {
    receiver: trigger::Receiver<Vec<StateSnapshot>>,
    notifier: Notifier,
    workers: thread::Pool,
    project_diagnostics: ProjectDiagnostics,
}

impl DiagnosticsControllerThread {
    /// Spawns a new diagnostics controller worker thread
    /// and returns a handle to it and the amount of parallelism it provides.
    fn spawn(
        receiver: trigger::Receiver<Vec<StateSnapshot>>,
        notifier: Notifier,
    ) -> (JoinHandle, usize) {
        let this = Self {
            receiver,
            notifier,
            workers: thread::Pool::new(),
            project_diagnostics: ProjectDiagnostics::new(),
        };

        let parallelism = this.workers.parallelism().get();

        let thread = thread::Builder::new(ThreadPriority::Worker)
            .name("cairo-ls:diagnostics-controller".into())
            .spawn(move || this.event_loop())
            .expect("failed to spawn diagnostics controller thread");

        (thread, parallelism)
    }

    /// Runs diagnostics controller's event loop.
    fn event_loop(&self) {
        while let Some(state) = self.receiver.wait() {
            if let Err(err) = catch_unwind(AssertUnwindSafe(|| {
                self.diagnostics_controller_tick(state);
            })) {
                if let Ok(err) = cancelled_anyhow(err, "diagnostics refreshing has been cancelled")
                {
                    trace!("{err:?}");
                } else {
                    error!("caught panic while refreshing diagnostics");
                }
            }
        }
    }

    /// Runs a single tick of the diagnostics controller's event loop.
    #[tracing::instrument(skip_all)]
    fn diagnostics_controller_tick(&self, mut state_snapshots: Vec<StateSnapshot>) {
        assert_eq!(state_snapshots.len(), self.workers.parallelism().get() + 1);
        let state = state_snapshots.pop().expect("we just asserted that it exists");

        let batches = make_file_batches(&state.db, &state.open_files, self.workers.parallelism());
        let files_to_preserve: HashSet<Url> =
            batches.iter().flatten().flat_map(|&file| state.db.url_for_file(file)).collect();

        for (batch, state) in zip(batches, state_snapshots) {
            self.spawn_worker(move |project_diagnostics, notifier| {
                refresh_diagnostics(
                    &state.db,
                    batch,
                    state.config.trace_macro_diagnostics,
                    project_diagnostics,
                    notifier,
                );
            });
        }

        self.spawn_worker(move |project_diagnostics, notifier| {
            clear_old_diagnostics(&state.db, files_to_preserve, project_diagnostics, notifier);
        });
    }

    /// Shortcut for spawning a worker task which does the boilerplate around cloning state parts
    /// and catching panics.
    fn spawn_worker(&self, f: impl FnOnce(ProjectDiagnostics, Notifier) + Send + 'static) {
        let project_diagnostics = self.project_diagnostics.clone();
        let notifier = self.notifier.clone();
        let worker_fn = move || f(project_diagnostics, notifier);
        self.workers.spawn(ThreadPriority::Worker, move || {
            if let Err(err) = catch_unwind(AssertUnwindSafe(worker_fn)) {
                if let Ok(err) = cancelled_anyhow(err, "diagnostics worker has been cancelled") {
                    trace!("{err:?}");
                } else {
                    error!("caught panic in diagnostics worker");
                }
            }
        });
    }
}
