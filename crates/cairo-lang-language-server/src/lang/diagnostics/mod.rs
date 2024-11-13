use std::collections::HashMap;
use std::panic::{AssertUnwindSafe, catch_unwind};

use lsp_types::Url;
use tracing::{error, trace};

use self::file_diagnostics::FileDiagnostics;
use self::refresh::refresh_diagnostics;
use self::trigger::trigger;
use crate::lang::diagnostics::file_batches::{batches, find_primary_files, find_secondary_files};
use crate::server::client::Notifier;
use crate::server::panic::cancelled_anyhow;
use crate::server::schedule::thread::{self, JoinHandle, ThreadPriority};
use crate::state::{State, StateSnapshot};

mod file_batches;
mod file_diagnostics;
mod lsp;
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
    trigger: trigger::Sender<StateSnapshot>,
    _thread: JoinHandle,
}

impl DiagnosticsController {
    /// Creates a new diagnostics controller.
    pub fn new(notifier: Notifier) -> Self {
        let (trigger, receiver) = trigger();
        let thread = DiagnosticsControllerThread::spawn(receiver, notifier);
        Self { trigger, _thread: thread }
    }

    /// Schedules diagnostics refreshing on snapshot(s) of the current state.
    pub fn refresh(&self, state: &State) {
        self.trigger.activate(state.snapshot());
    }
}

/// Stores entire state of diagnostics controller's worker thread.
struct DiagnosticsControllerThread {
    receiver: trigger::Receiver<StateSnapshot>,
    notifier: Notifier,
    // NOTE: Globally, we have to always identify files by URL instead of FileId,
    //   as the diagnostics state is independent of analysis database swaps,
    //   which invalidate FileIds.
    file_diagnostics: HashMap<Url, FileDiagnostics>,
}

impl DiagnosticsControllerThread {
    /// Spawns a new diagnostics controller worker thread.
    fn spawn(receiver: trigger::Receiver<StateSnapshot>, notifier: Notifier) -> JoinHandle {
        let mut this = Self { receiver, notifier, file_diagnostics: Default::default() };

        thread::Builder::new(ThreadPriority::Worker)
            .name("cairo-ls:diagnostics-controller".into())
            .spawn(move || this.event_loop())
            .expect("failed to spawn diagnostics controller thread")
    }

    /// Runs diagnostics controller's event loop.
    fn event_loop(&mut self) {
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
    fn diagnostics_controller_tick(&mut self, state: StateSnapshot) {
        // TODO(mkaput): Make multiple batches and run them in parallel.
        let primary_files = find_primary_files(&state.db, &state.open_files);
        let secondary_files = find_secondary_files(&state.db, &primary_files);
        let files = primary_files.into_iter().chain(secondary_files).collect::<Vec<_>>();
        for batch in batches(&files, 1.try_into().unwrap()) {
            refresh_diagnostics(
                &state.db,
                batch,
                state.config.trace_macro_diagnostics,
                &mut self.file_diagnostics,
                self.notifier.clone(),
            );
        }
    }
}
