use std::collections::HashMap;
use std::panic::{AssertUnwindSafe, catch_unwind};

use lsp_types::Url;
use tracing::{error, trace};

use crate::lang::diagnostics::refresh::refresh_diagnostics;
use crate::server::client::Notifier;
use crate::server::panic::is_cancelled;
use crate::server::schedule::thread::{self, JoinHandle, ThreadPriority};
use crate::server::sync::trigger::{Actuator, Trigger, trigger};
use crate::state::{FileDiagnostics, StateSnapshot};

mod lsp;
mod refresh;

/// Schedules refreshing of diagnostics in a background thread.
///
/// This structure *owns* the worker thread and is responsible for its lifecycle.
/// Dropping it will ask the worker to stop and synchronously wait for it to finish.
pub struct DiagnosticsController {
    // NOTE: The trigger MUST be dropped before worker's join handle.
    //   Otherwise, the worker will never be requested to stop.
    trigger: Trigger<WorkerArgs>,
    _worker: JoinHandle,
}

struct WorkerArgs {
    state: StateSnapshot,
    notifier: Notifier,
}

impl DiagnosticsController {
    /// Creates a new diagnostics controller.
    pub fn new() -> Self {
        let (trigger, actuator) = trigger();

        let worker = thread::Builder::new(ThreadPriority::Worker)
            .name("cairo-ls:diagnostics_controller".into())
            .spawn(move || Self::control_loop(actuator))
            .expect("failed to spawn diagnostics controller thread");

        Self { trigger, _worker: worker }
    }

    /// Schedules diagnostics refreshing using current state snapshot.
    pub fn refresh(&self, state: StateSnapshot, notifier: Notifier) {
        self.trigger.activate(WorkerArgs { state, notifier });
    }

    /// Runs diagnostics controller's event loop.
    fn control_loop(actuator: Actuator<WorkerArgs>) {
        let mut file_diagnostics = HashMap::<Url, FileDiagnostics>::new();

        while let Some(WorkerArgs { state, notifier }) = actuator.wait() {
            if let Err(err) = catch_unwind(AssertUnwindSafe(|| {
                refresh_diagnostics(
                    &state.db,
                    &state.open_files,
                    state.config.trace_macro_diagnostics,
                    &mut file_diagnostics,
                    notifier,
                );
            })) {
                if is_cancelled(&err) {
                    trace!("diagnostics refreshing has been cancelled");
                } else {
                    error!("caught panic while refreshing diagnostics");
                }
            }
        }
    }
}
