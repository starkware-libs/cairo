use std::ops::Add;

use crossbeam::channel::Sender;
use tracing::{error, warn};

use self::files_queue::FilesQueue;
use self::notifier::NotifierExt;
use self::slot::slot;
use self::state::{
    DiagnosticsState, FileDiagnosticsChange, StateDiff, StateSnapshotForDiagnostics,
};
use crate::ide::progress::Progress;
use crate::lang::db::AnalysisDatabase;
use crate::server::client::{Notifier, Responder};
use crate::server::panic::{catch_cancellation, UnwindErrorKind};
use crate::server::schedule::thread;
use crate::server::schedule::thread::ThreadPriority;
use crate::state::State;

mod file;
mod files_queue;
mod lsp;
mod notifier;
mod slot;
mod state;

pub struct Diagnostics {
    state: DiagnosticsState,
    notifier: Notifier,
    thread_pool: thread::SharedPool,
}

impl Diagnostics {
    pub fn tasks(
        thread_pool: thread::SharedPool,
    ) -> (impl Fn(&mut State, Notifier), impl FnOnce(Notifier, Responder)) {
        let (slot_reader, slot_writer) = slot(None);
        let jobs_number = Self::jobs_number(&thread_pool);

        let diagnostics_main_job = move |notifier, _responder| {
            let mut diagnostics = Self::new(notifier, thread_pool);

            while let Some(message) = slot_reader.pop() {
                diagnostics.refresh(message);
            }
        };

        let diagnostics_post_hook = move |state: &mut State, _notifier| {
            let message = StateSnapshotForDiagnostics::from_state(state, jobs_number);
            // TODO check if server is closing and send None to allow thread pool to be dropped.

            slot_writer.set(Some(message));
        };

        (diagnostics_post_hook, diagnostics_main_job)
    }

    fn new(notifier: Notifier, thread_pool: thread::SharedPool) -> Self {
        Self { state: Default::default(), thread_pool, notifier }
    }

    fn jobs_number(thread_pool: &thread::SharedPool) -> usize {
        let size = thread_pool.size();

        (size / 2).max(size - 2)
    }

    /// Refresh diagnostics and send diffs to the client.
    #[tracing::instrument(level = "debug", skip_all)]
    fn refresh(&mut self, message: StateSnapshotForDiagnostics) {
        let Ok(files) = catch_cancellation(|| {
            FilesQueue::new(message.db_snapshots.db_ref(), message.open_files.owned())
        }) else {
            // [`salsa`] failure while preparing state for workers.
            // Probably very fast cancellation, skip further work as it will fail anyway.
            return;
        };

        let jobs_number = Self::jobs_number(&self.thread_pool);
        let (sender, receiver) = crossbeam::channel::bounded(jobs_number);
        
        self.notifier.report_progress(
            "cairo/diagnostics",
            "Analyzing",
            Progress::Begin,
        );
        for (worker, db) in message.db_snapshots.into_iter().enumerate() {
            let files = files.worker_files_partition(worker, jobs_number);

            self.spawn_worker_tasks(files, db, message.trace_macro_diagnostics, sender.clone());
        }

        // For some reason rx is not disconnected after all threads completed.
        let state_diff = receiver.into_iter().take(jobs_number).reduce(Add::add).unwrap();

        // All db snapshots should be dropped at this point.
        self.notifier.report_progress(
            "cairo/diagnostics",
            "Analyzing",
            Progress::End,
        );
        self.apply_state_diff(state_diff);
    }

    fn spawn_worker_tasks(
        &mut self,
        files: FilesQueue,
        db: salsa::Snapshot<AnalysisDatabase>,
        trace_macro_diagnostics: bool,
        sender: Sender<StateDiff>,
    ) {
        let notifier = self.notifier.clone();
        let file_diagnostics =
            files.previous_generation_file_diagnostics(&self.state.diagnostics_for_file);

        self.thread_pool.spawn(ThreadPriority::Worker, move || {
            let mut diff = StateDiff::new(files.urls());
            
            for file in files {
                // Anything using salsa should be done in catch.
                let result = catch_cancellation(|| {
                    let new_file_diagnostics = file.refresh_diagnostics(&db);

                    if new_file_diagnostics.is_empty() {
                        diff.update_for(&file.url, FileDiagnosticsChange::Unset);

                        notifier.clear_diagnostics(file.url);
                    } else if file_diagnostics.get(&file.url) == Some(&new_file_diagnostics) {
                        diff.update_for(&file.url, FileDiagnosticsChange::Unchanged);

                        // No need to send same diagnostics twice.
                    } else {
                        notifier.publish_diagnostics(
                            file.url.clone(),
                            new_file_diagnostics.to_lsp(&db, trace_macro_diagnostics),
                        );

                        diff.update_for(
                            &file.url,
                            FileDiagnosticsChange::Replaced(new_file_diagnostics),
                        );
                    }
                });

                if let Err(err) = result {
                    diff.calculating_all_failed();

                    match err {
                        UnwindErrorKind::Canceled(_) => {
                            // Any further iteration will fail to this branch anyway.
                            // So no need to execute them.
                            break;
                        }
                        UnwindErrorKind::Other => {
                            error!("caught error while calculating diagnostics");
                        }
                    }
                }
            }

            sender.send(diff).unwrap();
        });
    }

    #[tracing::instrument(level = "trace", skip_all)]
    fn apply_state_diff(&mut self, mut state_diff: StateDiff) {
        self.state.diagnostics_for_file.retain(|url, old_diags| {
            match state_diff.remove(url) {
                Some(FileDiagnosticsChange::Replaced(diags)) => {
                    *old_diags = diags;

                    true
                }
                Some(FileDiagnosticsChange::Unchanged) => true,
                Some(FileDiagnosticsChange::Unset) => false,
                None => {
                    // Unset diagnostics for files that are no longer relevant only if we calculated
                    // all diagnostics (ie. no cancellation during this work
                    // happened) to make UX better.
                    if state_diff.calculated_all() {
                        self.notifier.clear_diagnostics(url.clone());
                    }

                    !state_diff.calculated_all()
                }
            }
        });

        // Files that were not previously tracked.
        for (file, diags) in state_diff {
            let FileDiagnosticsChange::Replaced(diags) = diags else {
                continue;
            };

            self.state.diagnostics_for_file.insert(file, diags);
        }
    }
}
