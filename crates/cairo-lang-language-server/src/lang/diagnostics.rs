use std::collections::{HashMap, HashSet};
use std::num::NonZeroUsize;
use std::ops::Add;

use anyhow::Result;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::FileLongId;
use cairo_lang_utils::LookupIntern;
use crossbeam::channel::Sender;
use file::FileBothFormats;
use lsp_types::Url;
use tracing::{error, trace_span, warn};

use self::force_write_buffer::ForceWriteBuffer;
use self::notifier::NotifierExt;
use self::state::{
    DiagnosticsState, FileDiagnostics, FileDiagnosticsChange, StateDiff,
    StateSnapshotForDiagnostics,
};
use crate::lang::db::AnalysisDatabase;
use crate::lang::lsp::LsProtoGroup;
use crate::server::client::{Notifier, Responder};
use crate::server::panic::{catch_cancellation, UnwindErrorKind};
use crate::server::schedule::thread;
use crate::server::schedule::thread::ThreadPriority;
use crate::state::State;

const WORKERS_THREAD_POOL_SIZE: usize = 4;
const _: () = assert!(WORKERS_THREAD_POOL_SIZE != 0);

mod file;
mod force_write_buffer;
mod lsp;
mod notifier;
mod state;

pub struct Diagnostics {
    state: DiagnosticsState,
    notifier: Notifier,
    thread_pool: thread::Pool,
}

impl Diagnostics {
    pub fn tasks() -> Result<(impl Fn(&mut State, Notifier), impl FnOnce(Notifier, Responder))> {
        let buffer = ForceWriteBuffer::new();

        let diagnostics_main_job = {
            let buffer = buffer.clone();

            move |notifier, _responder| {
                let mut diagnostics = Self::new(notifier);

                loop {
                    diagnostics.refresh(buffer.pop());
                }
            }
        };

        let diagnostics_post_hook = move |state: &mut State, _notifier| {
            buffer.force_push(StateSnapshotForDiagnostics::from_state(state));
        };

        Ok((diagnostics_post_hook, diagnostics_main_job))
    }

    fn new(notifier: Notifier) -> Self {
        Self {
            state: Default::default(),
            thread_pool: thread::Pool::new(NonZeroUsize::new(WORKERS_THREAD_POOL_SIZE).unwrap()),
            notifier,
        }
    }

    /// Refresh diagnostics and send diffs to the client.
    #[tracing::instrument(level = "debug", skip_all)]
    fn refresh(&mut self, message: StateSnapshotForDiagnostics) {
        let Ok((open_files, rest_of_files)) = catch_cancellation(|| {
            Self::get_files(
                &message.db_snapshots[0], // Safe because there is always more than 0 workers.
                message.open_files.owned(),
            )
        }) else {
            // [`salsa`] failure while preparing state for workers.
            // Probably very fast cancellation, skip further work as it will fail anyway.
            return;
        };

        let (sender, receiver) = crossbeam::channel::bounded(WORKERS_THREAD_POOL_SIZE);

        for (worker, db) in message.db_snapshots.into_iter().enumerate() {
            let files = Self::worker_files_partition(
                open_files.iter().cloned(),
                rest_of_files.iter().cloned(),
                worker,
            );

            self.spawn_worker_tasks(files, db, message.trace_macro_diagnostics, sender.clone());
        }

        // For some reason rx is not disconnected after all threads completed.
        let state_diff =
            receiver.into_iter().take(WORKERS_THREAD_POOL_SIZE).reduce(Add::add).unwrap();

        // All db snapshots should be dropped at this point.

        self.apply_state_diff(state_diff);
    }

    fn get_files(
        db: &AnalysisDatabase,
        open_files: HashSet<Url>,
    ) -> (Vec<FileBothFormats>, HashSet<FileBothFormats>) {
        let mut rest_of_files = HashSet::new();
        let open_files = trace_span!("get_open_files_ids").in_scope(|| {
            open_files
                .into_iter()
                .filter_map(|url| db.file_for_url(&url).map(|file| FileBothFormats::new(file, url)))
                .collect::<Vec<_>>()
        });

        for crate_id in db.crates() {
            for module_id in db.crate_modules(crate_id).iter() {
                if let Ok(module_files) = db.module_files(*module_id) {
                    let unprocessed_files = module_files
                        .iter()
                        .copied()
                        .filter(|file_id| {
                            matches!(file_id.lookup_intern(db), FileLongId::OnDisk(_))
                        })
                        .map(|file| FileBothFormats::new(file, db.url_for_file(file)));

                    rest_of_files.extend(unprocessed_files);
                }
            }
        }

        // Remove open files from rest of files.
        for file in &open_files {
            rest_of_files.remove(file);
        }

        (open_files, rest_of_files)
    }

    fn worker_files_partition(
        open_files: impl Iterator<Item = FileBothFormats>,
        rest_of_files: impl Iterator<Item = FileBothFormats>,
        worker: usize,
    ) -> Vec<FileBothFormats> {
        // Important: keep open files first so workers execute them at first too.
        open_files
            .chain(rest_of_files)
            .enumerate()
            .filter(move |(i, _file)| i % WORKERS_THREAD_POOL_SIZE == worker)
            .map(|(_, file)| file)
            .collect()
    }

    fn previous_generation_file_diagnostics(
        &self,
        files: impl Iterator<Item = FileBothFormats>,
    ) -> HashMap<Url, FileDiagnostics> {
        files
            .filter_map(|file| {
                self.state
                    .diagnostics_for_file
                    .get(&file.url)
                    .map(|diags| (file.url, diags.clone()))
            })
            .collect()
    }

    fn spawn_worker_tasks(
        &mut self,
        files: Vec<FileBothFormats>,
        db: salsa::Snapshot<AnalysisDatabase>,
        trace_macro_diagnostics: bool,
        sender: Sender<StateDiff>,
    ) {
        let notifier = self.notifier.clone();
        let file_diagnostics = self.previous_generation_file_diagnostics(files.iter().cloned());

        self.thread_pool.spawn(ThreadPriority::Worker, move || {
            let mut diff = StateDiff::new(files.iter().map(|file| file.url.clone()));

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
                        UnwindErrorKind::Canceled => {
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

            sender.send(diff).ok();
        });
    }

    #[tracing::instrument(skip_all)]
    fn apply_state_diff(&mut self, mut state_diff: StateDiff) {
        self.state.diagnostics_for_file.retain(|url, old_diags| {
            match state_diff.consume(url) {
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
