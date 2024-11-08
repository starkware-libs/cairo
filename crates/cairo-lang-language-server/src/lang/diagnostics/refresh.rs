use std::collections::{HashMap, HashSet};

use cairo_lang_defs::ids::ModuleId;
use cairo_lang_filesystem::ids::FileId;
use lsp_types::notification::PublishDiagnostics;
use tracing::info_span;

use crate::lang::db::AnalysisDatabase;
use crate::lang::diagnostics::file_diagnostics::FileDiagnostics;
use crate::server::client::Notifier;

/// Refresh diagnostics and send diffs to the client.
#[tracing::instrument(skip_all)]
pub fn refresh_diagnostics(
    db: &AnalysisDatabase,
    batch: Vec<FileId>,
    trace_macro_diagnostics: bool,
    file_diagnostics: &mut HashMap<FileId, FileDiagnostics>,
    notifier: Notifier,
) {
    let mut files_with_set_diagnostics: HashSet<FileId> = HashSet::default();
    let mut processed_modules: HashSet<ModuleId> = HashSet::default();

    for file in batch {
        refresh_file_diagnostics(
            db,
            file,
            trace_macro_diagnostics,
            &mut processed_modules,
            &mut files_with_set_diagnostics,
            file_diagnostics,
            &notifier,
        );
    }

    info_span!("clear_old_diagnostics").in_scope(|| {
        let mut removed_files = Vec::new();

        file_diagnostics.retain(|&file, _| {
            let retain = files_with_set_diagnostics.contains(&file);
            if !retain {
                removed_files.push(file);
            }
            retain
        });

        for file in removed_files {
            let Some(params) = FileDiagnostics::empty(file).to_lsp(db, trace_macro_diagnostics)
            else {
                // This is a pathological branch which is not expected to happen.
                // Theoretically, it is fine to skip here because if we cannot make a URL for the
                // file, then we likely never have been able to publish any diagnostics for it in
                // the past.
                continue;
            };
            notifier.notify::<PublishDiagnostics>(params);
        }
    });
}

/// Refresh diagnostics for a single file.
fn refresh_file_diagnostics(
    db: &AnalysisDatabase,
    file: FileId,
    trace_macro_diagnostics: bool,
    processed_modules: &mut HashSet<ModuleId>,
    files_with_set_diagnostics: &mut HashSet<FileId>,
    file_diagnostics: &mut HashMap<FileId, FileDiagnostics>,
    notifier: &Notifier,
) {
    let Some(new_file_diagnostics) = FileDiagnostics::collect(db, file, processed_modules) else {
        return;
    };

    if !new_file_diagnostics.is_empty() {
        files_with_set_diagnostics.insert(file);
    }

    // Since we are using Arcs, this comparison should be efficient.
    if let Some(old_file_diagnostics) = file_diagnostics.get(&file) {
        if old_file_diagnostics == &new_file_diagnostics {
            return;
        }

        file_diagnostics.insert(file, new_file_diagnostics.clone());
    };

    if let Some(params) = new_file_diagnostics.to_lsp(db, trace_macro_diagnostics) {
        notifier.notify::<PublishDiagnostics>(params);
    }
}
