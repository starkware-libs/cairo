use std::collections::HashSet;

use cairo_lang_defs::ids::ModuleId;
use cairo_lang_filesystem::ids::FileId;
use lsp_types::Url;
use lsp_types::notification::PublishDiagnostics;

use crate::lang::db::AnalysisDatabase;
use crate::lang::diagnostics::file_diagnostics::FileDiagnostics;
use crate::lang::diagnostics::project_diagnostics::ProjectDiagnostics;
use crate::lang::lsp::LsProtoGroup;
use crate::server::client::Notifier;

/// Refresh diagnostics and send diffs to the client.
#[tracing::instrument(skip_all)]
pub fn refresh_diagnostics(
    db: &AnalysisDatabase,
    batch: Vec<FileId>,
    trace_macro_diagnostics: bool,
    project_diagnostics: ProjectDiagnostics,
    notifier: Notifier,
) {
    let mut processed_modules: HashSet<ModuleId> = HashSet::default();

    for file in batch {
        refresh_file_diagnostics(
            db,
            file,
            trace_macro_diagnostics,
            &mut processed_modules,
            &project_diagnostics,
            &notifier,
        );
    }
}

/// Refresh diagnostics for a single file.
#[tracing::instrument(skip_all, fields(url = tracing_file_url(db, file)))]
fn refresh_file_diagnostics(
    db: &AnalysisDatabase,
    file: FileId,
    trace_macro_diagnostics: bool,
    processed_modules: &mut HashSet<ModuleId>,
    project_diagnostics: &ProjectDiagnostics,
    notifier: &Notifier,
) {
    let Some(new_file_diagnostics) = FileDiagnostics::collect(db, file, processed_modules) else {
        return;
    };

    if project_diagnostics.insert(new_file_diagnostics.clone()) {
        if let Some(params) = new_file_diagnostics.to_lsp(db, trace_macro_diagnostics) {
            notifier.notify::<PublishDiagnostics>(params);
        }
    }
}

/// Wipes diagnostics for any files not present in the preserve set.
#[tracing::instrument(skip_all)]
pub fn clear_old_diagnostics(
    db: &AnalysisDatabase,
    files_to_preserve: HashSet<Url>,
    project_diagnostics: ProjectDiagnostics,
    notifier: Notifier,
) {
    let removed = project_diagnostics.clear_old(&files_to_preserve);
    for mut file_diagnostics in removed {
        // It might be that we are removing a file that actually had some diagnostics.
        // For example, this might happen if a file with a syntax error is deleted.
        // We are reusing just removed `FileDiagnostics` instead of constructing a fresh one
        // to preserve any extra state it might contain.
        file_diagnostics.clear();

        // We can safely assume `trace_macro_diagnostics` = false here, as we are explicitly
        // sending a "no diagnostics" message.
        if let Some(params) = file_diagnostics.to_lsp(db, false) {
            notifier.notify::<PublishDiagnostics>(params);
        }
    }
}

fn tracing_file_url(db: &AnalysisDatabase, file: FileId) -> String {
    db.url_for_file(file).map(|u| u.to_string()).unwrap_or_default()
}
