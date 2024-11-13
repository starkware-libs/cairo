use std::collections::{HashMap, HashSet};

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_utils::LookupIntern;
use lsp_types::notification::PublishDiagnostics;
use lsp_types::{PublishDiagnosticsParams, Url};
use tracing::{info_span, trace};

use crate::lang::db::AnalysisDatabase;
use crate::lang::diagnostics::file_diagnostics::FileDiagnostics;
use crate::lang::lsp::LsProtoGroup;
use crate::server::client::Notifier;

/// Refresh diagnostics and send diffs to the client.
#[tracing::instrument(skip_all)]
pub fn refresh_diagnostics(
    db: &AnalysisDatabase,
    open_files: &HashSet<Url>,
    trace_macro_diagnostics: bool,
    file_diagnostics: &mut HashMap<Url, FileDiagnostics>,
    notifier: Notifier,
) {
    let mut files_with_set_diagnostics: HashSet<Url> = HashSet::default();
    let mut processed_modules: HashSet<ModuleId> = HashSet::default();

    let open_files_ids = info_span!("get_open_files_ids").in_scope(|| {
        open_files.iter().filter_map(|uri| db.file_for_url(uri)).collect::<HashSet<FileId>>()
    });

    // Refresh open files modules first for better UX
    info_span!("refresh_open_files_modules").in_scope(|| {
        for &file in &open_files_ids {
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
    });

    let rest_of_files = info_span!("get_rest_of_files").in_scope(|| {
        let mut rest_of_files: HashSet<FileId> = HashSet::default();
        for crate_id in db.crates() {
            for module_id in db.crate_modules(crate_id).iter() {
                // Schedule only module main files for refreshing.
                // All other related files will be refreshed along with it in a single job.
                if let Ok(file) = db.module_main_file(*module_id) {
                    if !open_files_ids.contains(&file) {
                        rest_of_files.insert(file);
                    }
                }
            }
        }
        rest_of_files
    });

    // Refresh the rest of files after, since they are not viewed currently
    info_span!("refresh_other_files_modules").in_scope(|| {
        for file in rest_of_files {
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
    });

    // Clear old diagnostics
    info_span!("clear_old_diagnostics").in_scope(|| {
        let mut removed_files = Vec::new();

        file_diagnostics.retain(|uri, _| {
            let retain = files_with_set_diagnostics.contains(uri);
            if !retain {
                removed_files.push(uri.clone());
            }
            retain
        });

        for file in removed_files {
            notifier.notify::<PublishDiagnostics>(PublishDiagnosticsParams {
                uri: file,
                diagnostics: vec![],
                version: None,
            });
        }
    });
}

/// Refresh diagnostics for a single file.
fn refresh_file_diagnostics(
    db: &AnalysisDatabase,
    file: FileId,
    trace_macro_diagnostics: bool,
    processed_modules: &mut HashSet<ModuleId>,
    files_with_set_diagnostics: &mut HashSet<Url>,
    file_diagnostics: &mut HashMap<Url, FileDiagnostics>,
    notifier: &Notifier,
) {
    let Some(file_uri) = db.url_for_file(file) else {
        trace!("url for file not found: {:?}", file.lookup_intern(db));
        return;
    };

    let Some(new_file_diagnostics) = FileDiagnostics::collect(db, file, processed_modules) else {
        return;
    };

    if !new_file_diagnostics.is_empty() {
        files_with_set_diagnostics.insert(file_uri.clone());
    }

    // Since we are using Arcs, this comparison should be efficient.
    if let Some(old_file_diagnostics) = file_diagnostics.get(&file_uri) {
        if old_file_diagnostics == &new_file_diagnostics {
            return;
        }

        file_diagnostics.insert(file_uri.clone(), new_file_diagnostics.clone());
    };

    if let Some(params) = new_file_diagnostics.to_lsp(db, trace_macro_diagnostics) {
        notifier.notify::<PublishDiagnostics>(params);
    }
}
