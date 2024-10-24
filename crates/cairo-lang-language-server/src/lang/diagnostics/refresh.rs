use std::collections::{HashMap, HashSet};
use std::panic::{AssertUnwindSafe, catch_unwind, resume_unwind};

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_diagnostics::Diagnostics;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::diagnostic::LoweringDiagnostic;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_semantic::SemanticDiagnostic;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_utils::Upcast;
use lsp_types::notification::PublishDiagnostics;
use lsp_types::{PublishDiagnosticsParams, Url};
use tracing::{error, info_span};

use crate::lang::db::AnalysisDatabase;
use crate::lang::diagnostics::lsp::map_cairo_diagnostics_to_lsp;
use crate::lang::lsp::LsProtoGroup;
use crate::server::client::Notifier;
use crate::server::panic::is_cancelled;
use crate::state::FileDiagnostics;

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

    let open_files_modules = get_files_modules(db, open_files_ids.iter().copied());

    // Refresh open files modules first for better UX
    info_span!("refresh_open_files_modules").in_scope(|| {
        for (file, file_modules_ids) in open_files_modules {
            refresh_file_diagnostics(
                db,
                &file,
                &file_modules_ids,
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
                if let Ok(module_files) = db.module_files(*module_id) {
                    let unprocessed_files =
                        module_files.iter().filter(|file| !open_files_ids.contains(file));
                    rest_of_files.extend(unprocessed_files);
                }
            }
        }
        rest_of_files
    });

    let rest_of_files_modules = get_files_modules(db, rest_of_files.iter().copied());

    // Refresh the rest of files after, since they are not viewed currently
    info_span!("refresh_other_files_modules").in_scope(|| {
        for (file, file_modules_ids) in rest_of_files_modules {
            refresh_file_diagnostics(
                db,
                &file,
                &file_modules_ids,
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
#[expect(clippy::too_many_arguments)]
fn refresh_file_diagnostics(
    db: &AnalysisDatabase,
    file: &FileId,
    modules_ids: &Vec<ModuleId>,
    trace_macro_diagnostics: bool,
    processed_modules: &mut HashSet<ModuleId>,
    files_with_set_diagnostics: &mut HashSet<Url>,
    file_diagnostics: &mut HashMap<Url, FileDiagnostics>,
    notifier: &Notifier,
) {
    let file_uri = db.url_for_file(*file);
    let mut semantic_file_diagnostics: Vec<SemanticDiagnostic> = vec![];
    let mut lowering_file_diagnostics: Vec<LoweringDiagnostic> = vec![];

    macro_rules! diags {
        ($db:ident. $query:ident($file_id:expr), $f:expr) => {
            info_span!(stringify!($query)).in_scope(|| {
                catch_unwind(AssertUnwindSafe(|| $db.$query($file_id)))
                    .map($f)
                    .map_err(|err| {
                        if is_cancelled(&err) {
                            resume_unwind(err);
                        } else {
                            error!("caught panic when computing diagnostics for file {file_uri}");
                            err
                        }
                    })
                    .unwrap_or_default()
            })
        };
    }

    for module_id in modules_ids {
        if !processed_modules.contains(module_id) {
            semantic_file_diagnostics.extend(
                diags!(db.module_semantic_diagnostics(*module_id), Result::unwrap_or_default)
                    .get_all(),
            );
            lowering_file_diagnostics.extend(
                diags!(db.module_lowering_diagnostics(*module_id), Result::unwrap_or_default)
                    .get_all(),
            );

            processed_modules.insert(*module_id);
        }
    }

    let parser_file_diagnostics = diags!(db.file_syntax_diagnostics(*file), |r| r);

    let new_file_diagnostics = FileDiagnostics {
        parser: parser_file_diagnostics,
        semantic: Diagnostics::from_iter(semantic_file_diagnostics),
        lowering: Diagnostics::from_iter(lowering_file_diagnostics),
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

    let mut diags = Vec::new();
    map_cairo_diagnostics_to_lsp(
        (*db).upcast(),
        &mut diags,
        &new_file_diagnostics.parser,
        file,
        trace_macro_diagnostics,
    );
    map_cairo_diagnostics_to_lsp(
        (*db).upcast(),
        &mut diags,
        &new_file_diagnostics.semantic,
        file,
        trace_macro_diagnostics,
    );
    map_cairo_diagnostics_to_lsp(
        (*db).upcast(),
        &mut diags,
        &new_file_diagnostics.lowering,
        file,
        trace_macro_diagnostics,
    );

    notifier.notify::<PublishDiagnostics>(PublishDiagnosticsParams {
        uri: file_uri,
        diagnostics: diags,
        version: None,
    });
}

/// Gets the mapping of files to their respective modules.
#[tracing::instrument(skip_all)]
fn get_files_modules(
    db: &AnalysisDatabase,
    files_ids: impl Iterator<Item = FileId>,
) -> HashMap<FileId, Vec<ModuleId>> {
    let mut result = HashMap::default();
    for file_id in files_ids {
        if let Ok(file_modules) = db.file_modules(file_id) {
            result.insert(file_id, file_modules.iter().cloned().collect::<Vec<_>>());
        }
    }
    result
}
