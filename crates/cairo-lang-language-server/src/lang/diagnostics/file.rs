use std::panic::{catch_unwind, AssertUnwindSafe};

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::diagnostic::LoweringDiagnostic;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::SemanticDiagnostic;
use lsp_types::Url;
use tracing::{error, trace_span};

use super::state::FileDiagnostics;
use crate::lang::db::AnalysisDatabase;

/// In LSP context [`Url`] is required and in [`salsa`], [`FileId`] but conversion is fallible as it
/// requires [`salsa`] query and snapshot can be already cancelled.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FileBothFormats {
    pub id: FileId,
    pub url: Url,
}

impl FileBothFormats {
    pub fn new(id: FileId, url: Url) -> Self {
        Self { id, url }
    }

    /// Refresh diagnostics for a single file.
    pub fn refresh_diagnostics(&self, db: &AnalysisDatabase) -> FileDiagnostics {
        let modules_ids = db.file_modules(self.id).unwrap_or_default();
        let mut semantic_file_diagnostics: Vec<SemanticDiagnostic> = vec![];
        let mut lowering_file_diagnostics: Vec<LoweringDiagnostic> = vec![];

        // TODO should we catch here? Currently cancellation can happen on next iteration
        // db.file_modules() only, we should not catch it here too.
        macro_rules! diags {
            ($db:ident. $query:ident($file_id:expr), $f:expr) => {
                trace_span!(stringify!($query)).in_scope(|| {
                    catch_unwind(AssertUnwindSafe(|| $db.$query($file_id)))
                        .map($f)
                        .inspect_err(|_| {
                            error!("caught panic when computing diagnostics for file {}", self.url);
                        })
                        .unwrap_or_default()
                })
            };
        }

        for module_id in modules_ids.iter() {
            semantic_file_diagnostics.extend(
                diags!(db.module_semantic_diagnostics(*module_id), Result::unwrap_or_default)
                    .get_all(),
            );
            lowering_file_diagnostics.extend(
                diags!(db.module_lowering_diagnostics(*module_id), Result::unwrap_or_default)
                    .get_all(),
            );
        }

        let parser_file_diagnostics = diags!(db.file_syntax_diagnostics(self.id), |r| r);

        FileDiagnostics::new(
            parser_file_diagnostics,
            semantic_file_diagnostics,
            lowering_file_diagnostics,
        )
    }
}
