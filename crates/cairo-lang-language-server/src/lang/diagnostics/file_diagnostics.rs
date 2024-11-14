use std::collections::HashSet;
use std::panic::{AssertUnwindSafe, catch_unwind, resume_unwind};

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_diagnostics::Diagnostics;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::diagnostic::LoweringDiagnostic;
use cairo_lang_parser::ParserDiagnostic;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_semantic::SemanticDiagnostic;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_utils::{LookupIntern, Upcast};
use tracing::{error, info_span};

use crate::lang::db::AnalysisDatabase;
use crate::lang::diagnostics::lsp::map_cairo_diagnostics_to_lsp;
use crate::lang::lsp::LsProtoGroup;
use crate::server::panic::is_cancelled;

/// Result of refreshing diagnostics for a single file.
///
/// ## Comparisons
///
/// Diagnostics in this structure are stored as Arcs that directly come from Salsa caches.
/// This means that equality comparisons of `FileDiagnostics` are efficient.
///
/// ## Virtual files
///
/// When collecting diagnostics using [`FileDiagnostics::collect`], all virtual files related
/// to the given `file` will also be visited and their diagnostics collected.
#[derive(Clone, PartialEq, Eq)]
pub struct FileDiagnostics {
    /// The file ID these diagnostics are associated with.
    pub file: FileId,
    pub parser: Diagnostics<ParserDiagnostic>,
    pub semantic: Diagnostics<SemanticDiagnostic>,
    pub lowering: Diagnostics<LoweringDiagnostic>,
}

impl FileDiagnostics {
    /// Collects all diagnostics kinds from the given `file` and constructs a new `FileDiagnostics`.
    ///
    /// The `processed_modules` in/out parameter is used to avoid constructing two overlapping
    /// `FileDiagnostics` in a single batch.
    /// Any [`ModuleId`] present in this collection will be skipped (leaving empty diagnostics in
    /// constructed `FileDiagnostics`), and new module IDs will be added.
    pub fn collect(
        db: &AnalysisDatabase,
        file: FileId,
        processed_modules: &mut HashSet<ModuleId>,
    ) -> Option<Self> {
        macro_rules! query {
            ($query:expr) => {
                info_span!(stringify!($query)).in_scope(|| {
                    catch_unwind(AssertUnwindSafe(|| $query)).map_err(|err| {
                        if is_cancelled(err.as_ref()) {
                            resume_unwind(err);
                        } else {
                            error!(
                                "caught panic when computing diagnostics for file: {:?}",
                                file.lookup_intern(db)
                            );
                            err
                        }
                    })
                })
            };
        }

        let module_ids = query!(db.file_modules(file)).ok()?.ok()?;

        let mut semantic_file_diagnostics: Vec<SemanticDiagnostic> = vec![];
        let mut lowering_file_diagnostics: Vec<LoweringDiagnostic> = vec![];

        for &module_id in module_ids.iter() {
            if !processed_modules.contains(&module_id) {
                semantic_file_diagnostics.extend(
                    query!(db.module_semantic_diagnostics(module_id))
                        .map(Result::unwrap_or_default)
                        .unwrap_or_default()
                        .get_all(),
                );
                lowering_file_diagnostics.extend(
                    query!(db.module_lowering_diagnostics(module_id))
                        .map(Result::unwrap_or_default)
                        .unwrap_or_default()
                        .get_all(),
                );

                processed_modules.insert(module_id);
            }
        }

        let parser_file_diagnostics = query!(db.file_syntax_diagnostics(file)).unwrap_or_default();

        Some(FileDiagnostics {
            file,
            parser: parser_file_diagnostics,
            semantic: Diagnostics::from_iter(semantic_file_diagnostics),
            lowering: Diagnostics::from_iter(lowering_file_diagnostics),
        })
    }

    /// Clears all diagnostics from this `FileDiagnostics`.
    pub fn clear(&mut self) {
        self.parser = Diagnostics::default();
        self.semantic = Diagnostics::default();
        self.lowering = Diagnostics::default();
    }

    /// Constructs a new [`lsp_types::PublishDiagnosticsParams`] from this `FileDiagnostics`.
    pub fn to_lsp(
        &self,
        db: &AnalysisDatabase,
        trace_macro_diagnostics: bool,
    ) -> Option<lsp_types::PublishDiagnosticsParams> {
        let uri = db.url_for_file(self.file)?;

        let mut diagnostics = Vec::new();
        map_cairo_diagnostics_to_lsp(
            (*db).upcast(),
            &mut diagnostics,
            &self.parser,
            self.file,
            trace_macro_diagnostics,
        );
        map_cairo_diagnostics_to_lsp(
            (*db).upcast(),
            &mut diagnostics,
            &self.semantic,
            self.file,
            trace_macro_diagnostics,
        );
        map_cairo_diagnostics_to_lsp(
            (*db).upcast(),
            &mut diagnostics,
            &self.lowering,
            self.file,
            trace_macro_diagnostics,
        );

        Some(lsp_types::PublishDiagnosticsParams { uri, diagnostics, version: None })
    }
}
