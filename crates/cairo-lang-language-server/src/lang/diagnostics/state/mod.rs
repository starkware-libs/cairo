use std::collections::{HashMap, HashSet};

use cairo_lang_lowering::diagnostic::LoweringDiagnostic;
use cairo_lang_parser::ParserDiagnostic;
use cairo_lang_semantic::SemanticDiagnostic;
use cairo_lang_utils::Upcast;
use lsp_types::Url;
use salsa::ParallelDatabase;

pub use self::diff::StateDiff;
use crate::lang::db::AnalysisDatabase;
use crate::lang::diagnostics::lsp::map_cairo_diagnostics_to_lsp;
use crate::state::{Snapshot, State};

mod diff;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct FileDiagnostics {
    parser: cairo_lang_diagnostics::Diagnostics<ParserDiagnostic>,
    semantic: cairo_lang_diagnostics::Diagnostics<SemanticDiagnostic>,
    lowering: cairo_lang_diagnostics::Diagnostics<LoweringDiagnostic>,
}

impl FileDiagnostics {
    pub fn is_empty(&self) -> bool {
        self.semantic.is_empty() && self.lowering.is_empty() && self.parser.is_empty()
    }

    pub fn new(
        parser: cairo_lang_diagnostics::Diagnostics<ParserDiagnostic>,
        semantic: impl IntoIterator<Item = SemanticDiagnostic>,
        lowering: impl IntoIterator<Item = LoweringDiagnostic>,
    ) -> Self {
        Self {
            parser,
            semantic: cairo_lang_diagnostics::Diagnostics::from_iter(semantic),
            lowering: cairo_lang_diagnostics::Diagnostics::from_iter(lowering),
        }
    }

    pub fn to_lsp(
        &self,
        db: &AnalysisDatabase,
        trace_macro_diagnostics: bool,
    ) -> Vec<lsp_types::Diagnostic> {
        let mut diags = Vec::new();

        map_cairo_diagnostics_to_lsp(
            (*db).upcast(),
            &mut diags,
            &self.parser,
            trace_macro_diagnostics,
        );
        map_cairo_diagnostics_to_lsp(
            (*db).upcast(),
            &mut diags,
            &self.semantic,
            trace_macro_diagnostics,
        );
        map_cairo_diagnostics_to_lsp(
            (*db).upcast(),
            &mut diags,
            &self.lowering,
            trace_macro_diagnostics,
        );

        diags
    }
}

#[derive(Debug, Default)]
pub struct DiagnosticsState {
    pub diagnostics_for_file: HashMap<Url, FileDiagnostics>,
}

pub struct DbSnapshots(Vec<salsa::Snapshot<AnalysisDatabase>>);

impl IntoIterator for DbSnapshots {
    type IntoIter = std::vec::IntoIter<Self::Item>;
    type Item = salsa::Snapshot<AnalysisDatabase>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl DbSnapshots {
    pub fn db_ref(&self) -> &AnalysisDatabase {
        // Safe because there is always more than 0 workers.
        &self.0[0]
    }
}

pub struct StateSnapshotForDiagnostics {
    pub db_snapshots: DbSnapshots,
    pub open_files: Snapshot<HashSet<Url>>,
    pub trace_macro_diagnostics: bool,
}

impl StateSnapshotForDiagnostics {
    pub fn from_state(state: &mut State, workers_num: usize) -> Self {
        Self {
            db_snapshots: DbSnapshots((0..workers_num).map(|_| state.db.snapshot()).collect()),
            open_files: state.open_files.snapshot(),
            trace_macro_diagnostics: state.config.trace_macro_diagnostics,
        }
    }
}

#[derive(Debug)]
pub enum FileDiagnosticsChange {
    Replaced(FileDiagnostics),
    Unchanged,
    Unset,
}
