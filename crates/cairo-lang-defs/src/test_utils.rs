use cairo_lang_diagnostics::{DiagnosticEntry, DiagnosticLocation, DiagnosticsBuilder, Severity};
use itertools::Itertools;

use crate::db::DefsGroup;
use crate::diagnostic_utils::StableLocation;
use crate::ids::ModuleId;
use crate::plugin::PluginDiagnostic;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TestDiagnosticEntry(pub PluginDiagnostic);
impl DiagnosticEntry for TestDiagnosticEntry {
    type DbType = dyn DefsGroup;
    fn format(&self, _db: &Self::DbType) -> String {
        self.0.message.clone()
    }
    fn location(&self, db: &Self::DbType) -> DiagnosticLocation {
        StableLocation::new(self.0.stable_ptr).diagnostic_location(db)
    }
    fn severity(&self) -> Severity {
        self.0.severity
    }
    fn is_same_kind(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

pub(crate) fn build_plugin_diagnostics(
    db: &(dyn DefsGroup + 'static),
    module_id: ModuleId,
) -> String {
    let mut builder = DiagnosticsBuilder::default();
    for (_, diagnostic) in db.module_plugin_diagnostics(module_id).unwrap().iter() {
        builder.add(TestDiagnosticEntry(diagnostic.clone()));
    }
    let diagnostics = builder.build();
    let file_notes = Default::default();
    let formatted = diagnostics.format_with_severity(db, &file_notes);
    formatted.into_iter().map(|diag| diag.to_string()).join("\n")
}
