use defs::diagnostic_utils::StableLocation;
use diagnostics::{DiagnosticEntry, DiagnosticLocation};

use crate::db::SierraGenGroup;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct SierraGeneratorDiagnostic {
    pub stable_location: StableLocation,
    pub kind: SierraGeneratorDiagnosticKind,
}
impl DiagnosticEntry for SierraGeneratorDiagnostic {
    type DbType = dyn SierraGenGroup;

    fn format(&self, _db: &Self::DbType) -> String {
        panic!("DiagnosticEntry::format() is not implemented.");
    }

    fn location(&self, db: &Self::DbType) -> DiagnosticLocation {
        self.stable_location.diagnostic_location(db.upcast())
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum SierraGeneratorDiagnosticKind {}
