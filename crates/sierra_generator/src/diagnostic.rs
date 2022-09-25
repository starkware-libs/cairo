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
        match self.kind {
            SierraGeneratorDiagnosticKind::NonZeroValueInMatch => {
                "Match with a non-zero value is not supported."
            }
            SierraGeneratorDiagnosticKind::CallLibFuncWithGenericArgs => {
                "Calling a libfunc with generic arguments is not supported yet."
            }
            SierraGeneratorDiagnosticKind::OnlyMatchZeroIsSupported => {
                "Only match zero (match ... { 0 => ..., _ => ... }) is currently supported."
            }
            SierraGeneratorDiagnosticKind::InternalErrorUnknownVariable => {
                "Internal compiler error: unknown variable."
            }
            SierraGeneratorDiagnosticKind::InternalErrorDuplicatedVariable => {
                r#"Internal compiler error: found two definitions for the same variable."#
            }
        }
        .into()
    }

    fn location(&self, db: &Self::DbType) -> DiagnosticLocation {
        self.stable_location.diagnostic_location(db.upcast())
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum SierraGeneratorDiagnosticKind {
    // TODO(lior): Remove once supported.
    NonZeroValueInMatch,
    CallLibFuncWithGenericArgs,
    // TODO(lior): Remove once supported.
    OnlyMatchZeroIsSupported,
    InternalErrorUnknownVariable,
    InternalErrorDuplicatedVariable,
}
