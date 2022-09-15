#[cfg(test)]
#[path = "diagnostic_test.rs"]
mod test;

use defs::diagnostic_utils::StableLocation;
use diagnostics::{DiagnosticEntry, DiagnosticLocation};

use crate::db::SemanticGroup;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct SemanticDiagnostic {
    pub stable_location: StableLocation,
    pub kind: SemanticDiagnosticKind,
}
impl DiagnosticEntry for SemanticDiagnostic {
    type DbType = dyn SemanticGroup;

    fn format(&self, _db: &Self::DbType) -> String {
        match self.kind {
            SemanticDiagnosticKind::UnknownBinaryOperator => "Unknown binary operator.",
            SemanticDiagnosticKind::UnknownFunction => "Unknown function.",
            SemanticDiagnosticKind::UnknownType => "Unknown type.",
            SemanticDiagnosticKind::WrongArgumentType { arg_typ: _, param_typ: _ } => {
                // TODO(lior): Add "Expected: {arg_typ}, found: {param_typ}.".
                "Unexpected argument type."
            }
        }
        .into()
    }

    fn location(&self, db: &Self::DbType) -> DiagnosticLocation {
        self.stable_location.diagnostic_location(db.upcast())
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum SemanticDiagnosticKind {
    UnknownBinaryOperator,
    UnknownFunction,
    UnknownType,
    WrongArgumentType { arg_typ: crate::TypeId, param_typ: crate::TypeId },
}
