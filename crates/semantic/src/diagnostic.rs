#[cfg(test)]
#[path = "diagnostic_test.rs"]
mod test;

use defs::diagnostic_utils::StableLocation;
use diagnostics::{DiagnosticEntry, DiagnosticLocation};

use crate::db::SemanticGroup;
use crate::semantic;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct SemanticDiagnostic {
    pub stable_location: StableLocation,
    pub kind: SemanticDiagnosticKind,
}
impl DiagnosticEntry for SemanticDiagnostic {
    type DbType = dyn SemanticGroup;

    fn format(&self, db: &Self::DbType) -> String {
        match self.kind {
            SemanticDiagnosticKind::UnknownBinaryOperator => "Unknown binary operator.".into(),
            SemanticDiagnosticKind::UnknownFunction => "Unknown function.".into(),
            SemanticDiagnosticKind::UnknownType => "Unknown type.".into(),
            SemanticDiagnosticKind::WrongArgumentType { expected_ty, actual_ty } => {
                format!(
                    r#"Unexpected argument type. Expected: "{}", found: "{}"."#,
                    expected_ty.format(db),
                    actual_ty.format(db)
                )
            }
            SemanticDiagnosticKind::WrongReturnType { expected_ty, actual_ty } => {
                format!(
                    r#"Unexpected return type. Expected: "{}", found: "{}"."#,
                    expected_ty.format(db),
                    actual_ty.format(db)
                )
            }
        }
    }

    fn location(&self, db: &Self::DbType) -> DiagnosticLocation {
        self.stable_location.diagnostic_location(db.as_defs_group())
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum SemanticDiagnosticKind {
    UnknownBinaryOperator,
    UnknownFunction,
    UnknownType,
    WrongArgumentType { expected_ty: semantic::TypeId, actual_ty: semantic::TypeId },
    WrongReturnType { expected_ty: semantic::TypeId, actual_ty: semantic::TypeId },
}
