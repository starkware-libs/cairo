#[cfg(test)]
#[path = "diagnostic_test.rs"]
mod test;

use defs::diagnostic_utils::StableLocation;
use defs::ids::{LanguageElementId, StructId};
use diagnostics::{DiagnosticEntry, DiagnosticLocation};
use smol_str::SmolStr;

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
        match &self.kind {
            SemanticDiagnosticKind::UnknownBinaryOperator => "Unknown binary operator.".into(),
            SemanticDiagnosticKind::UnknownFunction => "Unknown function.".into(),
            SemanticDiagnosticKind::UnknownType => "Unknown type.".into(),
            SemanticDiagnosticKind::WrongArgumentType { arg_typ: _, param_typ: _ } => {
                // TODO(lior): Add "Expected: {arg_typ}, found: {param_typ}.".
                "Unexpected argument type.".into()
            }
            SemanticDiagnosticKind::StructMemberRedefinition { struct_id, member_name } => {
                format!(
                    r#"Redefinition of member "{member_name}" on struct "{}"."#,
                    struct_id.full_path(db.as_defs_group())
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
    WrongArgumentType { arg_typ: semantic::TypeId, param_typ: semantic::TypeId },
    StructMemberRedefinition { struct_id: StructId, member_name: SmolStr },
}
