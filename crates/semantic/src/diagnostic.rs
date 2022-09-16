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
            SemanticDiagnosticKind::Unsupported => "Unsupported feature.".into(),
            SemanticDiagnosticKind::UnknownLiteral => "Unknown literal.".into(),
            SemanticDiagnosticKind::UnknownBinaryOperator => "Unknown binary operator.".into(),
            SemanticDiagnosticKind::UnknownFunction => "Unknown function.".into(),
            SemanticDiagnosticKind::UnknownType => "Unknown type.".into(),
            SemanticDiagnosticKind::UnknownStruct => "Unknown struct.".into(),
            SemanticDiagnosticKind::UnknownMember => "Unknown member.".into(),
            SemanticDiagnosticKind::MissingMember { member_name } => {
                format!("Missing member {member_name}.")
            }
            SemanticDiagnosticKind::MemberSpecifiedMoreThanOnce => {
                "Member specified more than once.".into()
            }
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
            SemanticDiagnosticKind::VariableNotFound { name } => {
                format!("Variable {name} not found.")
            }
            SemanticDiagnosticKind::StructMemberRedefinition { struct_id, member_name } => {
                format!(
                    r#"Redefinition of member "{member_name}" on struct "{}"."#,
                    struct_id.full_path(db.upcast())
                )
            }
            SemanticDiagnosticKind::IncompatibleMatchArms { match_ty, arm_ty } => format!(
                r#"Match arms have incompatible types: "{}" and "{}""#,
                match_ty.format(db),
                arm_ty.format(db)
            ),
            SemanticDiagnosticKind::StructHasNoMembers { ty, member_name: _ } => {
                format!("Type {} has no members.", ty.format(db))
            }
            SemanticDiagnosticKind::NoSuchMember { struct_id, member_name } => {
                format!("Struct {} has not member {member_name}", struct_id.full_path(db.upcast()))
            }
            SemanticDiagnosticKind::InvalidMemberExpression => "Invalid member expression.".into(),
        }
    }

    fn location(&self, db: &Self::DbType) -> DiagnosticLocation {
        self.stable_location.diagnostic_location(db.upcast())
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum SemanticDiagnosticKind {
    Unsupported,
    UnknownLiteral,
    UnknownBinaryOperator,
    UnknownFunction,
    UnknownType,
    UnknownStruct,
    UnknownMember,
    MemberSpecifiedMoreThanOnce,
    MissingMember { member_name: SmolStr },
    WrongArgumentType { expected_ty: semantic::TypeId, actual_ty: semantic::TypeId },
    WrongReturnType { expected_ty: semantic::TypeId, actual_ty: semantic::TypeId },
    VariableNotFound { name: SmolStr },
    StructMemberRedefinition { struct_id: StructId, member_name: SmolStr },
    IncompatibleMatchArms { match_ty: semantic::TypeId, arm_ty: semantic::TypeId },
    StructHasNoMembers { ty: semantic::TypeId, member_name: SmolStr },
    NoSuchMember { struct_id: StructId, member_name: SmolStr },
    InvalidMemberExpression,
}
