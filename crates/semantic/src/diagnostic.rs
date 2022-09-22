#[cfg(test)]
#[path = "diagnostic_test.rs"]
mod test;

use defs::diagnostic_utils::StableLocation;
use defs::ids::{EnumId, LanguageElementId, ModuleId, StructId};
use diagnostics::{DiagnosticEntry, DiagnosticLocation, Diagnostics};
use smol_str::SmolStr;
use syntax::node::ids::SyntaxStablePtrId;
use syntax::node::TypedSyntaxNode;

use crate::db::SemanticGroup;
use crate::semantic;

pub struct SemanticDiagnostics {
    pub diagnostics: Diagnostics<SemanticDiagnostic>,
    pub module_id: ModuleId,
}
impl SemanticDiagnostics {
    pub fn new(module_id: ModuleId) -> Self {
        Self { module_id, diagnostics: Diagnostics::default() }
    }
    pub fn report<TNode: TypedSyntaxNode>(&mut self, node: &TNode, kind: SemanticDiagnosticKind) {
        self.diagnostics.add(SemanticDiagnostic {
            stable_location: StableLocation::from_ast(self.module_id, node),
            kind,
        });
    }
    pub fn report_by_ptr(&mut self, stable_ptr: SyntaxStablePtrId, kind: SemanticDiagnosticKind) {
        self.diagnostics.add(SemanticDiagnostic {
            stable_location: StableLocation::new(self.module_id, stable_ptr),
            kind,
        });
    }
}

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
            SemanticDiagnosticKind::MemberSpecifiedMoreThanOnce => {
                "Member specified more than once.".into()
            }
            SemanticDiagnosticKind::MissingMember { member_name } => {
                format!("Missing member {member_name}.")
            }
            SemanticDiagnosticKind::WrongNumberOfArguments { expected, actual } => {
                format!("Wrong number of arguments. Expected {expected}, found: {actual}")
            }
            SemanticDiagnosticKind::WrongNumberOfGenericArguments { expected, actual } => {
                format!("Wrong number of generic arguments. Expected {expected}, found: {actual}")
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
            SemanticDiagnosticKind::EnumVariantRedefinition { enum_id, variant_name } => {
                format!(
                    r#"Redefinition of variant "{variant_name}" on enum "{}"."#,
                    enum_id.full_path(db.upcast())
                )
            }
            SemanticDiagnosticKind::IncompatibleMatchArms { match_ty, arm_ty } => format!(
                r#"Match arms have incompatible types: "{}" and "{}""#,
                match_ty.format(db),
                arm_ty.format(db)
            ),
            SemanticDiagnosticKind::TypeHasNoMembers { ty, member_name: _ } => {
                format!("Type {} has no members.", ty.format(db))
            }
            SemanticDiagnosticKind::NoSuchMember { struct_id, member_name } => {
                format!("Struct {} has not member {member_name}", struct_id.full_path(db.upcast()))
            }
            SemanticDiagnosticKind::InvalidMemberExpression => "Invalid member expression.".into(),
            SemanticDiagnosticKind::InvalidPath => "Invalid path.".into(),
            SemanticDiagnosticKind::PathNotFound => "Path not found.".into(),
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
    WrongNumberOfArguments { expected: usize, actual: usize },
    WrongNumberOfGenericArguments { expected: usize, actual: usize },
    WrongArgumentType { expected_ty: semantic::TypeId, actual_ty: semantic::TypeId },
    WrongReturnType { expected_ty: semantic::TypeId, actual_ty: semantic::TypeId },
    VariableNotFound { name: SmolStr },
    StructMemberRedefinition { struct_id: StructId, member_name: SmolStr },
    EnumVariantRedefinition { enum_id: EnumId, variant_name: SmolStr },
    IncompatibleMatchArms { match_ty: semantic::TypeId, arm_ty: semantic::TypeId },
    TypeHasNoMembers { ty: semantic::TypeId, member_name: SmolStr },
    NoSuchMember { struct_id: StructId, member_name: SmolStr },
    InvalidMemberExpression,
    InvalidPath,
    PathNotFound,
}
