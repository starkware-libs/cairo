#[cfg(test)]
#[path = "diagnostic_test.rs"]
mod test;

use defs::diagnostic_utils::StableLocation;
use defs::ids::{EnumId, GenericFunctionId, ModuleId, StructId, TopLevelLanguageElementId};
use diagnostics::{DiagnosticEntry, DiagnosticLocation, Diagnostics, DiagnosticsBuilder};
use smol_str::SmolStr;
use syntax::node::ids::SyntaxStablePtrId;
use syntax::node::TypedSyntaxNode;

use crate::db::SemanticGroup;
use crate::semantic;

pub struct SemanticDiagnostics {
    pub diagnostics: DiagnosticsBuilder<SemanticDiagnostic>,
    pub module_id: ModuleId,
}
impl SemanticDiagnostics {
    pub fn new(module_id: ModuleId) -> Self {
        Self { module_id, diagnostics: DiagnosticsBuilder::default() }
    }
    pub fn build(self) -> Diagnostics<SemanticDiagnostic> {
        self.diagnostics.build()
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
            SemanticDiagnosticKind::NotAFunction => "Not a function.".into(),
            SemanticDiagnosticKind::UnknownType => "Unknown type.".into(),
            SemanticDiagnosticKind::UnknownStruct => "Unknown struct.".into(),
            SemanticDiagnosticKind::UnknownEnum => "Unknown enum.".into(),
            SemanticDiagnosticKind::NotAVariant => {
                "Not a variant. Use the full name Enum::Variant.".into()
            }
            SemanticDiagnosticKind::NotAStruct => "Not a struct.".into(),
            SemanticDiagnosticKind::NotAType => "Not a type.".into(),
            SemanticDiagnosticKind::UnexpectedGenericArgs => "Unexpected generic arguments".into(),
            SemanticDiagnosticKind::UnknownMember => "Unknown member.".into(),
            SemanticDiagnosticKind::MemberSpecifiedMoreThanOnce => {
                "Member specified more than once.".into()
            }
            SemanticDiagnosticKind::UseCycle => {
                "Cycle detected while resolving 'use' items.".into()
            }
            SemanticDiagnosticKind::ExpectedConcreteVariant => {
                "Expected a concrete variant. Use `::<>` syntax.".to_string()
            }
            SemanticDiagnosticKind::MissingMember { member_name } => {
                format!(r#"Missing member "{member_name}"."#)
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
                format!(r#"Variable "{name}" not found."#)
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
            SemanticDiagnosticKind::ParamNameRedefinition { function_id, param_name } => {
                format!(
                    r#"Redefinition of parameter name "{param_name}" in function "{}"."#,
                    function_id.full_path(db.upcast())
                )
            }
            SemanticDiagnosticKind::IncompatibleMatchArms { match_ty, arm_ty } => format!(
                r#"Match arms have incompatible types: "{}" and "{}""#,
                match_ty.format(db),
                arm_ty.format(db)
            ),
            SemanticDiagnosticKind::IncompatibleIfBlockTypes { block_if_ty, block_else_ty } => {
                format!(
                    r#"If blocks have incompatible types: "{}" and "{}""#,
                    block_if_ty.format(db),
                    block_else_ty.format(db),
                )
            }
            SemanticDiagnosticKind::TypeHasNoMembers { ty, member_name: _ } => {
                format!(r#"Type "{}" has no members."#, ty.format(db))
            }
            SemanticDiagnosticKind::NoSuchMember { struct_id, member_name } => {
                format!(
                    r#"Struct "{}" has no member "{member_name}""#,
                    struct_id.full_path(db.upcast())
                )
            }
            SemanticDiagnosticKind::NoSuchVariant { enum_id, variant_name } => {
                format!(
                    r#"Enum "{}" has no variant "{variant_name}""#,
                    enum_id.full_path(db.upcast())
                )
            }
            SemanticDiagnosticKind::InvalidMemberExpression => "Invalid member expression.".into(),
            SemanticDiagnosticKind::InvalidPath => "Invalid path.".into(),
            SemanticDiagnosticKind::RefArgNotAVariable => "ref argument must be a variable.".into(),
            SemanticDiagnosticKind::InvalidLhsForAssignment => {
                "Invalid left-hand side of assignment.".into()
            }
            SemanticDiagnosticKind::PathNotFound => "Path not found.".into(),
            SemanticDiagnosticKind::UnexpectedLiteralPattern { ty } => format!(
                r#"Unexpected type for literal pattern. Expected: felt. Got: "{}""#,
                ty.format(db),
            ),
            SemanticDiagnosticKind::UnexpectedEnumPattern { ty } => {
                format!(r#"Unexpected type for enum pattern. "{}" is not an enum."#, ty.format(db),)
            }
            SemanticDiagnosticKind::UnexpectedStructPattern { ty } => {
                format!(
                    r#"Unexpected type for struct pattern. "{}" is not a struct."#,
                    ty.format(db),
                )
            }
            SemanticDiagnosticKind::UnexpectedTuplePattern { ty } => {
                format!(r#"Unexpected type for tuple pattern. "{}" is not a tuple."#, ty.format(db),)
            }
            SemanticDiagnosticKind::WrongEnum { expected_enum, actual_enum } => {
                format!(
                    r#"Wrong enum in pattern. Expected: "{}". Got: "{}"."#,
                    expected_enum.full_path(db.upcast()),
                    actual_enum.full_path(db.upcast())
                )
            }
            SemanticDiagnosticKind::RepeatedModifier { modifier } => {
                format!("`{}` modifier may not be repeated", modifier)
            }
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
    NotAFunction,
    UnknownType,
    UnknownStruct,
    UnknownEnum,
    NotAVariant,
    NotAStruct,
    NotAType,
    UnexpectedGenericArgs,
    UnknownMember,
    MemberSpecifiedMoreThanOnce,
    UseCycle,
    ExpectedConcreteVariant,
    MissingMember { member_name: SmolStr },
    WrongNumberOfArguments { expected: usize, actual: usize },
    WrongNumberOfGenericArguments { expected: usize, actual: usize },
    WrongArgumentType { expected_ty: semantic::TypeId, actual_ty: semantic::TypeId },
    WrongReturnType { expected_ty: semantic::TypeId, actual_ty: semantic::TypeId },
    VariableNotFound { name: SmolStr },
    StructMemberRedefinition { struct_id: StructId, member_name: SmolStr },
    EnumVariantRedefinition { enum_id: EnumId, variant_name: SmolStr },
    ParamNameRedefinition { function_id: GenericFunctionId, param_name: SmolStr },
    IncompatibleMatchArms { match_ty: semantic::TypeId, arm_ty: semantic::TypeId },
    IncompatibleIfBlockTypes { block_if_ty: semantic::TypeId, block_else_ty: semantic::TypeId },
    TypeHasNoMembers { ty: semantic::TypeId, member_name: SmolStr },
    NoSuchMember { struct_id: StructId, member_name: SmolStr },
    NoSuchVariant { enum_id: EnumId, variant_name: SmolStr },
    RefArgNotAVariable,
    InvalidLhsForAssignment,
    InvalidMemberExpression,
    InvalidPath,
    PathNotFound,
    RepeatedModifier { modifier: SmolStr },
    UnexpectedLiteralPattern { ty: semantic::TypeId },
    UnexpectedEnumPattern { ty: semantic::TypeId },
    UnexpectedStructPattern { ty: semantic::TypeId },
    UnexpectedTuplePattern { ty: semantic::TypeId },
    WrongEnum { expected_enum: EnumId, actual_enum: EnumId },
}
