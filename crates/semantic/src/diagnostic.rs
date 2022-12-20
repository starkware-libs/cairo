#[cfg(test)]
#[path = "diagnostic_test.rs"]
mod test;

use defs::diagnostic_utils::StableLocation;
use defs::ids::{
    EnumId, GenericFunctionId, ImplFunctionId, ImplId, ModuleFileId, StructId,
    TopLevelLanguageElementId, TraitFunctionId, TraitId,
};
use defs::plugin::PluginDiagnostic;
use diagnostics::{
    DiagnosticAdded, DiagnosticEntry, DiagnosticLocation, Diagnostics, DiagnosticsBuilder,
};
use smol_str::SmolStr;
use syntax::node::ids::SyntaxStablePtrId;
use syntax::node::TypedSyntaxNode;

use crate::db::SemanticGroup;
use crate::semantic;

pub struct SemanticDiagnostics {
    pub diagnostics: DiagnosticsBuilder<SemanticDiagnostic>,
    pub module_file_id: ModuleFileId,
}
impl SemanticDiagnostics {
    pub fn new(module_file_id: ModuleFileId) -> Self {
        Self { module_file_id, diagnostics: DiagnosticsBuilder::default() }
    }
    pub fn build(self) -> Diagnostics<SemanticDiagnostic> {
        self.diagnostics.build()
    }
    pub fn report<TNode: TypedSyntaxNode>(
        &mut self,
        node: &TNode,
        kind: SemanticDiagnosticKind,
    ) -> DiagnosticAdded {
        self.diagnostics.add(SemanticDiagnostic {
            stable_location: StableLocation::from_ast(self.module_file_id, node),
            kind,
        })
    }
    pub fn report_by_ptr(
        &mut self,
        stable_ptr: SyntaxStablePtrId,
        kind: SemanticDiagnosticKind,
    ) -> DiagnosticAdded {
        self.diagnostics.add(SemanticDiagnostic {
            stable_location: StableLocation::new(self.module_file_id, stable_ptr),
            kind,
        })
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
            SemanticDiagnosticKind::ModuleFileNotFound { path } => {
                format!("Module file not found. Expected path: {path}")
            }
            SemanticDiagnosticKind::Unsupported => "Unsupported feature.".into(),
            SemanticDiagnosticKind::UnknownLiteral => "Unknown literal.".into(),
            SemanticDiagnosticKind::UnsupportedUnaryOperator { op, ty } => {
                format!("Unary operator '{op}' is not supported for type '{}'.", ty.format(db),)
            }
            SemanticDiagnosticKind::UnknownBinaryOperator => "Unknown binary operator.".into(),
            SemanticDiagnosticKind::UnsupportedBinaryOperator { op, type1, type2 } => {
                format!(
                    "Binary operator '{op}' is not supported for types '{}' and '{}'.",
                    type1.format(db),
                    type2.format(db)
                )
            }
            SemanticDiagnosticKind::UnknownFunction => "Unknown function.".into(),
            SemanticDiagnosticKind::UnknownTrait => "Unknown trait.".into(),
            SemanticDiagnosticKind::UnknownImpl => "Unknown impl.".into(),
            SemanticDiagnosticKind::NotAFunction => "Not a function.".into(),
            SemanticDiagnosticKind::UnknownType => "Unknown type.".into(),
            SemanticDiagnosticKind::UnknownStruct => "Unknown struct.".into(),
            SemanticDiagnosticKind::UnknownEnum => "Unknown enum.".into(),
            SemanticDiagnosticKind::NoLiteralFunctionFound => {
                "A literal with this type cannot be created.".into()
            }
            SemanticDiagnosticKind::NotAVariant => {
                "Not a variant. Use the full name Enum::Variant.".into()
            }
            SemanticDiagnosticKind::NotAStruct => "Not a struct.".into(),
            SemanticDiagnosticKind::NotAType => "Not a type.".into(),
            SemanticDiagnosticKind::NotATrait => "Not a trait.".into(),
            SemanticDiagnosticKind::FunctionNotMemberOfTrait {
                impl_id,
                impl_function_id,
                trait_id,
            } => {
                let defs_db = db.upcast();
                format!(
                    "Impl function `{}::{}` is not a member of trait `{}`.",
                    impl_id.name(defs_db),
                    impl_function_id.name(defs_db),
                    trait_id.name(defs_db)
                )
            }
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
            SemanticDiagnosticKind::WrongNumberOfParameters {
                impl_id,
                impl_function_id,
                trait_id,
                expected,
                actual,
            } => {
                let defs_db = db.upcast();
                let function_name = impl_function_id.name(defs_db);
                format!(
                    "The number of parameters in the impl function `{}::{}` is incompatible with \
                     `{}::{}`. Expected: {}, actual: {}.",
                    impl_id.name(defs_db),
                    function_name,
                    trait_id.name(defs_db),
                    function_name,
                    expected,
                    actual,
                )
            }
            SemanticDiagnosticKind::WrongNumberOfArguments { expected, actual } => {
                format!("Wrong number of arguments. Expected {expected}, found: {actual}")
            }
            SemanticDiagnosticKind::WrongNumberOfGenericArguments { expected, actual } => {
                format!("Wrong number of generic arguments. Expected {expected}, found: {actual}")
            }
            SemanticDiagnosticKind::WrongParameterType {
                impl_id,
                impl_function_id,
                trait_id,
                expected_ty,
                actual_ty,
            } => {
                let defs_db = db.upcast();
                let function_name = impl_function_id.name(defs_db);
                format!(
                    "Parameter type of impl function `{}::{}` is incompatible with `{}::{}`. \
                     Expected: `{}`, actual: `{}`.",
                    impl_id.name(defs_db),
                    function_name,
                    trait_id.name(defs_db),
                    function_name,
                    expected_ty.format(db),
                    actual_ty.format(db)
                )
            }
            SemanticDiagnosticKind::TraitParamMutable { trait_id, function_id } => {
                let defs_db = db.upcast();
                format!(
                    "Parameter of trait function `{}::{}` can't be defined as mutable.",
                    trait_id.name(defs_db),
                    function_id.name(defs_db),
                )
            }
            SemanticDiagnosticKind::ParamaterShouldBeReference {
                impl_id,
                impl_function_id,
                trait_id,
            } => {
                let defs_db = db.upcast();
                let function_name = impl_function_id.name(defs_db);
                format!(
                    "Parameter of impl function {}::{} is incompatible with {}::{}. It should be \
                     a reference.",
                    impl_id.name(defs_db),
                    function_name,
                    trait_id.name(defs_db),
                    function_name,
                )
            }
            SemanticDiagnosticKind::ParamaterShouldNotBeReference {
                impl_id,
                impl_function_id,
                trait_id,
            } => {
                let defs_db = db.upcast();
                let function_name = impl_function_id.name(defs_db);
                format!(
                    "Parameter of impl function {}::{} is incompatible with {}::{}. It should not \
                     be a reference.",
                    impl_id.name(defs_db),
                    function_name,
                    trait_id.name(defs_db),
                    function_name,
                )
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
            SemanticDiagnosticKind::WrongReturnTypeForImpl {
                impl_id,
                impl_function_id,
                trait_id,
                expected_ty,
                actual_ty,
            } => {
                let defs_db = db.upcast();
                let function_name = impl_function_id.name(defs_db);
                format!(
                    "Return type of impl function `{}::{}` is incompatible with `{}::{}`. \
                     Expected: `{}`, actual: `{}`.",
                    impl_id.name(defs_db),
                    function_name,
                    trait_id.name(defs_db),
                    function_name,
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
            SemanticDiagnosticKind::IncompatibleErrorPropagateType { return_ty, err_ty } => {
                format!(
                    r#"Return type "{}" does not wrap error "{}""#,
                    return_ty.format(db),
                    err_ty.format(db)
                )
            }
            SemanticDiagnosticKind::ErrorPropagateOnNonErrorType { ty } => {
                format!(r#"Type "{}" can not error propagate"#, ty.format(db))
            }
            SemanticDiagnosticKind::InvalidMemberExpression => "Invalid member expression.".into(),
            SemanticDiagnosticKind::InvalidPath => "Invalid path.".into(),
            SemanticDiagnosticKind::RefArgNotAVariable => "ref argument must be a variable.".into(),
            SemanticDiagnosticKind::RefArgNotMutable => {
                "ref argument must be a mutable variable.".into()
            }
            SemanticDiagnosticKind::AssignmentToImmutableVar => {
                "Cannot assign to an immutable variable.".into()
            }
            SemanticDiagnosticKind::InvalidLhsForAssignment => {
                "Invalid left-hand side of assignment.".into()
            }
            SemanticDiagnosticKind::PathNotFound(item_type) => match item_type {
                NotFoundItemType::Identifier => "Identifier not found.".into(),
                NotFoundItemType::Function => "Function not found.".into(),
                NotFoundItemType::Type => "Type not found.".into(),
                NotFoundItemType::Trait => "Trait not found.".into(),
                NotFoundItemType::Impl => "Impl not found.".into(),
            },
            SemanticDiagnosticKind::SuperUsedInRootModule => {
                "'super' cannot be used for the crate's root module.".into()
            }
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
            SemanticDiagnosticKind::RedundantModifier { current_modifier, previous_modifier } => {
                format!(
                    "`{current_modifier}` modifier was specified after another modifier \
                     (`{previous_modifier}`). Only a single modifier is allowed."
                )
            }
            SemanticDiagnosticKind::ReferenceLocalVariable => {
                "`ref` is only allowed for function parameters, not for local variables."
                    .to_string()
            }
            SemanticDiagnosticKind::ShortStringMustBeAscii => {
                "Short strings can only include ASCII characters.".into()
            }
            SemanticDiagnosticKind::IllegalStringEscaping(err) => {
                format!("Invalid string escaping:\n{err}")
            }
            SemanticDiagnosticKind::InvalidCopyTraitImpl => {
                "Invalid copy trait implementation.".into()
            }
            SemanticDiagnosticKind::InvalidDropTraitImpl => {
                "Invalid drop trait implementation.".into()
            }
            SemanticDiagnosticKind::InvalidImplItem { item_kw } => {
                format!("`{}` is not allowed inside impl.", item_kw)
            }
            SemanticDiagnosticKind::PassPanicAsNonpanic { impl_function_id, trait_id } => {
                let name = impl_function_id.name(db.upcast());
                let trait_name = trait_id.name(db.upcast());
                format!(
                    "The signature of function `{name}` is incompatible with trait \
                     `{trait_name}`. The trait function is declared as nopanic."
                )
            }
            SemanticDiagnosticKind::PanicableFromNonPanicable => {
                "Function is declared as nopanic but calls a function that may panic.".into()
            }
            SemanticDiagnosticKind::PanicableExternFunction => {
                "An extern function must be marked as nopanic.".into()
            }
            SemanticDiagnosticKind::PluginDiagnostic(diagnostic) => {
                format!("Plugin diagnostic: {}", diagnostic.message)
            }
            SemanticDiagnosticKind::WrappedPluginDiagnostic { diagnostic, original_diag: _ } => {
                // TODO(spapini): Support nested diagnostics.
                format!("Plugin diagnostic: {}", diagnostic.message)
            }
        }
    }

    fn location(&self, db: &Self::DbType) -> DiagnosticLocation {
        self.stable_location.diagnostic_location(db.upcast())
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum SemanticDiagnosticKind {
    ModuleFileNotFound {
        path: String,
    },
    Unsupported,
    UnknownLiteral,
    UnsupportedUnaryOperator {
        op: SmolStr,
        ty: semantic::TypeId,
    },
    UnknownBinaryOperator,
    UnsupportedBinaryOperator {
        op: SmolStr,
        type1: semantic::TypeId,
        type2: semantic::TypeId,
    },
    UnknownFunction,
    UnknownTrait,
    UnknownImpl,
    NotAFunction,
    UnknownType,
    UnknownStruct,
    UnknownEnum,
    NoLiteralFunctionFound,
    NotAVariant,
    NotAStruct,
    NotAType,
    NotATrait,
    FunctionNotMemberOfTrait {
        impl_id: ImplId,
        impl_function_id: ImplFunctionId,
        trait_id: TraitId,
    },
    UnexpectedGenericArgs,
    UnknownMember,
    MemberSpecifiedMoreThanOnce,
    UseCycle,
    ExpectedConcreteVariant,
    MissingMember {
        member_name: SmolStr,
    },
    WrongNumberOfParameters {
        impl_id: ImplId,
        impl_function_id: ImplFunctionId,
        trait_id: TraitId,
        expected: usize,
        actual: usize,
    },
    WrongNumberOfArguments {
        expected: usize,
        actual: usize,
    },
    WrongNumberOfGenericArguments {
        expected: usize,
        actual: usize,
    },
    WrongParameterType {
        impl_id: ImplId,
        impl_function_id: ImplFunctionId,
        trait_id: TraitId,
        expected_ty: semantic::TypeId,
        actual_ty: semantic::TypeId,
    },
    TraitParamMutable {
        trait_id: TraitId,
        function_id: TraitFunctionId,
    },
    ParamaterShouldBeReference {
        impl_id: ImplId,
        impl_function_id: ImplFunctionId,
        trait_id: TraitId,
    },
    ParamaterShouldNotBeReference {
        impl_id: ImplId,
        impl_function_id: ImplFunctionId,
        trait_id: TraitId,
    },
    WrongArgumentType {
        expected_ty: semantic::TypeId,
        actual_ty: semantic::TypeId,
    },
    WrongReturnType {
        expected_ty: semantic::TypeId,
        actual_ty: semantic::TypeId,
    },
    WrongReturnTypeForImpl {
        impl_id: ImplId,
        impl_function_id: ImplFunctionId,
        trait_id: TraitId,
        expected_ty: semantic::TypeId,
        actual_ty: semantic::TypeId,
    },
    VariableNotFound {
        name: SmolStr,
    },
    StructMemberRedefinition {
        struct_id: StructId,
        member_name: SmolStr,
    },
    EnumVariantRedefinition {
        enum_id: EnumId,
        variant_name: SmolStr,
    },
    ParamNameRedefinition {
        function_id: GenericFunctionId,
        param_name: SmolStr,
    },
    IncompatibleMatchArms {
        match_ty: semantic::TypeId,
        arm_ty: semantic::TypeId,
    },
    IncompatibleIfBlockTypes {
        block_if_ty: semantic::TypeId,
        block_else_ty: semantic::TypeId,
    },
    TypeHasNoMembers {
        ty: semantic::TypeId,
        member_name: SmolStr,
    },
    NoSuchMember {
        struct_id: StructId,
        member_name: SmolStr,
    },
    NoSuchVariant {
        enum_id: EnumId,
        variant_name: SmolStr,
    },
    IncompatibleErrorPropagateType {
        return_ty: semantic::TypeId,
        err_ty: semantic::TypeId,
    },
    ErrorPropagateOnNonErrorType {
        ty: semantic::TypeId,
    },
    RefArgNotAVariable,
    RefArgNotMutable,
    AssignmentToImmutableVar,
    InvalidLhsForAssignment,
    InvalidMemberExpression,
    InvalidPath,
    PathNotFound(NotFoundItemType),
    SuperUsedInRootModule,
    RedundantModifier {
        current_modifier: SmolStr,
        previous_modifier: SmolStr,
    },
    ReferenceLocalVariable,
    UnexpectedLiteralPattern {
        ty: semantic::TypeId,
    },
    UnexpectedEnumPattern {
        ty: semantic::TypeId,
    },
    UnexpectedStructPattern {
        ty: semantic::TypeId,
    },
    UnexpectedTuplePattern {
        ty: semantic::TypeId,
    },
    WrongEnum {
        expected_enum: EnumId,
        actual_enum: EnumId,
    },
    ShortStringMustBeAscii,
    IllegalStringEscaping(String),
    InvalidCopyTraitImpl,
    InvalidDropTraitImpl,
    InvalidImplItem {
        item_kw: SmolStr,
    },
    PassPanicAsNonpanic {
        impl_function_id: ImplFunctionId,
        trait_id: TraitId,
    },
    PanicableFromNonPanicable,
    PanicableExternFunction,
    PluginDiagnostic(PluginDiagnostic),
    WrappedPluginDiagnostic {
        diagnostic: PluginDiagnostic,
        original_diag: Box<SemanticDiagnostic>,
    },
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum NotFoundItemType {
    Identifier,
    Function,
    Type,
    Trait,
    Impl,
}
