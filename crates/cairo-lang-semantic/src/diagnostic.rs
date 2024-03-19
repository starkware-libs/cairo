use std::fmt::Display;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{
    EnumId, FunctionTitleId, ImplDefId, ImplFunctionId, ModuleItemId, NamedLanguageElementId,
    StructId, TopLevelLanguageElementId, TraitFunctionId, TraitId,
};
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_diagnostics::{
    error_code, DiagnosticAdded, DiagnosticEntry, DiagnosticLocation, Diagnostics,
    DiagnosticsBuilder, ErrorCode, Severity,
};
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_syntax as syntax;
use itertools::Itertools;
use smol_str::SmolStr;
use syntax::node::ids::SyntaxStablePtrId;
use syntax::node::TypedSyntaxNode;

use crate::corelib::LiteralError;
use crate::db::SemanticGroup;
use crate::expr::inference::InferenceError;
use crate::resolve::ResolvedConcreteItem;
use crate::semantic;
use crate::types::peel_snapshots;

#[cfg(test)]
#[path = "diagnostic_test.rs"]
mod test;

pub struct SemanticDiagnostics {
    pub diagnostics: DiagnosticsBuilder<SemanticDiagnostic>,
    pub file_id: FileId,
}
impl SemanticDiagnostics {
    pub fn new(file_id: FileId) -> Self {
        Self { file_id, diagnostics: DiagnosticsBuilder::default() }
    }
    pub fn build(self) -> Diagnostics<SemanticDiagnostic> {
        self.diagnostics.build()
    }
    /// Report a diagnostic in the location of the given node.
    pub fn report<TNode: TypedSyntaxNode>(
        &mut self,
        node: &TNode,
        kind: SemanticDiagnosticKind,
    ) -> DiagnosticAdded {
        self.diagnostics.add(SemanticDiagnostic::new(StableLocation::from_ast(node), kind))
    }
    /// Report a diagnostic in the location after the given node (with width 0).
    pub fn report_after<TNode: TypedSyntaxNode>(
        &mut self,
        node: &TNode,
        kind: SemanticDiagnosticKind,
    ) -> DiagnosticAdded {
        self.diagnostics.add(SemanticDiagnostic::new_after(StableLocation::from_ast(node), kind))
    }
    pub fn report_by_ptr(
        &mut self,
        stable_ptr: SyntaxStablePtrId,
        kind: SemanticDiagnosticKind,
    ) -> DiagnosticAdded {
        self.diagnostics.add(SemanticDiagnostic::new(StableLocation::new(stable_ptr), kind))
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct SemanticDiagnostic {
    pub stable_location: StableLocation,
    pub kind: SemanticDiagnosticKind,
    /// true if the diagnostic should be reported *after* the given location. Normally false, in
    /// which case the diagnostic points to the given location (as-is).
    pub after: bool,
}
impl SemanticDiagnostic {
    /// Create a diagnostic in the given location.
    pub fn new(stable_location: StableLocation, kind: SemanticDiagnosticKind) -> Self {
        SemanticDiagnostic { stable_location, kind, after: false }
    }
    /// Create a diagnostic in the location after the given location (with width 0).
    pub fn new_after(stable_location: StableLocation, kind: SemanticDiagnosticKind) -> Self {
        SemanticDiagnostic { stable_location, kind, after: true }
    }
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
            SemanticDiagnosticKind::UnknownBinaryOperator => "Unknown binary operator.".into(),
            SemanticDiagnosticKind::UnknownTrait => "Unknown trait.".into(),
            SemanticDiagnosticKind::UnknownImpl => "Unknown impl.".into(),
            SemanticDiagnosticKind::UnexpectedElement { expected, actual } => {
                let expected_str = expected.iter().map(|kind| kind.to_string()).join(" or ");
                format!("Expected {expected_str}, found {actual}.")
            }
            SemanticDiagnosticKind::UnknownType => "Unknown type.".into(),
            SemanticDiagnosticKind::UnknownEnum => "Unknown enum.".into(),
            SemanticDiagnosticKind::LiteralError(literal_error) => literal_error.format(db),
            SemanticDiagnosticKind::NotAVariant => {
                "Not a variant. Use the full name Enum::Variant.".into()
            }
            SemanticDiagnosticKind::NotAStruct => "Not a struct.".into(),
            SemanticDiagnosticKind::NotAType => "Not a type.".into(),
            SemanticDiagnosticKind::NotATrait => "Not a trait.".into(),
            SemanticDiagnosticKind::NotAnImpl => "Not an impl.".into(),
            SemanticDiagnosticKind::ImplItemNotInTrait {
                impl_def_id,
                impl_item_name,
                trait_id,
                item_kind,
            } => {
                let defs_db = db.upcast();
                format!(
                    "Impl item {item_kind} `{}::{}` is not a member of trait `{}`.",
                    impl_def_id.name(defs_db),
                    impl_item_name,
                    trait_id.name(defs_db)
                )
            }
            SemanticDiagnosticKind::GenericsNotSupportedInItem { scope, item_kind } => {
                format!("Generic parameters are not supported in {scope} item {item_kind}.")
            }
            SemanticDiagnosticKind::UnexpectedGenericArgs => "Unexpected generic arguments".into(),
            SemanticDiagnosticKind::UnknownMember => "Unknown member.".into(),
            SemanticDiagnosticKind::MemberSpecifiedMoreThanOnce => {
                "Member specified more than once.".into()
            }
            SemanticDiagnosticKind::ConstCycle => {
                "Cycle detected while resolving 'const' items.".into()
            }
            SemanticDiagnosticKind::UseCycle => {
                "Cycle detected while resolving 'use' items.".into()
            }
            SemanticDiagnosticKind::TypeAliasCycle => {
                "Cycle detected while resolving 'type alias' items.".into()
            }
            SemanticDiagnosticKind::ImplAliasCycle => {
                "Cycle detected while resolving 'impls alias' items.".into()
            }
            SemanticDiagnosticKind::ImplRequirementCycle => {
                "Cycle detected while resolving generic param. Try specifying the generic impl \
                 parameter explicitly to break the cycle."
                    .into()
            }
            SemanticDiagnosticKind::MissingMember { member_name } => {
                format!(r#"Missing member "{member_name}"."#)
            }
            SemanticDiagnosticKind::WrongNumberOfParameters {
                impl_def_id,
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
                    impl_def_id.name(defs_db),
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
            SemanticDiagnosticKind::WrongParameterType {
                impl_def_id,
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
                    impl_def_id.name(defs_db),
                    function_name,
                    trait_id.name(defs_db),
                    function_name,
                    expected_ty.format(db),
                    actual_ty.format(db)
                )
            }
            SemanticDiagnosticKind::VariantCtorNotImmutable => {
                "Variant constructor argument must be immutable.".to_string()
            }
            SemanticDiagnosticKind::TraitParamMutable { trait_id, function_id } => {
                let defs_db = db.upcast();
                format!(
                    "Parameter of trait function `{}::{}` can't be defined as mutable.",
                    trait_id.name(defs_db),
                    function_id.name(defs_db),
                )
            }
            SemanticDiagnosticKind::TraitFunctionWithBody { trait_id, function_id } => {
                let defs_db = db.upcast();
                format!(
                    "Trait function `{}::{}` has a body. Trait functions with body are not \
                     supported.",
                    trait_id.name(defs_db),
                    function_id.name(defs_db),
                )
            }
            SemanticDiagnosticKind::ParameterShouldBeReference {
                impl_def_id,
                impl_function_id,
                trait_id,
            } => {
                let defs_db = db.upcast();
                let function_name = impl_function_id.name(defs_db);
                format!(
                    "Parameter of impl function {}::{} is incompatible with {}::{}. It should be \
                     a reference.",
                    impl_def_id.name(defs_db),
                    function_name,
                    trait_id.name(defs_db),
                    function_name,
                )
            }
            SemanticDiagnosticKind::ParameterShouldNotBeReference {
                impl_def_id,
                impl_function_id,
                trait_id,
            } => {
                let defs_db = db.upcast();
                let function_name = impl_function_id.name(defs_db);
                format!(
                    "Parameter of impl function {}::{} is incompatible with {}::{}. It should not \
                     be a reference.",
                    impl_def_id.name(defs_db),
                    function_name,
                    trait_id.name(defs_db),
                    function_name,
                )
            }
            SemanticDiagnosticKind::WrongParameterName {
                impl_def_id,
                impl_function_id,
                trait_id,
                expected_name,
            } => {
                let defs_db = db.upcast();
                let function_name = impl_function_id.name(defs_db);
                format!(
                    "Parameter name of impl function {}::{function_name} is incompatible with \
                     {}::{function_name} parameter `{expected_name}`.",
                    impl_def_id.name(defs_db),
                    trait_id.name(defs_db),
                )
            }
            SemanticDiagnosticKind::WrongType { expected_ty, actual_ty } => {
                format!(
                    r#"Expected type "{}", found: "{}"."#,
                    expected_ty.format(db),
                    actual_ty.format(db)
                )
            }
            SemanticDiagnosticKind::InconsistentBinding => "variable is bound inconsistently \
                                                            across alternatives separated by `|` \
                                                            bound in different ways"
                .into(),
            SemanticDiagnosticKind::WrongArgumentType { expected_ty, actual_ty } => {
                let diagnostic_prefix = format!(
                    r#"Unexpected argument type. Expected: "{}", found: "{}"."#,
                    expected_ty.format(db),
                    actual_ty.format(db)
                );
                if (expected_ty.is_fully_concrete(db) && actual_ty.is_fully_concrete(db))
                    || peel_snapshots(db, *expected_ty).0 == peel_snapshots(db, *actual_ty).0
                {
                    diagnostic_prefix
                } else {
                    format!(
                        "{}\nIt is possible that the type inference failed because the types \
                         differ in the number of snapshots.\nConsider adding or removing \
                         snapshots.",
                        diagnostic_prefix
                    )
                }
            }
            SemanticDiagnosticKind::WrongReturnType { expected_ty, actual_ty } => {
                format!(
                    r#"Unexpected return type. Expected: "{}", found: "{}"."#,
                    expected_ty.format(db),
                    actual_ty.format(db)
                )
            }
            SemanticDiagnosticKind::WrongNumberOfGenericParamsForImplFunction {
                expected,
                actual,
            } => {
                format!(
                    "Wrong number of generic parameters for impl function. Expected: {}, found: \
                     {}.",
                    expected, actual
                )
            }
            SemanticDiagnosticKind::WrongReturnTypeForImpl {
                impl_def_id,
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
                    impl_def_id.name(defs_db),
                    function_name,
                    trait_id.name(defs_db),
                    function_name,
                    expected_ty.format(db),
                    actual_ty.format(db)
                )
            }
            SemanticDiagnosticKind::AmbiguousTrait { trait_function_id0, trait_function_id1 } => {
                format!(
                    "Ambiguous method call. More than one applicable trait function with a \
                     suitable self type was found: {} and {}. Consider adding type annotations or \
                     explicitly refer to the impl function.",
                    trait_function_id0.full_path(db.upcast()),
                    trait_function_id1.full_path(db.upcast())
                )
            }
            SemanticDiagnosticKind::VariableNotFound { name } => {
                format!(r#"Variable "{name}" not found."#)
            }
            SemanticDiagnosticKind::MissingVariableInPattern => {
                "Missing variable in pattern.".into()
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
            SemanticDiagnosticKind::ParamNameRedefinition { function_title_id, param_name } => {
                format!(
                    r#"Redefinition of parameter name "{param_name}" in function "{}"."#,
                    function_title_id.full_path(db.upcast())
                )
            }
            SemanticDiagnosticKind::ConditionNotBool { condition_ty } => {
                format!(r#"Condition has type "{}", expected bool."#, condition_ty.format(db))
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
            SemanticDiagnosticKind::LogicalOperatorNotAllowedInIfLet => {
                "Logical operator not allowed in if-let.".into()
            }
            SemanticDiagnosticKind::LogicalOperatorNotAllowedInWhileLet => {
                "Logical operator not allowed in while-let.".into()
            }
            SemanticDiagnosticKind::IncompatibleLoopBreakTypes { current_ty, break_ty } => {
                format!(
                    r#"Loop has incompatible return types: "{}" and "{}""#,
                    current_ty.format(db),
                    break_ty.format(db),
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
            SemanticDiagnosticKind::MemberNotVisible { member_name } => {
                format!(r#"Member "{member_name}" is not visible in this context."#)
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
            SemanticDiagnosticKind::UnhandledMustUseType { ty } => {
                format!(r#"Unhandled `#[must_use]` type `{}`"#, ty.format(db))
            }
            SemanticDiagnosticKind::UnhandledMustUseFunction => {
                "Unhandled `#[must_use]` function.".into()
            }
            SemanticDiagnosticKind::UnstableFeature { feature_name } => {
                format!(
                    r#"Usage of unstable feature `{feature_name}` with no `#[feature({feature_name})]` attribute."#
                )
            }
            SemanticDiagnosticKind::UnusedVariable => {
                "Unused variable. Consider ignoring by prefixing with `_`.".into()
            }
            SemanticDiagnosticKind::InvalidMemberExpression => "Invalid member expression.".into(),
            SemanticDiagnosticKind::InvalidPath => "Invalid path.".into(),
            SemanticDiagnosticKind::RefArgNotAVariable => "ref argument must be a variable.".into(),
            SemanticDiagnosticKind::RefArgNotMutable => {
                "ref argument must be a mutable variable.".into()
            }
            SemanticDiagnosticKind::RefArgNotExplicit => {
                "ref argument must be passed with a preceding 'ref'.".into()
            }
            SemanticDiagnosticKind::ImmutableArgWithModifiers => {
                "Argument to immutable parameter cannot have modifiers.".into()
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
            SemanticDiagnosticKind::ItemNotVisible { item_id } => {
                format!("Item `{}` is not visible in this context.", item_id.full_path(db.upcast()))
            }
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
            SemanticDiagnosticKind::UnexpectedFixedSizeArrayPattern { ty } => {
                format!(
                    "Unexpected type for fixed size array pattern. \"{}\" is not a fixed size \
                     array.",
                    ty.format(db),
                )
            }
            SemanticDiagnosticKind::WrongNumberOfTupleElements { expected, actual } => format!(
                r#"Wrong number of tuple elements in pattern. Expected: {}. Got: {}."#,
                expected, actual
            ),
            SemanticDiagnosticKind::WrongNumberOfFixedSizeArrayElements { expected, actual } => {
                format!(
                    "Wrong number of fixed size array elements in pattern. Expected: {}. Got: {}.",
                    expected, actual
                )
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
            SemanticDiagnosticKind::InvalidCopyTraitImpl { inference_error } => {
                format!("Invalid copy trait implementation, {}.", inference_error.format(db))
            }
            SemanticDiagnosticKind::InvalidDropTraitImpl { inference_error } => {
                format!("Invalid drop trait implementation, {}.", inference_error.format(db))
            }
            SemanticDiagnosticKind::InvalidImplItem { item_kw } => {
                format!("`{item_kw}` is not allowed inside impl.")
            }
            SemanticDiagnosticKind::MissingItemsInImpl { item_names } => {
                format!(
                    "Not all trait items are implemented. Missing: {}.",
                    item_names.iter().map(|name| format!("'{name}'")).join(", ")
                )
            }
            SemanticDiagnosticKind::PassPanicAsNopanic { impl_function_id, trait_id } => {
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
            SemanticDiagnosticKind::NameDefinedMultipleTimes { name } => {
                format!("The name `{name}` is defined multiple times.")
            }
            SemanticDiagnosticKind::NamedArgumentsAreNotSupported => {
                "Named arguments are not supported in this context.".into()
            }
            SemanticDiagnosticKind::UnnamedArgumentFollowsNamed => {
                "Unnamed arguments cannot follow named arguments.".into()
            }
            SemanticDiagnosticKind::NamedArgumentMismatch { expected, found } => {
                format!("Unexpected argument name. Expected: '{expected}', found '{found}'.")
            }
            SemanticDiagnosticKind::UnsupportedOutsideOfFunction { feature_name } => {
                let feature_name_str = match feature_name {
                    UnsupportedOutsideOfFunctionFeatureName::ReturnStatement => "Return statement",
                    UnsupportedOutsideOfFunctionFeatureName::ErrorPropagate => "The '?' operator",
                };
                format!("{feature_name_str} is not supported outside of functions.")
            }
            SemanticDiagnosticKind::UnsupportedConstant => {
                "This expression is not supported as constant.".into()
            }
            SemanticDiagnosticKind::DivisionByZero => "Division by zero.".into(),
            SemanticDiagnosticKind::ExternTypeWithImplGenericsNotSupported => {
                "Extern types with impl generics are not supported.".into()
            }
            SemanticDiagnosticKind::ImplGenericsAfterNonImplGenericsInExternFunction => {
                "Non-impl generics following an impl generic in an extern function are not \
                 supported."
                    .into()
            }
            SemanticDiagnosticKind::MissingSemicolon => "Missing semicolon".into(),
            SemanticDiagnosticKind::TraitMismatch { expected_trt, actual_trt } => {
                format!(
                    "Expected an impl of `{:?}`. Got an impl of `{:?}`.",
                    expected_trt.debug(db),
                    actual_trt.debug(db),
                )
            }
            SemanticDiagnosticKind::InternalInferenceError(err) => err.format(db),
            SemanticDiagnosticKind::DesnapNonSnapshot => {
                "Desnap operator can only be applied on snapshots".into()
            }
            SemanticDiagnosticKind::NoImplementationOfIndexOperator { ty, inference_errors } => {
                if inference_errors.is_empty() {
                    format!(
                        "Type `{}` does not implement the `Index` trait nor the `IndexView` trait.",
                        ty.format(db)
                    )
                } else {
                    format!(
                        "Type `{}` could not be indexed.\n{}",
                        ty.format(db),
                        inference_errors.format(db)
                    )
                }
            }
            SemanticDiagnosticKind::MultipleImplementationOfIndexOperator(ty) => {
                format!(
                    r#"Type "{}" implements both the "Index" trait and the "IndexView" trait."#,
                    ty.format(db)
                )
            }

            SemanticDiagnosticKind::UnsupportedInlineArguments => {
                "Unsupported `inline` arguments.".into()
            }
            SemanticDiagnosticKind::RedundantInlineAttribute => {
                "Redundant `inline` attribute.".into()
            }
            SemanticDiagnosticKind::InlineAttrForExternFunctionNotAllowed => {
                "`inline` attribute is not allowed for extern functions.".into()
            }
            SemanticDiagnosticKind::InlineAlwaysWithImplGenericArgNotAllowed => {
                "`#[inline(always)]` is not allowed for functions with impl generic parameters."
                    .into()
            }
            SemanticDiagnosticKind::CannotCallMethod { ty, method_name, inference_errors } => {
                if inference_errors.is_empty() {
                    format!(
                        "Method `{}` not found on type `{}`. Did you import the correct trait and \
                         impl?",
                        method_name,
                        ty.format(db)
                    )
                } else {
                    format!(
                        "Method `{}` could not be called on type `{}`.\n{}",
                        method_name,
                        ty.format(db),
                        inference_errors.format(db)
                    )
                }
            }
            SemanticDiagnosticKind::TailExpressionNotAllowedInLoop => {
                "Tail expression not allowed in a `loop` block.".into()
            }
            SemanticDiagnosticKind::ContinueOnlyAllowedInsideALoop => {
                "`continue` only allowed inside a `loop`.".into()
            }
            SemanticDiagnosticKind::BreakOnlyAllowedInsideALoop => {
                "`break` only allowed inside a `loop`.".into()
            }
            SemanticDiagnosticKind::BreakWithValueOnlyAllowedInsideALoop => {
                "Can only break with a value inside a `loop`.".into()
            }
            SemanticDiagnosticKind::ReturnNotAllowedInsideALoop => {
                "`return` not allowed inside a `loop`.".into()
            }
            SemanticDiagnosticKind::ErrorPropagateNotAllowedInsideALoop => {
                "`?` not allowed inside a `loop`.".into()
            }
            SemanticDiagnosticKind::ConstGenericParamNotSupported => {
                "Const generic args are not allowed in this context.".into()
            }
            SemanticDiagnosticKind::NegativeImplsNotEnabled => {
                "Negative impls are not enabled in the current crate.".into()
            }
            SemanticDiagnosticKind::NegativeImplsOnlyOnImpls => {
                "Negative impls supported only in impl definitions.".into()
            }
            SemanticDiagnosticKind::ImplicitPrecedenceAttrForExternFunctionNotAllowed => {
                "`implicit_precedence` attribute is not allowed for extern functions.".into()
            }
            SemanticDiagnosticKind::RedundantImplicitPrecedenceAttribute => {
                "Redundant `implicit_precedence` attribute.".into()
            }
            SemanticDiagnosticKind::UnsupportedImplicitPrecedenceArguments => {
                "Unsupported `implicit_precedence` arguments.".into()
            }
            SemanticDiagnosticKind::UnsupportedFeatureAttrArguments => {
                "`feature` attribute argument should be a single string.".into()
            }
            SemanticDiagnosticKind::UnsupportedPubArgument => "Unsupported `pub` argument.".into(),
            SemanticDiagnosticKind::UnknownStatementAttribute => {
                "Unknown statement attribute.".into()
            }
            SemanticDiagnosticKind::InlineMacroNotFound { macro_name } => {
                format!("Inline macro `{}` not found.", macro_name)
            }
            SemanticDiagnosticKind::InlineMacroFailed { macro_name } => {
                format!("Inline macro `{}` failed.", macro_name)
            }
            SemanticDiagnosticKind::UnknownGenericParam { name } => {
                format!("Unknown generic parameter `{}`.", name)
            }
            SemanticDiagnosticKind::PositionalGenericAfterNamed => {
                "Positional generic parameters must come before named generic parameters.".into()
            }
            SemanticDiagnosticKind::GenericArgDuplicate { name } => {
                format!("Generic argument `{}` is specified more than once.", name)
            }
            SemanticDiagnosticKind::TooManyGenericArguments { expected, actual } => {
                format!("Expected {} generic arguments, found {}.", expected, actual)
            }
            SemanticDiagnosticKind::GenericArgOutOfOrder { name } => {
                format!("Generic argument `{}` is out of order.", name)
            }
            SemanticDiagnosticKind::ArgPassedToNegativeImpl => {
                "Only `_` is a valid for negative impls.".into()
            }
            SemanticDiagnosticKind::UnsupportedTraitItem { kind } => {
                format!("{kind} items are not yet supported in traits.")
            }
            SemanticDiagnosticKind::UnsupportedImplItem { kind } => {
                format!("{kind} items are not yet supported in impls.")
            }
            SemanticDiagnosticKind::CouponForExternFunctionNotAllowed => {
                "Coupon cannot be used with extern functions.".into()
            }
            SemanticDiagnosticKind::CouponArgumentNoModifiers => {
                "The __coupon__ argument cannot have modifiers.".into()
            }
            SemanticDiagnosticKind::CouponsDisabled => {
                "Coupons are disabled in the current crate.\nYou can enable them by enabling the \
                 coupons experimental feature in the crate config."
                    .into()
            }
            SemanticDiagnosticKind::StructBaseStructExpressionNotLast => {
                "The base struct must always be the last argument.".into()
            }
            SemanticDiagnosticKind::StructBaseStructExpressionNoEffect => {
                "Base struct has no effect, all the fields in the struct have already been \
                 specified."
                    .into()
            }
            SemanticDiagnosticKind::FixedSizeArrayTypeNonSingleType => {
                "Fixed size array type must have exactly one type.".into()
            }
            SemanticDiagnosticKind::FixedSizeArrayTypeEmptySize => {
                "Fixed size array type must have a size clause.".into()
            }
            SemanticDiagnosticKind::FixedSizeArrayNonNumericSize => {
                "Fixed size array type must have a positive integer size.".into()
            }
            SemanticDiagnosticKind::FixedSizeArrayNonSingleValue => {
                "Fixed size array with defined size must have exactly one value.".into()
            }
            SemanticDiagnosticKind::FixedSizeArrayEmptyElements => {
                "Fixed size array must have at least one element.".into()
            }
            SemanticDiagnosticKind::FixedSizeArraySizeTooBig => {
                "Fixed size array size must be smaller than 2^15.".into()
            }
            SemanticDiagnosticKind::SelfNotSupportedInContext => {
                "`Self` is not supported in this context.".into()
            }
            SemanticDiagnosticKind::SelfMustBeFirst => {
                "`Self` can only be the first segment of a path.".into()
            }
        }
    }

    fn location(&self, db: &Self::DbType) -> DiagnosticLocation {
        let mut location = self.stable_location.diagnostic_location(db.upcast());
        if self.after {
            location = location.after();
        }
        location
    }

    fn severity(&self) -> Severity {
        match &self.kind {
            SemanticDiagnosticKind::UnusedVariable
            | SemanticDiagnosticKind::UnhandledMustUseType { .. }
            | SemanticDiagnosticKind::UnhandledMustUseFunction => Severity::Warning,
            SemanticDiagnosticKind::PluginDiagnostic(diag) => diag.severity,
            _ => Severity::Error,
        }
    }

    fn error_code(&self) -> Option<ErrorCode> {
        self.kind.error_code()
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum SemanticDiagnosticKind {
    ModuleFileNotFound {
        path: String,
    },
    Unsupported,
    UnknownLiteral,
    UnknownBinaryOperator,
    UnknownTrait,
    UnknownImpl,
    UnexpectedElement {
        expected: Vec<ElementKind>,
        actual: ElementKind,
    },
    UnknownType,
    UnknownEnum,
    LiteralError(LiteralError),
    NotAVariant,
    NotAStruct,
    NotAType,
    NotATrait,
    NotAnImpl,
    ImplItemNotInTrait {
        impl_def_id: ImplDefId,
        impl_item_name: SmolStr,
        trait_id: TraitId,
        item_kind: String,
    },
    GenericsNotSupportedInItem {
        scope: String,
        item_kind: String,
    },
    UnexpectedGenericArgs,
    UnknownMember,
    MemberSpecifiedMoreThanOnce,
    StructBaseStructExpressionNotLast,
    StructBaseStructExpressionNoEffect,
    ConstCycle,
    UseCycle,
    TypeAliasCycle,
    ImplAliasCycle,
    ImplRequirementCycle,
    MissingMember {
        member_name: SmolStr,
    },
    WrongNumberOfParameters {
        impl_def_id: ImplDefId,
        impl_function_id: ImplFunctionId,
        trait_id: TraitId,
        expected: usize,
        actual: usize,
    },
    WrongNumberOfArguments {
        expected: usize,
        actual: usize,
    },
    WrongParameterType {
        impl_def_id: ImplDefId,
        impl_function_id: ImplFunctionId,
        trait_id: TraitId,
        expected_ty: semantic::TypeId,
        actual_ty: semantic::TypeId,
    },
    VariantCtorNotImmutable,
    TraitParamMutable {
        trait_id: TraitId,
        function_id: TraitFunctionId,
    },
    TraitFunctionWithBody {
        trait_id: TraitId,
        function_id: TraitFunctionId,
    },
    ParameterShouldBeReference {
        impl_def_id: ImplDefId,
        impl_function_id: ImplFunctionId,
        trait_id: TraitId,
    },
    ParameterShouldNotBeReference {
        impl_def_id: ImplDefId,
        impl_function_id: ImplFunctionId,
        trait_id: TraitId,
    },
    WrongParameterName {
        impl_def_id: ImplDefId,
        impl_function_id: ImplFunctionId,
        trait_id: TraitId,
        expected_name: SmolStr,
    },
    WrongType {
        expected_ty: semantic::TypeId,
        actual_ty: semantic::TypeId,
    },
    InconsistentBinding,
    WrongArgumentType {
        expected_ty: semantic::TypeId,
        actual_ty: semantic::TypeId,
    },
    WrongReturnType {
        expected_ty: semantic::TypeId,
        actual_ty: semantic::TypeId,
    },
    WrongNumberOfGenericParamsForImplFunction {
        expected: usize,
        actual: usize,
    },
    WrongReturnTypeForImpl {
        impl_def_id: ImplDefId,
        impl_function_id: ImplFunctionId,
        trait_id: TraitId,
        expected_ty: semantic::TypeId,
        actual_ty: semantic::TypeId,
    },
    AmbiguousTrait {
        trait_function_id0: TraitFunctionId,
        trait_function_id1: TraitFunctionId,
    },
    VariableNotFound {
        name: SmolStr,
    },
    MissingVariableInPattern,
    StructMemberRedefinition {
        struct_id: StructId,
        member_name: SmolStr,
    },
    EnumVariantRedefinition {
        enum_id: EnumId,
        variant_name: SmolStr,
    },
    ParamNameRedefinition {
        function_title_id: FunctionTitleId,
        param_name: SmolStr,
    },
    ConditionNotBool {
        condition_ty: semantic::TypeId,
    },
    IncompatibleMatchArms {
        match_ty: semantic::TypeId,
        arm_ty: semantic::TypeId,
    },
    IncompatibleIfBlockTypes {
        block_if_ty: semantic::TypeId,
        block_else_ty: semantic::TypeId,
    },
    LogicalOperatorNotAllowedInIfLet,
    LogicalOperatorNotAllowedInWhileLet,
    IncompatibleLoopBreakTypes {
        current_ty: semantic::TypeId,
        break_ty: semantic::TypeId,
    },
    TypeHasNoMembers {
        ty: semantic::TypeId,
        member_name: SmolStr,
    },
    CannotCallMethod {
        ty: semantic::TypeId,
        method_name: SmolStr,
        inference_errors: TraitInferenceErrors,
    },
    NoSuchMember {
        struct_id: StructId,
        member_name: SmolStr,
    },
    MemberNotVisible {
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
    UnhandledMustUseType {
        ty: semantic::TypeId,
    },
    UnstableFeature {
        feature_name: SmolStr,
    },
    UnhandledMustUseFunction,
    UnusedVariable,
    ConstGenericParamNotSupported,
    NegativeImplsNotEnabled,
    NegativeImplsOnlyOnImpls,
    RefArgNotAVariable,
    RefArgNotMutable,
    RefArgNotExplicit,
    ImmutableArgWithModifiers,
    AssignmentToImmutableVar,
    InvalidLhsForAssignment,
    InvalidMemberExpression,
    InvalidPath,
    PathNotFound(NotFoundItemType),
    SuperUsedInRootModule,
    ItemNotVisible {
        item_id: ModuleItemId,
    },
    RedundantModifier {
        current_modifier: SmolStr,
        previous_modifier: SmolStr,
    },
    ReferenceLocalVariable,
    UnexpectedEnumPattern {
        ty: semantic::TypeId,
    },
    UnexpectedStructPattern {
        ty: semantic::TypeId,
    },
    UnexpectedTuplePattern {
        ty: semantic::TypeId,
    },
    UnexpectedFixedSizeArrayPattern {
        ty: semantic::TypeId,
    },
    WrongNumberOfTupleElements {
        expected: usize,
        actual: usize,
    },
    WrongNumberOfFixedSizeArrayElements {
        expected: usize,
        actual: usize,
    },
    WrongEnum {
        expected_enum: EnumId,
        actual_enum: EnumId,
    },
    InvalidCopyTraitImpl {
        inference_error: InferenceError,
    },
    InvalidDropTraitImpl {
        inference_error: InferenceError,
    },
    InvalidImplItem {
        item_kw: SmolStr,
    },
    MissingItemsInImpl {
        item_names: Vec<SmolStr>,
    },
    PassPanicAsNopanic {
        impl_function_id: ImplFunctionId,
        trait_id: TraitId,
    },
    PanicableFromNonPanicable,
    PanicableExternFunction,
    PluginDiagnostic(PluginDiagnostic),
    NameDefinedMultipleTimes {
        name: SmolStr,
    },
    NamedArgumentsAreNotSupported,
    ArgPassedToNegativeImpl,
    UnnamedArgumentFollowsNamed,
    NamedArgumentMismatch {
        expected: SmolStr,
        found: SmolStr,
    },
    UnsupportedOutsideOfFunction {
        feature_name: UnsupportedOutsideOfFunctionFeatureName,
    },
    UnsupportedConstant,
    DivisionByZero,
    ExternTypeWithImplGenericsNotSupported,
    ImplGenericsAfterNonImplGenericsInExternFunction,
    MissingSemicolon,
    TraitMismatch {
        expected_trt: semantic::ConcreteTraitId,
        actual_trt: semantic::ConcreteTraitId,
    },
    DesnapNonSnapshot,
    InternalInferenceError(InferenceError),
    NoImplementationOfIndexOperator {
        ty: semantic::TypeId,
        inference_errors: TraitInferenceErrors,
    },
    MultipleImplementationOfIndexOperator(semantic::TypeId),
    UnsupportedInlineArguments,
    RedundantInlineAttribute,
    InlineAttrForExternFunctionNotAllowed,
    InlineAlwaysWithImplGenericArgNotAllowed,
    TailExpressionNotAllowedInLoop,
    ContinueOnlyAllowedInsideALoop,
    BreakOnlyAllowedInsideALoop,
    BreakWithValueOnlyAllowedInsideALoop,
    ReturnNotAllowedInsideALoop,
    ErrorPropagateNotAllowedInsideALoop,
    ImplicitPrecedenceAttrForExternFunctionNotAllowed,
    RedundantImplicitPrecedenceAttribute,
    UnsupportedImplicitPrecedenceArguments,
    UnsupportedFeatureAttrArguments,
    UnsupportedPubArgument,
    UnknownStatementAttribute,
    InlineMacroNotFound {
        macro_name: SmolStr,
    },
    InlineMacroFailed {
        macro_name: SmolStr,
    },
    UnknownGenericParam {
        name: SmolStr,
    },
    PositionalGenericAfterNamed,
    GenericArgDuplicate {
        name: SmolStr,
    },
    TooManyGenericArguments {
        expected: usize,
        actual: usize,
    },
    GenericArgOutOfOrder {
        name: SmolStr,
    },
    UnsupportedTraitItem {
        kind: String,
    },
    UnsupportedImplItem {
        kind: String,
    },
    CouponForExternFunctionNotAllowed,
    CouponArgumentNoModifiers,
    /// Coupons are disabled in the current crate.
    CouponsDisabled,
    FixedSizeArrayTypeNonSingleType,
    FixedSizeArrayTypeEmptySize,
    FixedSizeArrayNonNumericSize,
    FixedSizeArrayNonSingleValue,
    FixedSizeArrayEmptyElements,
    FixedSizeArraySizeTooBig,
    SelfNotSupportedInContext,
    SelfMustBeFirst,
}

impl SemanticDiagnosticKind {
    pub fn error_code(&self) -> Option<ErrorCode> {
        Some(match &self {
            Self::UnusedVariable => error_code!(E0001),
            _ => return None,
        })
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum NotFoundItemType {
    Identifier,
    Function,
    Type,
    Trait,
    Impl,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum UnsupportedOutsideOfFunctionFeatureName {
    ReturnStatement,
    ErrorPropagate,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ElementKind {
    Constant,
    Variable,
    Module,
    Function,
    TraitFunction,
    Type,
    Variant,
    Trait,
    Impl,
}
impl From<&ResolvedConcreteItem> for ElementKind {
    fn from(val: &ResolvedConcreteItem) -> Self {
        match val {
            ResolvedConcreteItem::Constant(_) => ElementKind::Constant,
            ResolvedConcreteItem::ConstGenericParameter(_) => ElementKind::Constant,
            ResolvedConcreteItem::Module(_) => ElementKind::Module,
            ResolvedConcreteItem::Function(_) => ElementKind::Function,
            ResolvedConcreteItem::TraitFunction(_) => ElementKind::TraitFunction,
            ResolvedConcreteItem::Type(_) => ElementKind::Type,
            ResolvedConcreteItem::Variant(_) => ElementKind::Variant,
            ResolvedConcreteItem::Trait(_) => ElementKind::Trait,
            ResolvedConcreteItem::Impl(_) => ElementKind::Impl,
        }
    }
}
impl Display for ElementKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = match self {
            ElementKind::Constant => "constant",
            ElementKind::Variable => "variable",
            ElementKind::Module => "module",
            ElementKind::Function => "function",
            ElementKind::TraitFunction => "function",
            ElementKind::Type => "type",
            ElementKind::Variant => "variant",
            ElementKind::Trait => "trait",
            ElementKind::Impl => "impl",
        };
        write!(f, "{res}")
    }
}

/// A list of trait functions and the inference errors that occurred while trying to infer them.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TraitInferenceErrors {
    pub traits_and_errors: Vec<(TraitFunctionId, InferenceError)>,
}
impl TraitInferenceErrors {
    /// Is the error list empty.
    fn is_empty(&self) -> bool {
        self.traits_and_errors.is_empty()
    }
    /// Format the list of errors.
    fn format(&self, db: &(dyn SemanticGroup + 'static)) -> String {
        self.traits_and_errors
            .iter()
            .map(|(trait_function_id, inference_error)| {
                format!(
                    "Candidate `{}` inference failed with: {}",
                    trait_function_id.full_path(db.upcast()),
                    inference_error.format(db)
                )
            })
            .join("\n")
    }
}

/// A helper function to report diagnostics of yet-unsupported trait items.
pub fn report_unsupported_trait_item<Terminal: syntax::node::Terminal>(
    diagnostics: &mut SemanticDiagnostics,
    kw_terminal: Terminal,
    item_kind: &str,
) {
    diagnostics.report_by_ptr(
        kw_terminal.as_syntax_node().stable_ptr(),
        SemanticDiagnosticKind::UnsupportedTraitItem { kind: item_kind.into() },
    );
}

/// A helper function to report diagnostics of yet-unsupported impl items.
pub fn report_unsupported_impl_item<Terminal: syntax::node::Terminal>(
    diagnostics: &mut SemanticDiagnostics,
    kw_terminal: Terminal,
    item_kind: &str,
) {
    diagnostics.report_by_ptr(
        kw_terminal.as_syntax_node().stable_ptr(),
        SemanticDiagnosticKind::UnsupportedImplItem { kind: item_kind.into() },
    );
}
