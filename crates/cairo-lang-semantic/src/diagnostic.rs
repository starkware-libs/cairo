use std::fmt::Display;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{
    EnumId, FunctionTitleId, GenericKind, ImplDefId, ImplFunctionId, ModuleId, ModuleItemId,
    NamedLanguageElementId, StructId, TopLevelLanguageElementId, TraitFunctionId, TraitId,
    TraitImplId, TraitItemId, UseId,
};
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_diagnostics::{
    DiagnosticAdded, DiagnosticEntry, DiagnosticLocation, DiagnosticNote, DiagnosticsBuilder,
    ErrorCode, Severity, error_code,
};
use cairo_lang_filesystem::db::Edition;
use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_filesystem::span::TextWidth;
use cairo_lang_parser::ParserDiagnostic;
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::helpers::GetIdentifier;
use itertools::Itertools;
use salsa::Database;
use syntax::node::ids::SyntaxStablePtrId;

use crate::corelib::LiteralError;
use crate::expr::inference::InferenceError;
use crate::items::feature_kind::FeatureMarkerDiagnostic;
use crate::items::trt::ConcreteTraitTypeId;
use crate::resolve::{ResolvedConcreteItem, ResolvedGenericItem};
use crate::types::peel_snapshots;
use crate::{ConcreteTraitId, semantic};

#[cfg(test)]
#[path = "diagnostic_test.rs"]
mod test;

pub type SemanticDiagnostics<'db> = DiagnosticsBuilder<'db, SemanticDiagnostic<'db>>;
pub trait SemanticDiagnosticsBuilder<'db> {
    /// Report a diagnostic in the location of the given ptr.
    fn report(
        &mut self,
        stable_ptr: impl Into<SyntaxStablePtrId<'db>>,
        kind: SemanticDiagnosticKind<'db>,
    ) -> DiagnosticAdded;
    /// Report a diagnostic in the location after the given ptr (with width 0).
    fn report_after(
        &mut self,
        stable_ptr: impl Into<SyntaxStablePtrId<'db>>,
        kind: SemanticDiagnosticKind<'db>,
    ) -> DiagnosticAdded;
    /// Report a diagnostic in a sub-span of the location of the given ptr. The inner span is
    /// specified by an offset from the start of the pointer location and a width.
    fn report_with_inner_span(
        &mut self,
        stable_ptr: impl Into<SyntaxStablePtrId<'db>>,
        inner_span: (TextWidth, TextWidth),
        kind: SemanticDiagnosticKind<'db>,
    ) -> DiagnosticAdded;
}
impl<'db> SemanticDiagnosticsBuilder<'db> for SemanticDiagnostics<'db> {
    fn report(
        &mut self,
        stable_ptr: impl Into<SyntaxStablePtrId<'db>>,
        kind: SemanticDiagnosticKind<'db>,
    ) -> DiagnosticAdded {
        self.add(SemanticDiagnostic::new(StableLocation::new(stable_ptr.into()), kind))
    }
    fn report_after(
        &mut self,
        stable_ptr: impl Into<SyntaxStablePtrId<'db>>,
        kind: SemanticDiagnosticKind<'db>,
    ) -> DiagnosticAdded {
        self.add(SemanticDiagnostic::new_after(StableLocation::new(stable_ptr.into()), kind))
    }
    fn report_with_inner_span(
        &mut self,
        stable_ptr: impl Into<SyntaxStablePtrId<'db>>,
        inner_span: (TextWidth, TextWidth),
        kind: SemanticDiagnosticKind<'db>,
    ) -> DiagnosticAdded {
        self.add(SemanticDiagnostic::new(
            StableLocation::with_inner_span(stable_ptr.into(), inner_span),
            kind,
        ))
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::Update)]
pub struct SemanticDiagnostic<'db> {
    pub stable_location: StableLocation<'db>,
    pub kind: SemanticDiagnosticKind<'db>,
    /// true if the diagnostic should be reported *after* the given location. Normally false, in
    /// which case the diagnostic points to the given location (as-is).
    pub after: bool,
}
impl<'db> SemanticDiagnostic<'db> {
    /// Create a diagnostic in the given location.
    pub fn new(stable_location: StableLocation<'db>, kind: SemanticDiagnosticKind<'db>) -> Self {
        SemanticDiagnostic { stable_location, kind, after: false }
    }
    /// Create a diagnostic in the location after the given location (with width 0).
    pub fn new_after(
        stable_location: StableLocation<'db>,
        kind: SemanticDiagnosticKind<'db>,
    ) -> Self {
        SemanticDiagnostic { stable_location, kind, after: true }
    }
}
impl<'db> DiagnosticEntry<'db> for SemanticDiagnostic<'db> {
    fn format(&self, db: &dyn Database) -> String {
        match &self.kind {
            SemanticDiagnosticKind::ModuleFileNotFound(path) => {
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
                format!(
                    "Impl item {item_kind} `{}::{}` is not a member of trait `{}`.",
                    impl_def_id.name(db).long(db),
                    impl_item_name.long(db),
                    trait_id.name(db).long(db)
                )
            }
            SemanticDiagnosticKind::ImplicitImplNotInferred {
                trait_impl_id,
                concrete_trait_id,
            } => {
                format!(
                    "Cannot infer implicit impl `{}.`\nCould not find implementation of trait \
                     `{:?}`",
                    trait_impl_id.name(db).long(db),
                    concrete_trait_id.debug(db)
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
                "Cycle detected while resolving type-alias/impl-type items.".into()
            }
            SemanticDiagnosticKind::ImplAliasCycle => {
                "Cycle detected while resolving 'impls alias' items.".into()
            }
            SemanticDiagnosticKind::ImplRequirementCycle => {
                "Cycle detected while resolving generic param. Try specifying the generic impl \
                 parameter explicitly to break the cycle."
                    .into()
            }
            SemanticDiagnosticKind::MissingMember(member_name) => {
                format!(r#"Missing member "{}"."#, member_name.long(db))
            }
            SemanticDiagnosticKind::WrongNumberOfParameters {
                impl_def_id,
                impl_function_id,
                trait_id,
                expected,
                actual,
            } => {
                let function_name = impl_function_id.name(db).long(db);
                format!(
                    "The number of parameters in the impl function `{}::{}` is incompatible with \
                     `{}::{}`. Expected: {}, actual: {}.",
                    impl_def_id.name(db).long(db),
                    function_name,
                    trait_id.name(db).long(db),
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
                let function_name = impl_function_id.name(db).long(db);
                format!(
                    "Parameter type of impl function `{}::{}` is incompatible with `{}::{}`. \
                     Expected: `{}`, actual: `{}`.",
                    impl_def_id.name(db).long(db),
                    function_name,
                    trait_id.name(db).long(db),
                    function_name,
                    expected_ty.format(db),
                    actual_ty.format(db)
                )
            }
            SemanticDiagnosticKind::VariantCtorNotImmutable => {
                "Variant constructor argument must be immutable.".to_string()
            }
            SemanticDiagnosticKind::TraitParamMutable { trait_id, function_id } => {
                format!(
                    "Parameter of trait function `{}::{}` can't be defined as mutable.",
                    trait_id.name(db).long(db),
                    function_id.name(db).long(db),
                )
            }
            SemanticDiagnosticKind::ParameterShouldBeReference {
                impl_def_id,
                impl_function_id,
                trait_id,
            } => {
                let function_name = impl_function_id.name(db).long(db);
                format!(
                    "Parameter of impl function {}::{} is incompatible with {}::{}. It should be \
                     a reference.",
                    impl_def_id.name(db).long(db),
                    function_name,
                    trait_id.name(db).long(db),
                    function_name,
                )
            }
            SemanticDiagnosticKind::ParameterShouldNotBeReference {
                impl_def_id,
                impl_function_id,
                trait_id,
            } => {
                let function_name = impl_function_id.name(db).long(db);
                format!(
                    "Parameter of impl function {}::{} is incompatible with {}::{}. It should not \
                     be a reference.",
                    impl_def_id.name(db).long(db),
                    function_name,
                    trait_id.name(db).long(db),
                    function_name,
                )
            }
            SemanticDiagnosticKind::WrongParameterName {
                impl_def_id,
                impl_function_id,
                trait_id,
                expected_name,
            } => {
                let function_name = impl_function_id.name(db).long(db);
                format!(
                    "Parameter name of impl function {}::{function_name} is incompatible with \
                     {}::{function_name} parameter `{}`.",
                    impl_def_id.name(db).long(db),
                    trait_id.name(db).long(db),
                    expected_name.long(db)
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
                        "{diagnostic_prefix}\nIt is possible that the type inference failed \
                         because the types differ in the number of snapshots.\nConsider adding or \
                         removing snapshots."
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
            SemanticDiagnosticKind::WrongExprType { expected_ty, actual_ty } => {
                format!(
                    r#"Unexpected expression type. Expected: "{}", found: "{}"."#,
                    expected_ty.format(db),
                    actual_ty.format(db)
                )
            }
            SemanticDiagnosticKind::WrongNumberOfGenericParamsForImplFunction {
                expected,
                actual,
            } => {
                format!(
                    "Wrong number of generic parameters for impl function. Expected: {expected}, \
                     found: {actual}."
                )
            }
            SemanticDiagnosticKind::WrongReturnTypeForImpl {
                impl_def_id,
                impl_function_id,
                trait_id,
                expected_ty,
                actual_ty,
            } => {
                let function_name = impl_function_id.name(db).long(db);
                format!(
                    "Return type of impl function `{}::{}` is incompatible with `{}::{}`. \
                     Expected: `{}`, actual: `{}`.",
                    impl_def_id.name(db).long(db),
                    function_name,
                    trait_id.name(db).long(db),
                    function_name,
                    expected_ty.format(db),
                    actual_ty.format(db)
                )
            }
            SemanticDiagnosticKind::WrongGenericParamTraitForImplFunction {
                impl_def_id,
                impl_function_id,
                trait_id,
                expected_trait,
                actual_trait,
            } => {
                let function_name = impl_function_id.name(db).long(db);
                format!(
                    "Generic parameter trait of impl function `{}::{}` is incompatible with \
                     `{}::{}`. Expected: `{:?}`, actual: `{:?}`.",
                    impl_def_id.name(db).long(db),
                    function_name,
                    trait_id.name(db).long(db),
                    function_name,
                    expected_trait.debug(db),
                    actual_trait.debug(db)
                )
            }
            SemanticDiagnosticKind::WrongGenericParamKindForImplFunction {
                impl_def_id,
                impl_function_id,
                trait_id,
                expected_kind,
                actual_kind,
            } => {
                let function_name = impl_function_id.name(db).long(db);
                format!(
                    "Generic parameter kind of impl function `{}::{}` is incompatible with \
                     `{}::{}`. Expected: `{:?}`, actual: `{:?}`.",
                    impl_def_id.name(db).long(db),
                    function_name,
                    trait_id.name(db).long(db),
                    function_name,
                    expected_kind,
                    actual_kind
                )
            }
            SemanticDiagnosticKind::AmbiguousTrait { trait_function_id0, trait_function_id1 } => {
                format!(
                    "Ambiguous method call. More than one applicable trait function with a \
                     suitable self type was found: {} and {}. Consider adding type annotations or \
                     explicitly refer to the impl function.",
                    trait_function_id0.full_path(db),
                    trait_function_id1.full_path(db)
                )
            }
            SemanticDiagnosticKind::VariableNotFound(name) => {
                format!(r#"Variable "{}" not found."#, name.long(db))
            }
            SemanticDiagnosticKind::MissingVariableInPattern => {
                "Missing variable in pattern.".into()
            }
            SemanticDiagnosticKind::VariableDefinedMultipleTimesInPattern(name) => {
                format!(r#"Redefinition of variable name "{}" in pattern."#, name.long(db))
            }
            SemanticDiagnosticKind::StructMemberRedefinition { struct_id, member_name } => {
                format!(
                    r#"Redefinition of member "{}" on struct "{}"."#,
                    member_name.long(db),
                    struct_id.full_path(db)
                )
            }
            SemanticDiagnosticKind::EnumVariantRedefinition { enum_id, variant_name } => {
                format!(
                    r#"Redefinition of variant "{}" on enum "{}"."#,
                    variant_name.long(db),
                    enum_id.full_path(db)
                )
            }
            SemanticDiagnosticKind::InfiniteSizeType(ty) => {
                format!(r#"Recursive type "{}" has infinite size."#, ty.format(db))
            }
            SemanticDiagnosticKind::ArrayOfZeroSizedElements(ty) => {
                format!(r#"Cannot have array of type "{}" that is zero sized."#, ty.format(db))
            }
            SemanticDiagnosticKind::ParamNameRedefinition { function_title_id, param_name } => {
                format!(
                    r#"Redefinition of parameter name "{}"{}"#,
                    param_name.long(db),
                    function_title_id
                        .map(|function_title_id| format!(
                            r#" in function "{}"."#,
                            function_title_id.full_path(db)
                        ))
                        .unwrap_or(".".into()),
                )
            }
            SemanticDiagnosticKind::ConditionNotBool(condition_ty) => {
                format!(r#"Condition has type "{}", expected bool."#, condition_ty.format(db))
            }
            SemanticDiagnosticKind::IncompatibleArms {
                multi_arm_expr_kind: incompatibility_kind,
                pending_ty: first_ty,
                different_ty,
            } => {
                let prefix = match incompatibility_kind {
                    MultiArmExprKind::Match => "Match arms have incompatible types",
                    MultiArmExprKind::If => "If blocks have incompatible types",
                    MultiArmExprKind::Loop => "Loop has incompatible return types",
                };
                format!(r#"{prefix}: "{}" and "{}""#, first_ty.format(db), different_ty.format(db))
            }
            SemanticDiagnosticKind::TypeHasNoMembers { ty, member_name: _ } => {
                format!(r#"Type "{}" has no members."#, ty.format(db))
            }
            SemanticDiagnosticKind::NoSuchStructMember { struct_id, member_name } => {
                format!(
                    r#"Struct "{}" has no member "{}""#,
                    struct_id.full_path(db),
                    member_name.long(db)
                )
            }
            SemanticDiagnosticKind::NoSuchTypeMember { ty, member_name } => {
                format!(r#"Type "{}" has no member "{}""#, ty.format(db), member_name.long(db))
            }
            SemanticDiagnosticKind::MemberNotVisible(member_name) => {
                format!(r#"Member "{}" is not visible in this context."#, member_name.long(db))
            }
            SemanticDiagnosticKind::NoSuchVariant { enum_id, variant_name } => {
                format!(
                    r#"Enum "{}" has no variant "{}""#,
                    enum_id.full_path(db),
                    variant_name.long(db)
                )
            }
            SemanticDiagnosticKind::ReturnTypeNotErrorPropagateType => {
                "`?` can only be used in a function with `Option` or `Result` return type.".into()
            }
            SemanticDiagnosticKind::IncompatibleErrorPropagateType { return_ty, err_ty } => {
                format!(
                    r#"Return type "{}" does not wrap error "{}""#,
                    return_ty.format(db),
                    err_ty.format(db)
                )
            }
            SemanticDiagnosticKind::ErrorPropagateOnNonErrorType(ty) => {
                format!(r#"Type "{}" can not error propagate"#, ty.format(db))
            }
            SemanticDiagnosticKind::UnhandledMustUseType(ty) => {
                format!(r#"Unhandled `#[must_use]` type `{}`"#, ty.format(db))
            }
            SemanticDiagnosticKind::UnhandledMustUseFunction => {
                "Unhandled `#[must_use]` function.".into()
            }
            SemanticDiagnosticKind::UnstableFeature { feature_name, note } => {
                format!(
                    "Usage of unstable feature `{0}` with no `#[feature({0})]` attribute.{1}",
                    feature_name.long(db),
                    note.as_ref()
                        .map(|note| format!(" Note: {}", note.long(db)))
                        .unwrap_or_default()
                )
            }
            SemanticDiagnosticKind::DeprecatedFeature { feature_name, note } => {
                format!(
                    "Usage of deprecated feature `{0}` with no `#[feature({0})]` attribute.{1}",
                    feature_name.long(db),
                    note.as_ref()
                        .map(|note| format!(" Note: {}", note.long(db)))
                        .unwrap_or_default()
                )
            }
            SemanticDiagnosticKind::InternalFeature { feature_name, note } => {
                format!(
                    "Usage of internal feature `{0}` with no `#[feature({0})]` attribute.{1}",
                    feature_name.long(db),
                    note.as_ref()
                        .map(|note| format!(" Note: {}", note.long(db)))
                        .unwrap_or_default()
                )
            }
            SemanticDiagnosticKind::FeatureMarkerDiagnostic(diagnostic) => match diagnostic {
                FeatureMarkerDiagnostic::MultipleMarkers => {
                    "Multiple feature marker attributes.".into()
                }
                FeatureMarkerDiagnostic::MissingAllowFeature => {
                    "Missing `feature` arg for feature marker attribute.".into()
                }
                FeatureMarkerDiagnostic::UnsupportedArgument => {
                    "Unsupported argument for feature marker attribute.".into()
                }
                FeatureMarkerDiagnostic::DuplicatedArgument => {
                    "Duplicated argument for feature marker attribute.".into()
                }
            },
            SemanticDiagnosticKind::UnusedVariable => {
                "Unused variable. Consider ignoring by prefixing with `_`.".into()
            }
            SemanticDiagnosticKind::UnusedConstant => {
                "Unused constant. Consider ignoring by prefixing with `_`.".into()
            }
            SemanticDiagnosticKind::MultipleConstantDefinition(constant_name) => {
                format!(r#"Multiple definitions of constant "{}"."#, constant_name.long(db))
            }
            SemanticDiagnosticKind::UnusedUse => "Unused use.".into(),
            SemanticDiagnosticKind::MultipleDefinitionforBinding(name) => {
                format!(
                    r#"Multiple definitions of identifier '{}' as constant and variable."#,
                    name.long(db)
                )
            }
            SemanticDiagnosticKind::MultipleGenericItemDefinition(type_name) => {
                format!(r#"Multiple definitions of an item "{}"."#, type_name.long(db))
            }
            SemanticDiagnosticKind::UnsupportedUseItemInStatement => {
                "Unsupported use item in statement.".into()
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
                NotFoundItemType::Macro => "Macro not found.".into(),
            },
            SemanticDiagnosticKind::AmbiguousPath(module_items) => {
                format!(
                    "Ambiguous path. Multiple matching items: {}",
                    module_items.iter().map(|item| format!("`{}`", item.full_path(db))).join(", ")
                )
            }
            SemanticDiagnosticKind::UseSelfNonMulti => {
                "`self` in `use` items is not allowed not in multi.".into()
            }
            SemanticDiagnosticKind::UseSelfEmptyPath => {
                "`self` in `use` items is not allowed for empty path.".into()
            }
            SemanticDiagnosticKind::UseStarEmptyPath => {
                "`*` in `use` items is not allowed for empty path.".into()
            }
            SemanticDiagnosticKind::GlobalUsesNotSupportedInEdition(edition) => {
                format!("Global `use` item is not supported in `{edition:?}` edition.")
            }
            SemanticDiagnosticKind::TraitInTraitMustBeExplicit => {
                "In a trait, paths of the same trait must be fully explicit. Either use `Self` if \
                 this is the intention, or explicitly specify all the generic arguments."
                    .to_string()
            }
            SemanticDiagnosticKind::ImplInImplMustBeExplicit => {
                "In an impl, paths of the same impl must be fully explicit. Either use `Self` if \
                 this is the intention, or explicitly specify all the generic arguments."
                    .to_string()
            }
            SemanticDiagnosticKind::TraitItemForbiddenInTheTrait => {
                "In a trait, paths of the same trait are not allowed. Did you mean to use `Self::`?"
                    .to_string()
            }
            SemanticDiagnosticKind::TraitItemForbiddenInItsImpl => "In an impl, paths of the \
                                                                    impl's trait are not allowed. \
                                                                    Did you mean to use `Self::`?"
                .to_string(),
            SemanticDiagnosticKind::ImplItemForbiddenInTheImpl => {
                "In an impl, paths of the same impl are not allowed. Did you mean to use `Self::`?"
                    .to_string()
            }
            SemanticDiagnosticKind::SuperUsedInRootModule => {
                "'super' cannot be used for the crate's root module.".into()
            }
            SemanticDiagnosticKind::SuperUsedInMacroCallTopLevel => {
                "`super` used in macro call top level.".into()
            }
            SemanticDiagnosticKind::ItemNotVisible(item_id, containing_modules) => {
                format!(
                    "Item `{}` is not visible in this context{}.",
                    item_id.full_path(db),
                    if containing_modules.is_empty() {
                        "".to_string()
                    } else if let [module_id] = &containing_modules[..] {
                        format!(" through module `{}`", module_id.full_path(db))
                    } else {
                        format!(
                            " through any of the modules: {}",
                            containing_modules
                                .iter()
                                .map(|module_id| format!("`{}`", module_id.full_path(db)))
                                .join(", ")
                        )
                    }
                )
            }
            SemanticDiagnosticKind::UnusedImport(use_id) => {
                format!("Unused import: `{}`", use_id.full_path(db))
            }
            SemanticDiagnosticKind::UnexpectedEnumPattern(ty) => {
                format!(r#"Unexpected type for enum pattern. "{}" is not an enum."#, ty.format(db),)
            }
            SemanticDiagnosticKind::UnexpectedStructPattern(ty) => {
                format!(
                    r#"Unexpected type for struct pattern. "{}" is not a struct."#,
                    ty.format(db),
                )
            }
            SemanticDiagnosticKind::UnexpectedTuplePattern(ty) => {
                format!(r#"Unexpected type for tuple pattern. "{}" is not a tuple."#, ty.format(db),)
            }
            SemanticDiagnosticKind::UnexpectedFixedSizeArrayPattern(ty) => {
                format!(
                    "Unexpected type for fixed size array pattern. \"{}\" is not a fixed size \
                     array.",
                    ty.format(db),
                )
            }
            SemanticDiagnosticKind::WrongNumberOfTupleElements { expected, actual } => format!(
                r#"Wrong number of tuple elements in pattern. Expected: {expected}. Got: {actual}."#,
            ),
            SemanticDiagnosticKind::WrongNumberOfFixedSizeArrayElements { expected, actual } => {
                format!(
                    "Wrong number of fixed size array elements in pattern. Expected: {expected}. \
                     Got: {actual}.",
                )
            }
            SemanticDiagnosticKind::WrongEnum { expected_enum, actual_enum } => {
                format!(
                    r#"Wrong enum in pattern. Expected: "{}". Got: "{}"."#,
                    expected_enum.full_path(db),
                    actual_enum.full_path(db)
                )
            }
            SemanticDiagnosticKind::RedundantModifier { current_modifier, previous_modifier } => {
                format!(
                    "`{}` modifier was specified after another modifier (`{}`). Only a single \
                     modifier is allowed.",
                    current_modifier.long(db),
                    previous_modifier.long(db)
                )
            }
            SemanticDiagnosticKind::ReferenceLocalVariable => {
                "`ref` is only allowed for function parameters, not for local variables."
                    .to_string()
            }
            SemanticDiagnosticKind::InvalidCopyTraitImpl(inference_error) => {
                format!("Invalid copy trait implementation, {}", inference_error.format(db))
            }
            SemanticDiagnosticKind::InvalidDropTraitImpl(inference_error) => {
                format!("Invalid drop trait implementation, {}", inference_error.format(db))
            }
            SemanticDiagnosticKind::InvalidImplItem(item_kw) => {
                format!("`{}` is not allowed inside impl.", item_kw.long(db))
            }
            SemanticDiagnosticKind::MissingItemsInImpl(item_names) => {
                format!(
                    "Not all trait items are implemented. Missing: {}.",
                    item_names.iter().map(|name| format!("'{}'", name.long(db))).join(", ")
                )
            }
            SemanticDiagnosticKind::PassPanicAsNopanic { impl_function_id, trait_id } => {
                let name = impl_function_id.name(db).long(db);
                let trait_name = trait_id.name(db).long(db);
                format!(
                    "The signature of function `{name}` is incompatible with trait \
                     `{trait_name}`. The trait function is declared as nopanic."
                )
            }
            SemanticDiagnosticKind::PassConstAsNonConst { impl_function_id, trait_id } => {
                let name = impl_function_id.name(db).long(db);
                let trait_name = trait_id.name(db).long(db);
                format!(
                    "The signature of function `{name}` is incompatible with trait \
                     `{trait_name}`. The trait function is declared as const."
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
            SemanticDiagnosticKind::MacroGeneratedCodeParserDiagnostic(parser_diagnostic) => {
                format!("Parser error in macro-expanded code: {}", parser_diagnostic.format(db))
            }
            SemanticDiagnosticKind::NameDefinedMultipleTimes(name) => {
                format!("The name `{}` is defined multiple times.", name.long(db))
            }
            SemanticDiagnosticKind::NonPrivateUseStar => {
                "`pub` not supported for global `use`.".into()
            }
            SemanticDiagnosticKind::SelfGlobalUse => {
                "cannot glob-import a module into itself".into()
            }
            SemanticDiagnosticKind::NamedArgumentsAreNotSupported => {
                "Named arguments are not supported in this context.".into()
            }
            SemanticDiagnosticKind::UnnamedArgumentFollowsNamed => {
                "Unnamed arguments cannot follow named arguments.".into()
            }
            SemanticDiagnosticKind::NamedArgumentMismatch { expected, found } => {
                format!(
                    "Unexpected argument name. Expected: '{}', found '{}'.",
                    expected.long(db),
                    found.long(db)
                )
            }
            SemanticDiagnosticKind::UnsupportedOutsideOfFunction(feature_name) => {
                let feature_name_str = match feature_name {
                    UnsupportedOutsideOfFunctionFeatureName::ReturnStatement => "Return statement",
                    UnsupportedOutsideOfFunctionFeatureName::ErrorPropagate => "The '?' operator",
                };
                format!("{feature_name_str} is not supported outside of functions.")
            }
            SemanticDiagnosticKind::UnsupportedConstant => {
                "This expression is not supported as constant.".into()
            }
            SemanticDiagnosticKind::FailedConstantCalculation => {
                "Failed to calculate constant.".into()
            }
            SemanticDiagnosticKind::ConstantCalculationDepthExceeded => {
                "Constant calculation depth exceeded.".into()
            }
            SemanticDiagnosticKind::InnerFailedConstantCalculation(inner, _) => inner.format(db),
            SemanticDiagnosticKind::DivisionByZero => "Division by zero.".into(),
            SemanticDiagnosticKind::ExternTypeWithImplGenericsNotSupported => {
                "Extern types with impl generics are not supported.".into()
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
            SemanticDiagnosticKind::DerefNonRef { ty } => {
                format!("Type `{}` cannot be dereferenced", ty.format(db))
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
            SemanticDiagnosticKind::CannotCallMethod {
                ty,
                method_name,
                inference_errors,
                relevant_traits,
            } => {
                if !inference_errors.is_empty() {
                    return format!(
                        "Method `{}` could not be called on type `{}`.\n{}",
                        method_name.long(db),
                        ty.format(db),
                        inference_errors.format(db)
                    );
                }
                if !relevant_traits.is_empty() {
                    let suggestions = relevant_traits
                        .iter()
                        .map(|trait_path| format!("`{trait_path}`"))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!(
                        "Method `{}` not found on type `{}`. Consider importing one of the \
                         following traits: {}.",
                        method_name.long(db),
                        ty.format(db),
                        suggestions
                    )
                } else {
                    format!(
                        "Method `{}` not found on type `{}`. Did you import the correct trait and \
                         impl?",
                        method_name.long(db),
                        ty.format(db)
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
            SemanticDiagnosticKind::UnsupportedAllowAttrArguments => {
                // TODO(orizi): Add information about the allowed arguments.
                "`allow` attribute argument not supported.".into()
            }
            SemanticDiagnosticKind::UnsupportedPubArgument => "Unsupported `pub` argument.".into(),
            SemanticDiagnosticKind::UnknownStatementAttribute => {
                "Unknown statement attribute.".into()
            }
            SemanticDiagnosticKind::InlineMacroNotFound(macro_name) => {
                format!("Inline macro `{}` not found.", macro_name.long(db))
            }
            SemanticDiagnosticKind::InlineMacroFailed(macro_name) => {
                format!("Inline macro `{}` failed.", macro_name.long(db))
            }
            SemanticDiagnosticKind::InlineMacroNoMatchingRule(macro_name) => {
                format!("No matching rule found in inline macro `{}`.", macro_name.long(db))
            }
            SemanticDiagnosticKind::MacroCallToNotAMacro(name) => {
                format!("Call to `{}` which is not a macro.", name.long(db))
            }
            SemanticDiagnosticKind::UnknownGenericParam(name) => {
                format!("Unknown generic parameter `{}`.", name.long(db))
            }
            SemanticDiagnosticKind::PositionalGenericAfterNamed => {
                "Positional generic parameters must come before named generic parameters.".into()
            }
            SemanticDiagnosticKind::GenericArgDuplicate(name) => {
                format!("Generic argument `{}` is specified more than once.", name.long(db))
            }
            SemanticDiagnosticKind::TooManyGenericArguments { expected, actual } => {
                format!("Expected {expected} generic arguments, found {actual}.")
            }
            SemanticDiagnosticKind::GenericArgOutOfOrder(name) => {
                format!("Generic argument `{}` is out of order.", name.long(db))
            }
            SemanticDiagnosticKind::ArgPassedToNegativeImpl => {
                "Only `_` is valid as a negative impl argument.".into()
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
            SemanticDiagnosticKind::ReprPtrsDisabled => {
                "Representation pointers are disabled in the current crate.\nYou can enable them \
                 by enabling the `repr_ptrs` experimental feature in the crate config."
                    .into()
            }
            SemanticDiagnosticKind::AssignmentToReprPtrVariable { .. } => {
                "Cannot assign to a variable with a taken pointer".into()
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
            SemanticDiagnosticKind::FixedSizeArraySizeTooBig => {
                "Fixed size array size must be smaller than 2^15.".into()
            }
            SemanticDiagnosticKind::SelfNotSupportedInContext => {
                "`Self` is not supported in this context.".into()
            }
            SemanticDiagnosticKind::SelfMustBeFirst => {
                "`Self` can only be the first segment of a path.".into()
            }
            SemanticDiagnosticKind::DollarNotSupportedInContext => {
                "`$` is not supported in this context.".into()
            }
            SemanticDiagnosticKind::UnknownResolverModifier { modifier } => {
                format!("`${}` is not supported.", modifier.long(db))
            }
            SemanticDiagnosticKind::EmptyPathAfterResolverModifier => {
                "Expected path after modifier.".into()
            }
            SemanticDiagnosticKind::CannotCreateInstancesOfPhantomTypes => {
                "Can not create instances of phantom types.".into()
            }
            SemanticDiagnosticKind::NonPhantomTypeContainingPhantomType => {
                "Non-phantom type containing phantom type.".into()
            }
            SemanticDiagnosticKind::DerefCycle { deref_chain } => {
                format!("Deref impls cycle detected:\n{deref_chain}")
            }
            SemanticDiagnosticKind::NoImplementationOfTrait {
                ty,
                trait_name,
                inference_errors,
            } => {
                if inference_errors.is_empty() {
                    format!(
                        "Implementation of trait `{}` not found on type `{}`. Did you import the \
                         correct trait and impl?",
                        trait_name.long(db),
                        ty.format(db)
                    )
                } else {
                    format!(
                        "Could not find implementation of trait `{}` on type `{}`.\n{}",
                        trait_name.long(db),
                        ty.format(db),
                        inference_errors.format(db)
                    )
                }
            }
            SemanticDiagnosticKind::CallExpressionRequiresFunction { ty, inference_errors } => {
                if inference_errors.is_empty() {
                    format!("Call expression requires a function, found `{}`.", ty.format(db))
                } else {
                    format!(
                        "Call expression requires a function, found `{}`.\n{}",
                        ty.format(db),
                        inference_errors.format(db)
                    )
                }
            }
            SemanticDiagnosticKind::CompilerTraitReImplementation { trait_id } => {
                format!(
                    "Trait `{}` should not be implemented outside of the corelib.",
                    trait_id.full_path(db)
                )
            }
            SemanticDiagnosticKind::ClosureInGlobalScope => {
                "Closures are not allowed in this context.".into()
            }
            SemanticDiagnosticKind::MaybeMissingColonColon => "Are you missing a `::`?.".into(),
            SemanticDiagnosticKind::CallingShadowedFunction { shadowed_function_name } => {
                format!(
                    "Function `{}` is shadowed by a local variable.",
                    shadowed_function_name.long(db)
                )
            }
            SemanticDiagnosticKind::RefClosureArgument => {
                "Arguments to closure functions cannot be references".into()
            }
            SemanticDiagnosticKind::RefClosureParam => {
                "Closure parameters cannot be references".into()
            }
            SemanticDiagnosticKind::MustBeNextToTypeOrTrait { trait_id } => {
                format!(
                    "'{}' implementation must be defined in the same module as either the type \
                     being dereferenced or the trait itself",
                    trait_id.name(db).long(db)
                )
            }
            SemanticDiagnosticKind::MutableCapturedVariable => {
                "Capture of mutable variables in a closure is not supported".into()
            }
            SemanticDiagnosticKind::NonTraitTypeConstrained { identifier, concrete_trait_id } => {
                format!(
                    "associated type `{}` not found for `{}`",
                    identifier.long(db),
                    concrete_trait_id.full_path(db)
                )
            }
            SemanticDiagnosticKind::DuplicateTypeConstraint {
                concrete_trait_type_id: trait_type_id,
            } => {
                format!(
                    "the value of the associated type `{}` in trait `{}` is already specified",
                    trait_type_id.trait_type(db).name(db).long(db),
                    trait_type_id.concrete_trait(db).full_path(db)
                )
            }
            SemanticDiagnosticKind::TypeConstraintsSyntaxNotEnabled => {
                "Type constraints syntax is not enabled in the current crate.".into()
            }
            SemanticDiagnosticKind::PatternMissingArgs(path) => {
                format!(
                    "Pattern missing subpattern for the payload of variant. Consider using `{}(_)`",
                    path.segments(db)
                        .elements(db)
                        .map(|seg| seg.identifier(db).long(db))
                        .join("::")
                )
            }
            SemanticDiagnosticKind::UndefinedMacroPlaceholder(name) => {
                format!("Undefined macro placeholder: '{}'.", name.long(db))
            }
            SemanticDiagnosticKind::UserDefinedInlineMacrosDisabled => {
                "User defined inline macros are disabled in the current crate.".into()
            }
            SemanticDiagnosticKind::NonNeverLetElseType => concat!(
                "`else` clause of `let...else` must exit the scope. ",
                "Consider using `return`, `continue`, ..."
            )
            .into(),
        }
    }
    fn location(&self, db: &'db dyn Database) -> DiagnosticLocation<'db> {
        if let SemanticDiagnosticKind::MacroGeneratedCodeParserDiagnostic(parser_diagnostic) =
            &self.kind
        {
            return DiagnosticLocation {
                file_id: parser_diagnostic.file_id,
                span: parser_diagnostic.span,
            };
        };

        let mut location = self.stable_location.diagnostic_location(db);
        if self.after {
            location = location.after();
        }
        location
    }

    fn severity(&self) -> Severity {
        match &self.kind {
            SemanticDiagnosticKind::UnusedVariable
            | SemanticDiagnosticKind::UnhandledMustUseType { .. }
            | SemanticDiagnosticKind::UnhandledMustUseFunction
            | SemanticDiagnosticKind::TraitInTraitMustBeExplicit
            | SemanticDiagnosticKind::ImplInImplMustBeExplicit
            | SemanticDiagnosticKind::TraitItemForbiddenInTheTrait
            | SemanticDiagnosticKind::TraitItemForbiddenInItsImpl
            | SemanticDiagnosticKind::ImplItemForbiddenInTheImpl
            | SemanticDiagnosticKind::UnstableFeature { .. }
            | SemanticDiagnosticKind::DeprecatedFeature { .. }
            | SemanticDiagnosticKind::UnusedImport { .. }
            | SemanticDiagnosticKind::CallingShadowedFunction { .. }
            | SemanticDiagnosticKind::UnusedConstant
            | SemanticDiagnosticKind::UnusedUse
            | SemanticDiagnosticKind::PatternMissingArgs(_)
            | SemanticDiagnosticKind::UnsupportedAllowAttrArguments => Severity::Warning,
            SemanticDiagnosticKind::PluginDiagnostic(diag) => diag.severity,
            _ => Severity::Error,
        }
    }

    fn notes(&self, _db: &dyn Database) -> &[DiagnosticNote<'_>] {
        match &self.kind {
            SemanticDiagnosticKind::InnerFailedConstantCalculation(_, notes) => notes,
            SemanticDiagnosticKind::AssignmentToReprPtrVariable(notes) => notes,
            _ => &[],
        }
    }

    fn error_code(&self) -> Option<ErrorCode> {
        self.kind.error_code()
    }

    fn is_same_kind(&self, other: &Self) -> bool {
        other.kind == self.kind
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::Update)]
pub enum SemanticDiagnosticKind<'db> {
    ModuleFileNotFound(String),
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
    LiteralError(LiteralError<'db>),
    NotAVariant,
    NotAStruct,
    NotAType,
    NotATrait,
    NotAnImpl,
    ImplItemNotInTrait {
        impl_def_id: ImplDefId<'db>,
        impl_item_name: SmolStrId<'db>,
        trait_id: TraitId<'db>,
        item_kind: String,
    },
    ImplicitImplNotInferred {
        trait_impl_id: TraitImplId<'db>,
        concrete_trait_id: ConcreteTraitId<'db>,
    },
    GenericsNotSupportedInItem {
        scope: String,
        item_kind: String,
    },
    UnexpectedGenericArgs,
    UnknownMember,
    CannotCreateInstancesOfPhantomTypes,
    NonPhantomTypeContainingPhantomType,
    MemberSpecifiedMoreThanOnce,
    StructBaseStructExpressionNotLast,
    StructBaseStructExpressionNoEffect,
    ConstCycle,
    UseCycle,
    TypeAliasCycle,
    ImplAliasCycle,
    ImplRequirementCycle,
    MissingMember(SmolStrId<'db>),
    WrongNumberOfParameters {
        impl_def_id: ImplDefId<'db>,
        impl_function_id: ImplFunctionId<'db>,
        trait_id: TraitId<'db>,
        expected: usize,
        actual: usize,
    },
    WrongNumberOfArguments {
        expected: usize,
        actual: usize,
    },
    WrongParameterType {
        impl_def_id: ImplDefId<'db>,
        impl_function_id: ImplFunctionId<'db>,
        trait_id: TraitId<'db>,
        expected_ty: semantic::TypeId<'db>,
        actual_ty: semantic::TypeId<'db>,
    },
    VariantCtorNotImmutable,
    TraitParamMutable {
        trait_id: TraitId<'db>,
        function_id: TraitFunctionId<'db>,
    },
    ParameterShouldBeReference {
        impl_def_id: ImplDefId<'db>,
        impl_function_id: ImplFunctionId<'db>,
        trait_id: TraitId<'db>,
    },
    ParameterShouldNotBeReference {
        impl_def_id: ImplDefId<'db>,
        impl_function_id: ImplFunctionId<'db>,
        trait_id: TraitId<'db>,
    },
    WrongParameterName {
        impl_def_id: ImplDefId<'db>,
        impl_function_id: ImplFunctionId<'db>,
        trait_id: TraitId<'db>,
        expected_name: SmolStrId<'db>,
    },
    WrongGenericParamTraitForImplFunction {
        impl_def_id: ImplDefId<'db>,
        impl_function_id: ImplFunctionId<'db>,
        trait_id: TraitId<'db>,
        expected_trait: ConcreteTraitId<'db>,
        actual_trait: ConcreteTraitId<'db>,
    },
    WrongGenericParamKindForImplFunction {
        impl_def_id: ImplDefId<'db>,
        impl_function_id: ImplFunctionId<'db>,
        trait_id: TraitId<'db>,
        expected_kind: GenericKind,
        actual_kind: GenericKind,
    },
    WrongType {
        expected_ty: semantic::TypeId<'db>,
        actual_ty: semantic::TypeId<'db>,
    },
    InconsistentBinding,
    WrongArgumentType {
        expected_ty: semantic::TypeId<'db>,
        actual_ty: semantic::TypeId<'db>,
    },
    WrongReturnType {
        expected_ty: semantic::TypeId<'db>,
        actual_ty: semantic::TypeId<'db>,
    },
    WrongExprType {
        expected_ty: semantic::TypeId<'db>,
        actual_ty: semantic::TypeId<'db>,
    },
    WrongNumberOfGenericParamsForImplFunction {
        expected: usize,
        actual: usize,
    },
    WrongReturnTypeForImpl {
        impl_def_id: ImplDefId<'db>,
        impl_function_id: ImplFunctionId<'db>,
        trait_id: TraitId<'db>,
        expected_ty: semantic::TypeId<'db>,
        actual_ty: semantic::TypeId<'db>,
    },
    AmbiguousTrait {
        trait_function_id0: TraitFunctionId<'db>,
        trait_function_id1: TraitFunctionId<'db>,
    },
    VariableNotFound(SmolStrId<'db>),
    MissingVariableInPattern,
    VariableDefinedMultipleTimesInPattern(SmolStrId<'db>),
    StructMemberRedefinition {
        struct_id: StructId<'db>,
        member_name: SmolStrId<'db>,
    },
    EnumVariantRedefinition {
        enum_id: EnumId<'db>,
        variant_name: SmolStrId<'db>,
    },
    InfiniteSizeType(semantic::TypeId<'db>),
    ArrayOfZeroSizedElements(semantic::TypeId<'db>),
    ParamNameRedefinition {
        function_title_id: Option<FunctionTitleId<'db>>,
        param_name: SmolStrId<'db>,
    },
    ConditionNotBool(semantic::TypeId<'db>),
    IncompatibleArms {
        multi_arm_expr_kind: MultiArmExprKind,
        pending_ty: semantic::TypeId<'db>,
        different_ty: semantic::TypeId<'db>,
    },
    TypeHasNoMembers {
        ty: semantic::TypeId<'db>,
        member_name: SmolStrId<'db>,
    },
    CannotCallMethod {
        ty: semantic::TypeId<'db>,
        method_name: SmolStrId<'db>,
        inference_errors: TraitInferenceErrors<'db>,
        relevant_traits: Vec<String>,
    },
    NoSuchStructMember {
        struct_id: StructId<'db>,
        member_name: SmolStrId<'db>,
    },
    NoSuchTypeMember {
        ty: semantic::TypeId<'db>,
        member_name: SmolStrId<'db>,
    },
    MemberNotVisible(SmolStrId<'db>),
    NoSuchVariant {
        enum_id: EnumId<'db>,
        variant_name: SmolStrId<'db>,
    },
    ReturnTypeNotErrorPropagateType,
    IncompatibleErrorPropagateType {
        return_ty: semantic::TypeId<'db>,
        err_ty: semantic::TypeId<'db>,
    },
    ErrorPropagateOnNonErrorType(semantic::TypeId<'db>),
    UnhandledMustUseType(semantic::TypeId<'db>),
    UnstableFeature {
        feature_name: SmolStrId<'db>,
        note: Option<SmolStrId<'db>>,
    },
    DeprecatedFeature {
        feature_name: SmolStrId<'db>,
        note: Option<SmolStrId<'db>>,
    },
    InternalFeature {
        feature_name: SmolStrId<'db>,
        note: Option<SmolStrId<'db>>,
    },
    FeatureMarkerDiagnostic(FeatureMarkerDiagnostic),
    UnhandledMustUseFunction,
    UnusedVariable,
    UnusedConstant,
    UnusedUse,
    MultipleConstantDefinition(SmolStrId<'db>),
    MultipleDefinitionforBinding(SmolStrId<'db>),
    MultipleGenericItemDefinition(SmolStrId<'db>),
    UnsupportedUseItemInStatement,
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
    AmbiguousPath(Vec<ModuleItemId<'db>>),
    UseSelfNonMulti,
    UseSelfEmptyPath,
    UseStarEmptyPath,
    GlobalUsesNotSupportedInEdition(Edition),
    TraitInTraitMustBeExplicit,
    ImplInImplMustBeExplicit,
    TraitItemForbiddenInTheTrait,
    TraitItemForbiddenInItsImpl,
    ImplItemForbiddenInTheImpl,
    SuperUsedInRootModule,
    SuperUsedInMacroCallTopLevel,
    ItemNotVisible(ModuleItemId<'db>, Vec<ModuleId<'db>>),
    UnusedImport(UseId<'db>),
    RedundantModifier {
        current_modifier: SmolStrId<'db>,
        previous_modifier: SmolStrId<'db>,
    },
    ReferenceLocalVariable,
    UnexpectedEnumPattern(semantic::TypeId<'db>),
    UnexpectedStructPattern(semantic::TypeId<'db>),
    UnexpectedTuplePattern(semantic::TypeId<'db>),
    UnexpectedFixedSizeArrayPattern(semantic::TypeId<'db>),
    WrongNumberOfTupleElements {
        expected: usize,
        actual: usize,
    },
    WrongNumberOfFixedSizeArrayElements {
        expected: usize,
        actual: usize,
    },
    WrongEnum {
        expected_enum: EnumId<'db>,
        actual_enum: EnumId<'db>,
    },
    InvalidCopyTraitImpl(InferenceError<'db>),
    InvalidDropTraitImpl(InferenceError<'db>),
    InvalidImplItem(SmolStrId<'db>),
    MissingItemsInImpl(Vec<SmolStrId<'db>>),
    PassPanicAsNopanic {
        impl_function_id: ImplFunctionId<'db>,
        trait_id: TraitId<'db>,
    },
    PassConstAsNonConst {
        impl_function_id: ImplFunctionId<'db>,
        trait_id: TraitId<'db>,
    },
    PanicableFromNonPanicable,
    PanicableExternFunction,
    MacroGeneratedCodeParserDiagnostic(ParserDiagnostic<'db>),
    PluginDiagnostic(PluginDiagnostic<'db>),
    NameDefinedMultipleTimes(SmolStrId<'db>),
    NonPrivateUseStar,
    SelfGlobalUse,
    NamedArgumentsAreNotSupported,
    ArgPassedToNegativeImpl,
    UnnamedArgumentFollowsNamed,
    NamedArgumentMismatch {
        expected: SmolStrId<'db>,
        found: SmolStrId<'db>,
    },
    UnsupportedOutsideOfFunction(UnsupportedOutsideOfFunctionFeatureName),
    UnsupportedConstant,
    FailedConstantCalculation,
    ConstantCalculationDepthExceeded,
    InnerFailedConstantCalculation(Box<SemanticDiagnostic<'db>>, Vec<DiagnosticNote<'db>>),
    DivisionByZero,
    ExternTypeWithImplGenericsNotSupported,
    MissingSemicolon,
    TraitMismatch {
        expected_trt: semantic::ConcreteTraitId<'db>,
        actual_trt: semantic::ConcreteTraitId<'db>,
    },
    DerefNonRef {
        ty: semantic::TypeId<'db>,
    },
    InternalInferenceError(InferenceError<'db>),
    NoImplementationOfIndexOperator {
        ty: semantic::TypeId<'db>,
        inference_errors: TraitInferenceErrors<'db>,
    },
    NoImplementationOfTrait {
        ty: semantic::TypeId<'db>,
        trait_name: SmolStrId<'db>,
        inference_errors: TraitInferenceErrors<'db>,
    },
    CallExpressionRequiresFunction {
        ty: semantic::TypeId<'db>,
        inference_errors: TraitInferenceErrors<'db>,
    },
    MultipleImplementationOfIndexOperator(semantic::TypeId<'db>),

    UnsupportedInlineArguments,
    RedundantInlineAttribute,
    InlineAttrForExternFunctionNotAllowed,
    InlineAlwaysWithImplGenericArgNotAllowed,
    TailExpressionNotAllowedInLoop,
    ContinueOnlyAllowedInsideALoop,
    BreakOnlyAllowedInsideALoop,
    BreakWithValueOnlyAllowedInsideALoop,
    ErrorPropagateNotAllowedInsideALoop,
    ImplicitPrecedenceAttrForExternFunctionNotAllowed,
    RedundantImplicitPrecedenceAttribute,
    UnsupportedImplicitPrecedenceArguments,
    UnsupportedFeatureAttrArguments,
    UnsupportedAllowAttrArguments,
    UnsupportedPubArgument,
    UnknownStatementAttribute,
    InlineMacroNotFound(SmolStrId<'db>),
    InlineMacroFailed(SmolStrId<'db>),
    InlineMacroNoMatchingRule(SmolStrId<'db>),
    MacroCallToNotAMacro(SmolStrId<'db>),
    UnknownGenericParam(SmolStrId<'db>),
    PositionalGenericAfterNamed,
    GenericArgDuplicate(SmolStrId<'db>),
    TooManyGenericArguments {
        expected: usize,
        actual: usize,
    },
    GenericArgOutOfOrder(SmolStrId<'db>),
    CouponForExternFunctionNotAllowed,
    CouponArgumentNoModifiers,
    /// Coupons are disabled in the current crate.
    CouponsDisabled,
    /// Representation pointers are disabled in the current crate.
    ReprPtrsDisabled,
    /// Cannot assign to a variable with a taken pointer.
    AssignmentToReprPtrVariable(Vec<DiagnosticNote<'db>>),
    FixedSizeArrayTypeNonSingleType,
    FixedSizeArrayTypeEmptySize,
    FixedSizeArrayNonNumericSize,
    FixedSizeArrayNonSingleValue,
    FixedSizeArraySizeTooBig,
    SelfNotSupportedInContext,
    SelfMustBeFirst,
    DollarNotSupportedInContext,
    UnknownResolverModifier {
        modifier: SmolStrId<'db>,
    },
    EmptyPathAfterResolverModifier,
    DerefCycle {
        deref_chain: String,
    },
    CompilerTraitReImplementation {
        trait_id: TraitId<'db>,
    },
    ClosureInGlobalScope,
    MaybeMissingColonColon,
    CallingShadowedFunction {
        shadowed_function_name: SmolStrId<'db>,
    },
    RefClosureArgument,
    RefClosureParam,
    MustBeNextToTypeOrTrait {
        trait_id: TraitId<'db>,
    },
    MutableCapturedVariable,
    NonTraitTypeConstrained {
        identifier: SmolStrId<'db>,
        concrete_trait_id: ConcreteTraitId<'db>,
    },
    DuplicateTypeConstraint {
        concrete_trait_type_id: ConcreteTraitTypeId<'db>,
    },
    TypeConstraintsSyntaxNotEnabled,
    PatternMissingArgs(ast::ExprPath<'db>),
    UndefinedMacroPlaceholder(SmolStrId<'db>),
    UserDefinedInlineMacrosDisabled,
    NonNeverLetElseType,
}

/// The kind of an expression with multiple possible return types.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, salsa::Update)]
pub enum MultiArmExprKind {
    If,
    Match,
    Loop,
}

impl<'db> SemanticDiagnosticKind<'db> {
    pub fn error_code(&self) -> Option<ErrorCode> {
        Some(match &self {
            Self::UnusedVariable => error_code!(E0001),
            Self::CannotCallMethod { .. } => {
                error_code!(E0002)
            }
            Self::MissingMember(_) => error_code!(E0003),
            Self::MissingItemsInImpl(_) => error_code!(E0004),
            Self::ModuleFileNotFound(_) => error_code!(E0005),
            Self::PathNotFound(_) => error_code!(E0006),
            Self::NoSuchTypeMember { .. } => error_code!(E0007),
            _ => return None,
        })
    }
}

// TODO(Gil): It seems to have the same functionality as ElementKind, maybe we can merge them.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, salsa::Update)]
pub enum NotFoundItemType {
    Identifier,
    Function,
    Type,
    Trait,
    Impl,
    Macro,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, salsa::Update)]
pub enum UnsupportedOutsideOfFunctionFeatureName {
    ReturnStatement,
    ErrorPropagate,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, salsa::Update)]
pub enum ElementKind {
    Constant,
    Variable,
    Module,
    Function,
    Type,
    Variant,
    Trait,
    Impl,
    Macro,
}
impl<'db> From<&ResolvedConcreteItem<'db>> for ElementKind {
    fn from(val: &ResolvedConcreteItem<'db>) -> Self {
        match val {
            ResolvedConcreteItem::Constant(_) => ElementKind::Constant,
            ResolvedConcreteItem::Module(_) => ElementKind::Module,
            ResolvedConcreteItem::Function(_) => ElementKind::Function,
            ResolvedConcreteItem::Type(_) => ElementKind::Type,
            ResolvedConcreteItem::Variant(_) => ElementKind::Variant,
            ResolvedConcreteItem::Trait(_) | ResolvedConcreteItem::SelfTrait(_) => {
                ElementKind::Trait
            }
            ResolvedConcreteItem::Impl(_) => ElementKind::Impl,
            ResolvedConcreteItem::Macro(_) => ElementKind::Macro,
        }
    }
}
impl<'db> From<&ResolvedGenericItem<'db>> for ElementKind {
    fn from(val: &ResolvedGenericItem<'db>) -> Self {
        match val {
            ResolvedGenericItem::GenericConstant(_)
            | ResolvedGenericItem::TraitItem(TraitItemId::Constant(_)) => ElementKind::Constant,
            ResolvedGenericItem::Module(_) => ElementKind::Module,
            ResolvedGenericItem::GenericFunction(_)
            | ResolvedGenericItem::TraitItem(TraitItemId::Function(_)) => ElementKind::Function,
            ResolvedGenericItem::GenericType(_)
            | ResolvedGenericItem::GenericTypeAlias(_)
            | ResolvedGenericItem::TraitItem(TraitItemId::Type(_)) => ElementKind::Type,
            ResolvedGenericItem::Variant(_) => ElementKind::Variant,
            ResolvedGenericItem::Trait(_) => ElementKind::Trait,
            ResolvedGenericItem::Impl(_)
            | ResolvedGenericItem::GenericImplAlias(_)
            | ResolvedGenericItem::TraitItem(TraitItemId::Impl(_)) => ElementKind::Impl,
            ResolvedGenericItem::Variable(_) => ElementKind::Variable,
            ResolvedGenericItem::Macro(_) => ElementKind::Macro,
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
            ElementKind::Type => "type",
            ElementKind::Variant => "variant",
            ElementKind::Trait => "trait",
            ElementKind::Impl => "impl",
            ElementKind::Macro => "macro",
        };
        write!(f, "{res}")
    }
}

/// A list of trait functions and the inference errors that occurred while trying to infer them.
#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::Update)]
pub struct TraitInferenceErrors<'db> {
    pub traits_and_errors: Vec<(TraitFunctionId<'db>, InferenceError<'db>)>,
}
impl<'db> TraitInferenceErrors<'db> {
    /// Is the error list empty.
    pub fn is_empty(&self) -> bool {
        self.traits_and_errors.is_empty()
    }
    /// Format the list of errors.
    fn format(&self, db: &dyn Database) -> String {
        self.traits_and_errors
            .iter()
            .map(|(trait_function_id, inference_error)| {
                format!(
                    "Candidate `{}` inference failed with: {}",
                    trait_function_id.full_path(db),
                    inference_error.format(db)
                )
            })
            .join("\n")
    }
}
