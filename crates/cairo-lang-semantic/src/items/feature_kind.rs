use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{LanguageElementId, ModuleId};
use cairo_lang_diagnostics::DiagnosticsBuilder;
use cairo_lang_syntax::attribute::consts::{
    DEPRECATED_ATTR, FEATURE_ATTR, INTERNAL_ATTR, UNSTABLE_ATTR,
};
use cairo_lang_syntax::attribute::structured::{
    self, AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, Terminal, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use smol_str::SmolStr;

use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::SemanticDiagnostic;

/// The kind of a feature for an item.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FeatureKind {
    /// The feature of the item is stable.
    Stable,
    /// The feature of the item is unstable, with the given name to allow.
    Unstable { feature: SmolStr, note: Option<SmolStr> },
    /// The feature of the item is deprecated, with the given name to allow, and an optional note
    /// to appear in diagnostics.
    Deprecated { feature: SmolStr, note: Option<SmolStr> },
    /// This feature is for internal corelib use only. Using it in user code is not advised.
    Internal { feature: SmolStr, note: Option<SmolStr> },
}
impl FeatureKind {
    pub fn from_ast(
        db: &dyn SyntaxGroup,
        diagnostics: &mut DiagnosticsBuilder<SemanticDiagnostic>,
        attrs: &ast::AttributeList,
    ) -> Self {
        let unstable_attrs = attrs.query_attr(db, UNSTABLE_ATTR);
        let deprecated_attrs = attrs.query_attr(db, DEPRECATED_ATTR);
        let internal_attrs = attrs.query_attr(db, INTERNAL_ATTR);
        if unstable_attrs.is_empty() && deprecated_attrs.is_empty() && internal_attrs.is_empty() {
            return Self::Stable;
        };
        if unstable_attrs.len() + deprecated_attrs.len() + internal_attrs.len() > 1 {
            add_diag(diagnostics, &attrs.stable_ptr(), FeatureMarkerDiagnostic::MultipleMarkers);
            return Self::Stable;
        }

        if !unstable_attrs.is_empty() {
            let attr = unstable_attrs.into_iter().next().unwrap().structurize(db);
            let [feature, note, _] =
                parse_feature_attr(db, diagnostics, &attr, ["feature", "note", "since"]);
            feature.map(|feature| Self::Unstable { feature, note }).ok_or(attr)
        } else if !deprecated_attrs.is_empty() {
            let attr = deprecated_attrs.into_iter().next().unwrap().structurize(db);
            let [feature, note, _] =
                parse_feature_attr(db, diagnostics, &attr, ["feature", "note", "since"]);
            feature.map(|feature| Self::Deprecated { feature, note }).ok_or(attr)
        } else {
            let attr = internal_attrs.into_iter().next().unwrap().structurize(db);
            let [feature, note, _] =
                parse_feature_attr(db, diagnostics, &attr, ["feature", "note", "since"]);
            feature.map(|feature| Self::Internal { feature, note }).ok_or(attr)
        }
        .unwrap_or_else(|attr| {
            add_diag(diagnostics, &attr.stable_ptr, FeatureMarkerDiagnostic::MissingAllowFeature);
            Self::Stable
        })
    }
}

/// Diagnostics for feature markers.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum FeatureMarkerDiagnostic {
    /// Multiple markers on the same item.
    MultipleMarkers,
    /// Every marker must have a feature argument, to allow ignoring the warning.
    MissingAllowFeature,
    /// Unsupported argument in the feature marker attribute.
    UnsupportedArgument,
    /// Duplicated argument in the feature marker attribute.
    DuplicatedArgument,
}

/// Parses the feature attribute.
fn parse_feature_attr<const EXTRA_ALLOWED: usize>(
    db: &dyn SyntaxGroup,
    diagnostics: &mut DiagnosticsBuilder<SemanticDiagnostic>,
    attr: &structured::Attribute,
    allowed_args: [&str; EXTRA_ALLOWED],
) -> [Option<SmolStr>; EXTRA_ALLOWED] {
    let mut arg_values = std::array::from_fn(|_| None);
    for AttributeArg { variant, arg, .. } in &attr.args {
        let AttributeArgVariant::Named { value: ast::Expr::String(value), name } = variant else {
            add_diag(diagnostics, &arg.stable_ptr(), FeatureMarkerDiagnostic::UnsupportedArgument);
            continue;
        };
        let Some(i) = allowed_args.iter().position(|x| x == &name.text.as_str()) else {
            add_diag(diagnostics, &name.stable_ptr, FeatureMarkerDiagnostic::UnsupportedArgument);
            continue;
        };
        if arg_values[i].is_some() {
            add_diag(diagnostics, &name.stable_ptr, FeatureMarkerDiagnostic::DuplicatedArgument);
        } else {
            arg_values[i] = Some(value.text(db));
        }
    }
    arg_values
}

/// Helper for adding a marker diagnostic.
fn add_diag(
    diagnostics: &mut DiagnosticsBuilder<SemanticDiagnostic>,
    stable_ptr: &impl TypedStablePtr,
    diagnostic: FeatureMarkerDiagnostic,
) {
    diagnostics.add(SemanticDiagnostic::new(
        StableLocation::new(stable_ptr.untyped()),
        SemanticDiagnosticKind::FeatureMarkerDiagnostic(diagnostic),
    ));
}

/// Returns the allowed features of an object which supports attributes.
pub fn extract_item_allowed_features(
    db: &dyn SyntaxGroup,
    syntax: &impl QueryAttrs,
    diagnostics: &mut SemanticDiagnostics,
) -> OrderedHashSet<SmolStr> {
    let mut features = OrderedHashSet::default();
    for attr_syntax in syntax.query_attr(db, FEATURE_ATTR) {
        let attr = attr_syntax.structurize(db);
        let feature_name = match &attr.args[..] {
            [
                AttributeArg {
                    variant: AttributeArgVariant::Unnamed(ast::Expr::String(value)),
                    ..
                },
            ] => value.text(db),
            _ => {
                diagnostics.report(
                    attr.args_stable_ptr.untyped(),
                    SemanticDiagnosticKind::UnsupportedFeatureAttrArguments,
                );
                continue;
            }
        };
        features.insert(feature_name);
    }
    features
}

/// Extracts the allowed features of an element, considering its parent modules as well as its
/// attributes.
pub fn extract_allowed_features(
    db: &dyn DefsGroup,
    element_id: &impl LanguageElementId,
    syntax: &impl QueryAttrs,
    diagnostics: &mut SemanticDiagnostics,
) -> OrderedHashSet<SmolStr> {
    let syntax_db = db.upcast();
    let mut allowed_features = extract_item_allowed_features(syntax_db, syntax, diagnostics);
    let ignored_diagnostics = &mut SemanticDiagnostics::default();
    let mut curr_module_id = element_id.parent_module(db);
    loop {
        let submodule_id = match curr_module_id {
            ModuleId::CrateRoot(_) => break,
            ModuleId::Submodule(id) => id,
        };
        let parent = submodule_id.parent_module(db);
        let module = &db.module_submodules(parent).unwrap()[&submodule_id];
        // TODO(orizi): Add parent module diagnostics.
        for allowed_feature in extract_item_allowed_features(syntax_db, module, ignored_diagnostics)
        {
            allowed_features.insert(allowed_feature);
        }
        curr_module_id = parent;
    }
    allowed_features
}
