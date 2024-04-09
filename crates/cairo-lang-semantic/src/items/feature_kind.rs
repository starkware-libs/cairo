use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{LanguageElementId, ModuleId};
use cairo_lang_diagnostics::DiagnosticsBuilder;
use cairo_lang_syntax::attribute::consts::{FEATURE_ATTR, UNSTABLE_ATTR};
use cairo_lang_syntax::attribute::structured::{
    AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, Terminal, TypedStablePtr};
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use smol_str::SmolStr;

use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics};
use crate::SemanticDiagnostic;

/// The kind of a feature for an item.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FeatureKind {
    /// The feature of the item is stable.
    Stable,
    /// The feature of the item is unstable, with the given name.
    Unstable(String),
}
impl FeatureKind {
    pub fn from_ast(
        db: &dyn SyntaxGroup,
        diagnostics: &mut DiagnosticsBuilder<SemanticDiagnostic>,
        attributes: &ast::AttributeList,
    ) -> Self {
        let unstable_attrs = attributes.query_attr(db, UNSTABLE_ATTR);
        if unstable_attrs.is_empty() {
            return Self::Stable;
        };
        let Ok([unstable_attr]): Result<[_; 1], _> = unstable_attrs.try_into() else {
            diagnostics.add(SemanticDiagnostic::new(
                StableLocation::from_ast(attributes),
                SemanticDiagnosticKind::MultipleFeatureAttributes,
            ));
            return Self::Stable;
        };
        let unstable_attr = unstable_attr.structurize(db);
        let Ok([arg]): Result<[_; 1], _> = unstable_attr.args.try_into() else {
            diagnostics.add(SemanticDiagnostic::new(
                StableLocation::new(unstable_attr.args_stable_ptr.untyped()),
                SemanticDiagnosticKind::UnsupportedUnstableAttrArguments,
            ));
            return Self::Stable;
        };
        match arg.variant {
            AttributeArgVariant::Named { value: ast::Expr::String(value), name, .. }
                if name == "feature" =>
            {
                Self::Unstable(value.text(db).into())
            }
            _ => {
                diagnostics.add(SemanticDiagnostic::new(
                    StableLocation::new(arg.arg_stable_ptr.untyped()),
                    SemanticDiagnosticKind::UnsupportedUnstableAttrArguments,
                ));
                Self::Stable
            }
        }
    }
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
                    variant: AttributeArgVariant::Unnamed { value: ast::Expr::String(value), .. },
                    ..
                },
            ] => value.text(db),
            _ => {
                diagnostics.report_by_ptr(
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

pub fn extract_allowed_features(
    db: &dyn DefsGroup,
    element_id: &impl LanguageElementId,
    syntax: &impl QueryAttrs,
    diagnostics: &mut SemanticDiagnostics,
) -> OrderedHashSet<SmolStr> {
    let syntax_db = db.upcast();
    let mut allowed_features = extract_item_allowed_features(syntax_db, syntax, diagnostics);
    let ignored_diagnostics =
        &mut SemanticDiagnostics::new(element_id.module_file_id(db).file_id(db).unwrap());
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
