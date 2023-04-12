use std::sync::Arc;

use cairo_lang_defs::plugin::{MacroPlugin, PluginDiagnostic, PluginResult};
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_semantic::plugin::{AsDynMacroPlugin, SemanticPlugin};
use cairo_lang_syntax::attribute::structured::{
    Attribute, AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, Terminal};

/// Plugin that enables ignoring modules not involved in the current config.
/// Mostly useful for marking test modules to prevent usage of their functionality out of tests,
/// and reduce compilation time when the tests data isn't required.
#[derive(Debug, Default)]
#[non_exhaustive]
pub struct ConfigPlugin;

impl MacroPlugin for ConfigPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        let mut diagnostics = Vec::new();
        let cfg_set = db.cfg_set();
        for attr in item_ast.query_attr(db, "cfg") {
            let attr = attr.structurize(db);
            if let Some(pattern) = parse_predicate(db, attr, &mut diagnostics) {
                if !cfg_set.is_superset(&pattern) {
                    return PluginResult { code: None, diagnostics, remove_original_item: true };
                }
            }
        }
        PluginResult { diagnostics, ..Default::default() }
    }
}
impl AsDynMacroPlugin for ConfigPlugin {
    fn as_dyn_macro_plugin<'a>(self: Arc<Self>) -> Arc<dyn MacroPlugin + 'a>
    where
        Self: 'a,
    {
        self
    }
}
impl SemanticPlugin for ConfigPlugin {}

/// Parse `#[cfg(...)]` attribute arguments as a predicate matching [`Cfg`] items.
fn parse_predicate(
    db: &dyn SyntaxGroup,
    attr: Attribute,
    diagnostics: &mut Vec<PluginDiagnostic>,
) -> Option<CfgSet> {
    attr
        .args
        .into_iter()
        .map(|arg| parse_predicate_item(db, arg, diagnostics))
        // NOTE: Try to parse each item eagerly, so that we will report any possible issues for all
        //   arguments at once. Take into account that Rust's `Iterator::collect::<Option<_>>`
        //   by itself would stop collection on first `None`.
        .collect::<Vec<_>>()
        .into_iter()
        .collect::<Option<Vec<Cfg>>>()
        .map(CfgSet::from_iter)
}

/// Parse single `#[cfg(...)]` attribute argument as a [`Cfg`] item.
fn parse_predicate_item(
    db: &dyn SyntaxGroup,
    arg: AttributeArg,
    diagnostics: &mut Vec<PluginDiagnostic>,
) -> Option<Cfg> {
    match arg.variant {
        AttributeArgVariant::FieldInitShorthand { .. } => {
            diagnostics.push(PluginDiagnostic {
                stable_ptr: arg.arg_stable_ptr.untyped(),
                message: "This attribute does not support field initialization shorthands.".into(),
            });
            None
        }
        AttributeArgVariant::Named { name, value, value_stable_ptr, .. } => {
            let ast::Expr::ShortString(terminal) = value else {
                diagnostics.push(PluginDiagnostic {
                    stable_ptr: value_stable_ptr.untyped(),
                    message: "Expected short string.".into(),
                });
                return None;
            };

            let value = terminal.string_value(db).unwrap_or_default();

            Some(Cfg::kv(name, value))
        }
        AttributeArgVariant::Unnamed { value, value_stable_ptr, .. } => {
            let ast::Expr::Path(path) = value else {
                diagnostics.push(PluginDiagnostic {
                    stable_ptr: value_stable_ptr.untyped(),
                    message: "Expected identifier.".into(),
                });
                return None;
            };
            let [ast::PathSegment::Simple(segment)] = &path.elements(db)[..] else {
                diagnostics.push(PluginDiagnostic {
                    stable_ptr: value_stable_ptr.untyped(),
                    message: "Expected simple path.".into(),
                });
                return None;
            };
            let key = segment.ident(db).text(db);
            Some(Cfg::tag(key))
        }
    }
}
