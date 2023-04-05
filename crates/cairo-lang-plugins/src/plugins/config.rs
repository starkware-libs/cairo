use std::sync::Arc;

use cairo_lang_defs::plugin::{MacroPlugin, PluginDiagnostic, PluginResult};
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_semantic::items::attribute::{ast_attribute_to_semantic, Attribute, AttributeArg};
use cairo_lang_semantic::plugin::{AsDynMacroPlugin, SemanticPlugin};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, Terminal};
use unescaper::unescape;

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
            let attr = ast_attribute_to_semantic(db, attr);
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
    let mut set = CfgSet::new();
    let mut clean = true;
    for arg in attr.args {
        if let Some(cfg) = parse_predicate_item(db, arg, diagnostics) {
            set.insert(cfg);
        } else {
            clean = false;
        }
    }
    clean.then_some(set)
}

/// Parse single `#[cfg(...)]` attribute argument as a [`Cfg`] item.
fn parse_predicate_item(
    db: &dyn SyntaxGroup,
    arg: AttributeArg,
    diagnostics: &mut Vec<PluginDiagnostic>,
) -> Option<Cfg> {
    let Some(value) = arg.value else {
        diagnostics.push(PluginDiagnostic {
            stable_ptr: arg.arg_stable_ptr.untyped(),
            message: "This attribute does not support field initialization shorthands.".into(),
        });
        return None;
    };

    let value_stable_ptr = arg.value_stable_ptr.expect("Missing value stable ptr.");

    if let Some(key) = arg.name {
        let ast::Expr::ShortString(terminal) = value else {
            diagnostics.push(PluginDiagnostic {
                stable_ptr: value_stable_ptr.untyped(),
                message: "Expected short string.".into(),
            });
            return None;
        };

        // TODO(mkaput): Extract utility function to parse string and numeric literals.
        let text = terminal.text(db);
        let (literal, _) = text[1..]
            .rsplit_once('\'')
            .expect("Code should be syntactically valid at this moment.");
        let Some(unescaped_literal) = unescape(literal).ok() else {
            diagnostics.push(PluginDiagnostic {
                stable_ptr: value_stable_ptr.untyped(),
                message: "Improperly escaped string.".into(),
            });
            return None;
        };

        Some(Cfg::kv(key, unescaped_literal))
    } else {
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
