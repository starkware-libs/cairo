use std::ops::Deref;
use std::sync::Arc;

use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, MacroPlugin, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_semantic::plugin::{AsDynMacroPlugin, SemanticPlugin, TrivialPluginAuxData};
use cairo_lang_syntax::attribute::structured::{
    Attribute, AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::element_list::ElementList;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, SyntaxNode, Terminal, TypedSyntaxNode};

/// Plugin that enables ignoring modules not involved in the current config.
/// Mostly useful for marking test modules to prevent usage of their functionality out of tests,
/// and reduce compilation time when the tests data isn't required.
#[derive(Debug, Default)]
#[non_exhaustive]
pub struct ConfigPlugin;

impl MacroPlugin for ConfigPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        let mut data = ConfigMacroHelper {
            cfg_set: db.cfg_set(),
            result_code: String::new(),
            code_changed: false,
            diagnostics: Vec::new(),
        };
        if data.should_drop(db, &item_ast) {
            return PluginResult {
                code: None,
                diagnostics: data.diagnostics,
                remove_original_item: true,
            };
        }
        data.traverse(db, item_ast.as_syntax_node());
        if data.code_changed {
            PluginResult {
                code: Some(PluginGeneratedFile {
                    name: "config".into(),
                    content: data.result_code.clone(),
                    aux_data: DynGeneratedFileAuxData(Arc::new(TrivialPluginAuxData {})),
                }),
                diagnostics: data.diagnostics,
                remove_original_item: true,
            }
        } else {
            PluginResult { code: None, diagnostics: data.diagnostics, remove_original_item: false }
        }
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

/// Helper for the [`ConfigPlugin`] that accumulates the code of the item, while dropping items not
/// matching the configuration.
struct ConfigMacroHelper {
    cfg_set: Arc<CfgSet>,
    result_code: String,
    code_changed: bool,
    diagnostics: Vec<PluginDiagnostic>,
}
impl ConfigMacroHelper {
    /// Traverse the syntax tree, accumulates the code without the dropped parts.
    fn traverse(&mut self, db: &dyn SyntaxGroup, syntax_node: SyntaxNode) {
        if let Some(text) = syntax_node.text(db) {
            self.result_code.push_str(&text);
        }
        match syntax_node.kind(db) {
            SyntaxKind::ItemList => {
                self.expand_list_node(db, ast::ItemList::from_syntax_node(db, syntax_node))
            }
            SyntaxKind::TraitItemList => {
                self.expand_list_node(db, ast::TraitItemList::from_syntax_node(db, syntax_node))
            }
            SyntaxKind::ImplItemList => {
                self.expand_list_node(db, ast::ImplItemList::from_syntax_node(db, syntax_node))
            }
            _ => {
                for child in syntax_node.children(db) {
                    self.traverse(db, child);
                }
            }
        }
    }

    /// Expands the node of an item list, while dropping elements that needs to be dropped.
    fn expand_list_node<
        Item: QueryAttrs + TypedSyntaxNode,
        List: Deref<Target = ElementList<Item, 1>>,
    >(
        &mut self,
        db: &dyn SyntaxGroup,
        list: List,
    ) {
        for item in list.elements(db) {
            if self.should_drop(db, &item) {
                self.code_changed = true;
            } else {
                self.traverse(db, item.as_syntax_node())
            }
        }
    }

    /// Check if the given item should be dropped from the AST.
    fn should_drop<Item: QueryAttrs>(&mut self, db: &dyn SyntaxGroup, item: &Item) -> bool {
        item.query_attr(db, "cfg").into_iter().any(|attr| {
            matches!(
                parse_predicate(db, attr.structurize(db), &mut self.diagnostics),
                Some(pattern) if !self.cfg_set.is_superset(&pattern)
            )
        })
    }
}

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
            Some(Cfg::name(key))
        }
    }
}
