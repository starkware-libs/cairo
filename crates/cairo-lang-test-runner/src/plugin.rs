use std::sync::Arc;

use cairo_lang_defs::plugin::{MacroPlugin, PluginResult};
use cairo_lang_semantic::plugin::{AsDynMacroPlugin, SemanticPlugin};
use cairo_lang_syntax::attribute::structured::AttributeListStructurize;
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::db::SyntaxGroup;

use crate::test_config::try_extract_test_config;

/// Plugin to create diagnostics for tests attributes.
#[derive(Debug, Default)]
#[non_exhaustive]
pub struct TestPlugin;

impl MacroPlugin for TestPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        PluginResult {
            code: None,
            diagnostics: if let ast::Item::FreeFunction(free_func_ast) = item_ast {
                try_extract_test_config(db, free_func_ast.attributes(db).structurize(db)).err()
            } else {
                None
            }
            .unwrap_or_default(),
            remove_original_item: false,
        }
    }
}
impl AsDynMacroPlugin for TestPlugin {
    fn as_dyn_macro_plugin<'a>(self: Arc<Self>) -> Arc<dyn MacroPlugin + 'a>
    where
        Self: 'a,
    {
        self
    }
}
impl SemanticPlugin for TestPlugin {}
