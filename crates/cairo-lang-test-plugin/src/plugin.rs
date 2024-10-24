use cairo_lang_defs::plugin::{MacroPlugin, MacroPluginMetadata, PluginResult};
use cairo_lang_syntax::attribute::structured::AttributeListStructurize;
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::db::SyntaxGroup;

use super::{AVAILABLE_GAS_ATTR, IGNORE_ATTR, SHOULD_PANIC_ATTR, TEST_ATTR};
use crate::test_config::try_extract_test_config;

/// Plugin to create diagnostics for tests attributes.
#[derive(Debug, Default)]
#[non_exhaustive]
pub struct TestPlugin;

impl MacroPlugin for TestPlugin {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: ast::ModuleItem,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult {
        PluginResult {
            code: None,
            diagnostics: if let ast::ModuleItem::FreeFunction(free_func_ast) = item_ast {
                try_extract_test_config(db, free_func_ast.attributes(db).structurize(db)).err()
            } else {
                None
            }
            .unwrap_or_default(),
            remove_original_item: false,
        }
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec![
            TEST_ATTR.to_string(),
            AVAILABLE_GAS_ATTR.to_string(),
            SHOULD_PANIC_ATTR.to_string(),
            IGNORE_ATTR.to_string(),
        ]
    }
}
