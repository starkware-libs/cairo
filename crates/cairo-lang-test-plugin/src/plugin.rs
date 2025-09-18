use cairo_lang_defs::plugin::{MacroPlugin, MacroPluginMetadata, PluginResult};
use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_syntax::attribute::structured::AttributeListStructurize;
use cairo_lang_syntax::node::ast;
use salsa::Database;

use super::{AVAILABLE_GAS_ATTR, IGNORE_ATTR, SHOULD_PANIC_ATTR, TEST_ATTR};
use crate::test_config::try_extract_test_config;

/// Plugin to create diagnostics for tests attributes.
#[derive(Debug, Default)]
#[non_exhaustive]
pub struct TestPlugin;

impl MacroPlugin for TestPlugin {
    fn generate_code<'db>(
        &self,
        db: &'db dyn Database,
        item_ast: ast::ModuleItem<'db>,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult<'db> {
        PluginResult {
            code: None,
            diagnostics: if let ast::ModuleItem::FreeFunction(free_func_ast) = item_ast {
                try_extract_test_config(db, &free_func_ast.attributes(db).structurize(db)).err()
            } else {
                None
            }
            .unwrap_or_default(),
            remove_original_item: false,
        }
    }

    fn declared_attributes<'db>(&self, db: &'db dyn Database) -> Vec<SmolStrId<'db>> {
        vec![
            SmolStrId::from(db, TEST_ATTR),
            SmolStrId::from(db, AVAILABLE_GAS_ATTR),
            SmolStrId::from(db, SHOULD_PANIC_ATTR),
            SmolStrId::from(db, IGNORE_ATTR),
        ]
    }
}
