use cairo_lang_defs::plugin::{MacroPlugin, PluginResult};
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::db::SyntaxGroup;

#[derive(Debug, Default)]
#[non_exhaustive]
pub struct FormatterConfigPlugin;

const FMT_SKIP_ATTR: &str = "cairofmt::skip";

impl MacroPlugin for FormatterConfigPlugin {
    fn generate_code(&self, _db: &dyn SyntaxGroup, _item_ast: ast::Item) -> PluginResult {
        // The formatter config plugin only affect the formatter, so it doesn't generate any code.
        PluginResult::default()
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec![FMT_SKIP_ATTR.to_string()]
    }

    fn declared_statement_attributes(&self) -> Vec<String> {
        vec![FMT_SKIP_ATTR.to_string()]
    }
}
