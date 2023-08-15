mod array;
mod consteval_int;

use std::sync::Arc;

use cairo_lang_defs::plugin::{InlineMacroExprPlugin, InlinePluginResult, PluginDiagnostic};
use cairo_lang_syntax::node::ast::{self};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::inline_macros::array::ArrayMacro;
use super::inline_macros::consteval_int::ConstevalIntMacro;

/// Gets the default plugins to load into the Cairo compiler.
pub fn get_default_inline_macro_plugins() -> OrderedHashMap<String, Arc<dyn InlineMacroExprPlugin>>
{
    let mut res = OrderedHashMap::<String, Arc<dyn InlineMacroExprPlugin>>::default();
    res.insert("array".to_string(), Arc::new(ArrayMacro));
    res.insert("consteval_int".to_string(), Arc::new(ConstevalIntMacro));
    res
}

fn unsupported_bracket_diagnostic(
    db: &dyn SyntaxGroup,
    macro_ast: &ast::ExprInlineMacro,
) -> InlinePluginResult {
    InlinePluginResult {
        code: None,
        diagnostics: vec![PluginDiagnostic {
            stable_ptr: macro_ast.stable_ptr().untyped(),
            message: format!(
                "Macro {} does not support this bracket type",
                macro_ast.path(db).as_syntax_node().get_text(db)
            ),
        }],
    }
}
