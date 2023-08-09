mod array;
mod consteval_int;

use std::sync::Arc;

use cairo_lang_defs::plugin::{InlineMacroExprPlugin, InlinePluginResult, PluginDiagnostic};
use cairo_lang_syntax::node::ast::{self};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::TypedSyntaxNode;

use super::inline_macros::array::ArrayMacro;
use super::inline_macros::consteval_int::ConstevalIntMacro;

/// Gets the list of default plugins to load into the Cairo compiler.
pub fn get_default_inline_macro_plugins() -> Vec<Arc<dyn InlineMacroExprPlugin>> {
    vec![Arc::new(ArrayMacro), Arc::new(ConstevalIntMacro)]
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
