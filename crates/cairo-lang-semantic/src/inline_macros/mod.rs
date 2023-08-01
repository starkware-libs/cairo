pub mod array;
pub mod consteval_int;
pub mod selector;

use cairo_lang_defs::plugin::{InlineMacroPlugin, InlinePluginResult, PluginDiagnostic};
use cairo_lang_syntax::node::ast::{self};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::TypedSyntaxNode;

use super::inline_macros::array::ArrayMacro;
use super::inline_macros::consteval_int::ConstevalIntMacro;
use super::inline_macros::selector::SelectorMacro;

/// Returns the inline macro plugin for the given macro name, or None if no such plugin exists.
pub fn get_inline_macro_plugin(macro_name: &str) -> Option<Box<dyn InlineMacroPlugin>> {
    match macro_name {
        "array" => Some(Box::new(ArrayMacro)),
        "consteval_int" => Some(Box::new(ConstevalIntMacro)),
        "selector" => Some(Box::new(SelectorMacro)),
        _ => None,
    }
}

pub fn unsupported_bracket_diagnostic(
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
