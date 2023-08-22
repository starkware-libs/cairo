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

/// Extracts a single unnamed argument.
pub fn extract_single_unnamed_arg(
    db: &dyn SyntaxGroup,
    macro_arguments: ast::ArgList,
) -> Option<ast::Expr> {
    if let Ok([arg]) = <[_; 1]>::try_from(macro_arguments.elements(db)) {
        if let ast::ArgClause::Unnamed(arg_clause) = arg.arg_clause(db) {
            return Some(arg_clause.value(db));
        }
    }
    None
}
