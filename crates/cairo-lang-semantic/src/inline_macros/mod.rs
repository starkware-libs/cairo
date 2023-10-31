mod array;
mod consteval_int;
mod format;
mod print;

use std::sync::Arc;

use cairo_lang_defs::plugin::{InlineMacroExprPlugin, InlinePluginResult, PluginDiagnostic};
use cairo_lang_syntax::node::ast::{self};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use self::format::FormatMacro;
use self::print::PrintMacro;
use super::inline_macros::array::ArrayMacro;
use super::inline_macros::consteval_int::ConstevalIntMacro;

/// Gets the default plugins to load into the Cairo compiler.
pub fn get_default_inline_macro_plugins() -> OrderedHashMap<String, Arc<dyn InlineMacroExprPlugin>>
{
    let mut res = OrderedHashMap::<String, Arc<dyn InlineMacroExprPlugin>>::default();
    res.insert("array".to_string(), Arc::new(ArrayMacro));
    res.insert("consteval_int".to_string(), Arc::new(ConstevalIntMacro));
    res.insert("format".to_string(), Arc::new(FormatMacro));
    res.insert("print".to_string(), Arc::new(PrintMacro));
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
                "Macro `{}` does not support this bracket type.",
                macro_ast.path(db).as_syntax_node().get_text_without_trivia(db)
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
        try_extract_unnamed_arg(db, &arg)
    } else {
        None
    }
}

/// Gets the syntax of an argument, and extracts the value if it is unnamed.
pub fn try_extract_unnamed_arg(db: &dyn SyntaxGroup, arg_ast: &ast::Arg) -> Option<ast::Expr> {
    if let ast::ArgClause::Unnamed(arg_clause) = arg_ast.arg_clause(db) {
        Some(arg_clause.value(db))
    } else {
        None
    }
}
