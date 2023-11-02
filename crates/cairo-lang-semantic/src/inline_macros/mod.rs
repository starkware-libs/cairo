mod array;
mod consteval_int;
mod format;
mod print;
mod write;

use cairo_lang_defs::plugin::{InlinePluginResult, PluginDiagnostic, PluginSuite};
use cairo_lang_plugins::get_base_plugin_suite;
use cairo_lang_syntax::node::ast::{self};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::WrappedArgListHelper;
use cairo_lang_syntax::node::TypedSyntaxNode;

use self::format::FormatMacro;
use self::print::{PrintMacro, PrintlnMacro};
use self::write::{WriteMacro, WritelnMacro};
use super::inline_macros::array::ArrayMacro;
use super::inline_macros::consteval_int::ConstevalIntMacro;

/// Gets the default plugin suite to load into the Cairo compiler.
pub fn get_default_plugin_suite() -> PluginSuite {
    let mut suite = get_base_plugin_suite();
    suite
        .add_inline_macro_plugin::<ArrayMacro>()
        .add_inline_macro_plugin::<ConstevalIntMacro>()
        .add_inline_macro_plugin::<FormatMacro>()
        .add_inline_macro_plugin::<PrintMacro>()
        .add_inline_macro_plugin::<PrintlnMacro>()
        .add_inline_macro_plugin::<WriteMacro>()
        .add_inline_macro_plugin::<WritelnMacro>();
    suite
}

pub fn unsupported_bracket_diagnostic(
    db: &dyn SyntaxGroup,
    macro_ast: &ast::ExprInlineMacro,
) -> InlinePluginResult {
    InlinePluginResult {
        code: None,
        diagnostics: vec![PluginDiagnostic {
            stable_ptr: macro_ast.arguments(db).left_bracket_stable_ptr(db),
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

/// Extracts `n` unnamed arguments.
pub fn extract_unnamed_args(
    db: &dyn SyntaxGroup,
    macro_arguments: &ast::ArgList,
    n: usize,
) -> Option<Vec<ast::Expr>> {
    let elements = macro_arguments.elements(db);
    if elements.len() != n {
        return None;
    }
    elements.iter().map(|x| try_extract_unnamed_arg(db, x)).collect()
}

/// Gets the syntax of an argument, and extracts the value if it is unnamed.
pub fn try_extract_unnamed_arg(db: &dyn SyntaxGroup, arg_ast: &ast::Arg) -> Option<ast::Expr> {
    if let ast::ArgClause::Unnamed(arg_clause) = arg_ast.arg_clause(db) {
        Some(arg_clause.value(db))
    } else {
        None
    }
}

/// Macro to extract unnamed arguments of an inline macro.
/// Gets the expected number of unnamed arguments, and the pattern for the allowed bracket types,
/// and returns a fixed size array with the argument expressions.
///
/// Example usage (2 arguments, allowing `()` or `{}` brackets):
/// let [arg1, arg2] = extract_macro_unnamed_args!(
///     db,
///     syntax,
///     2,
///     ast::WrappedArgList::ParenthesizedArgList(_) | ast::WrappedArgList::BracedArgList(_)
/// );
#[macro_export]
macro_rules! extract_macro_unnamed_args {
    ($db:expr, $syntax:expr, $n:expr, $pattern:pat) => {{
        if !matches!($syntax.arguments($db), $pattern) {
            return $crate::inline_macros::unsupported_bracket_diagnostic($db, $syntax);
        }
        // `unwrap` is ok because the above `matches` condition ensures it's not None (unless
        // the pattern contains the `Missing` variant).
        let macro_arg_list = cairo_lang_syntax::node::helpers::WrappedArgListHelper::arg_list(
            &$syntax.arguments($db),
            $db,
        )
        .unwrap();

        let args = $crate::inline_macros::extract_unnamed_args($db, &macro_arg_list, $n);
        let Some(args) = args else {
            let diagnostics = vec![PluginDiagnostic {
                stable_ptr: $syntax.stable_ptr().untyped(),
                message: format!(
                    "Macro `{}` must have exactly {} unnamed arguments.",
                    $syntax.path($db).as_syntax_node().get_text_without_trivia($db),
                    $n
                ),
            }];
            return InlinePluginResult { code: None, diagnostics };
        };
        let args: [ast::Expr; $n] = args.try_into().unwrap();
        args
    }};
}

/// Macro to extract a single unnamed argument of an inline macro.
/// Gets the pattern for the allowed bracket types, and returns the argument expression.
///
/// Example usage (allowing `()` or `{}` brackets):
/// let arg = extract_macro_single_unnamed_arg!(
///     db,
///     syntax,
///     ast::WrappedArgList::ParenthesizedArgList(_) | ast::WrappedArgList::BracedArgList(_)
/// );
#[macro_export]
macro_rules! extract_macro_single_unnamed_arg {
    ($db:expr, $syntax:expr, $pattern:pat) => {{
        let [x] = $crate::extract_macro_unnamed_args!($db, $syntax, 1, $pattern);
        x
    }};
}
