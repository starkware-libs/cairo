use cairo_lang_syntax::node::helpers::WrappedArgListHelper;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode, ast};
use cairo_lang_utils::require;
use itertools::Itertools;
use salsa::Database;

use crate::plugin::{InlinePluginResult, PluginDiagnostic, PluginResult};

/// Trait providing a consistent interface for inline macro calls.
pub trait InlineMacroCall<'db> {
    type PathNode: TypedSyntaxNode<'db>;
    type Result: PluginResultTrait<'db>;
    fn arguments(&self, db: &'db dyn Database) -> ast::WrappedArgList<'db>;
    fn path(&self, db: &'db dyn Database) -> Self::PathNode;
}

impl<'db> InlineMacroCall<'db> for ast::LegacyExprInlineMacro<'db> {
    type PathNode = ast::ExprPath<'db>;
    type Result = InlinePluginResult<'db>;

    fn arguments(&self, db: &'db dyn Database) -> ast::WrappedArgList<'db> {
        self.arguments(db)
    }

    fn path(&self, db: &'db dyn Database) -> ast::ExprPath<'db> {
        self.path(db)
    }
}

impl<'db> InlineMacroCall<'db> for ast::LegacyItemInlineMacro<'db> {
    type PathNode = ast::ExprPath<'db>;
    type Result = PluginResult<'db>;

    fn arguments(&self, db: &'db dyn Database) -> ast::WrappedArgList<'db> {
        self.arguments(db)
    }

    fn path(&self, db: &'db dyn Database) -> ast::ExprPath<'db> {
        self.path(db)
    }
}

/// Trait providing a consistent interface for the result of a macro plugins.
pub trait PluginResultTrait<'db> {
    fn diagnostic_only(diagnostic: PluginDiagnostic<'db>) -> Self;
}

impl<'db> PluginResultTrait<'db> for InlinePluginResult<'db> {
    fn diagnostic_only(diagnostic: PluginDiagnostic<'db>) -> Self {
        InlinePluginResult { code: None, diagnostics: vec![diagnostic] }
    }
}

impl<'db> PluginResultTrait<'db> for PluginResult<'db> {
    fn diagnostic_only(diagnostic: PluginDiagnostic<'db>) -> Self {
        PluginResult { code: None, diagnostics: vec![diagnostic], remove_original_item: true }
    }
}

/// Returns diagnostics for an unsupported bracket type.
pub fn unsupported_bracket_diagnostic<'db, CallAst: InlineMacroCall<'db>>(
    db: &'db dyn Database,
    legacy_macro_ast: &CallAst,
    macro_ast: impl Into<SyntaxStablePtrId<'db>>,
) -> CallAst::Result {
    CallAst::Result::diagnostic_only(PluginDiagnostic::error_with_inner_span(
        db,
        macro_ast,
        legacy_macro_ast.arguments(db).left_bracket_syntax_node(db),
        format!(
            "Macro `{}` does not support this bracket type.",
            legacy_macro_ast.path(db).as_syntax_node().get_text_without_trivia(db).long(db)
        ),
    ))
}

pub fn not_legacy_macro_diagnostic(stable_ptr: SyntaxStablePtrId<'_>) -> PluginDiagnostic<'_> {
    PluginDiagnostic::error(
        stable_ptr,
        "Macro can not be parsed as legacy macro. Expected an argument list wrapped in either \
         parentheses, brackets, or braces."
            .to_string(),
    )
}

/// Extracts a single unnamed argument.
pub fn extract_single_unnamed_arg<'db>(
    db: &'db dyn Database,
    macro_arguments: ast::ArgList<'db>,
) -> Option<ast::Expr<'db>> {
    let mut elements = macro_arguments.elements(db);
    if elements.len() == 1 { try_extract_unnamed_arg(db, &elements.next()?) } else { None }
}

/// Extracts `n` unnamed arguments.
pub fn extract_unnamed_args<'db>(
    db: &'db dyn Database,
    macro_arguments: &ast::ArgList<'db>,
    n: usize,
) -> Option<Vec<ast::Expr<'db>>> {
    let elements = macro_arguments.elements(db);
    require(elements.len() == n)?;
    elements.map(|x| try_extract_unnamed_arg(db, &x)).collect()
}

/// Gets the syntax of an argument, and extracts the value if it is unnamed.
pub fn try_extract_unnamed_arg<'db>(
    db: &'db dyn Database,
    arg_ast: &ast::Arg<'db>,
) -> Option<ast::Expr<'db>> {
    if let ast::ArgClause::Unnamed(arg_clause) = arg_ast.arg_clause(db) {
        Some(arg_clause.value(db))
    } else {
        None
    }
}

/// Escapes a node for use in a format string.
pub fn escape_node(db: &dyn Database, node: SyntaxNode<'_>) -> String {
    node.get_text_without_trivia(db)
        .long(db)
        .replace('{', "{{")
        .replace('}', "}}")
        .escape_unicode()
        .join("")
}

/// Macro to extract unnamed arguments of an inline macro.
///
/// Gets the expected number of unnamed arguments, and the pattern for the allowed bracket types,
/// and returns a fixed size array with the argument expressions.
///
/// Example usage (2 arguments, allowing `()` or `{}` brackets):
/// let [arg1, arg2] = extract_macro_unnamed_args!(
///     db,
///     syntax,
///     2,
///     ast::WrappedArgList::ParenthesizedArgList(_) | ast::WrappedArgList::BracedArgList(_),
///     token_tree_syntax.stable_ptr(db)
/// );
#[macro_export]
macro_rules! extract_macro_unnamed_args {
    ($db:expr, $syntax:expr, $n:expr, $pattern:pat, $diagnostics_ptr:expr) => {{
        let arguments = $crate::plugin_utils::InlineMacroCall::arguments($syntax, $db);
        if !matches!(arguments, $pattern) {
            return $crate::plugin_utils::unsupported_bracket_diagnostic(
                $db,
                $syntax,
                $diagnostics_ptr,
            );
        }
        // `unwrap` is ok because the above `matches` condition ensures it's not None (unless
        // the pattern contains the `Missing` variant).
        let macro_arg_list =
            cairo_lang_syntax::node::helpers::WrappedArgListHelper::arg_list(&arguments, $db)
                .unwrap();

        let args = $crate::plugin_utils::extract_unnamed_args($db, &macro_arg_list, $n);
        let Some(args) = args else {
            return $crate::plugin_utils::PluginResultTrait::diagnostic_only(
                PluginDiagnostic::error(
                    $diagnostics_ptr,
                    format!(
                        "Macro `{}` must have exactly {} unnamed arguments.",
                        $crate::plugin_utils::InlineMacroCall::path($syntax, $db)
                            .as_syntax_node()
                            .get_text_without_trivia($db)
                            .long($db),
                        $n
                    ),
                ),
            );
        };
        let args: [ast::Expr; $n] = args.try_into().unwrap();
        args
    }};
}

/// Macro to extract a single unnamed argument of an inline macro.
///
/// Gets the pattern for the allowed bracket types, and returns the argument expression.
/// The arguments are extracted from a `WrappedArgList` node syntax, as was in legacy inline macros.
/// However, as macros are now parsed as general token trees, the diagnostics pointer is passed to
/// the macro to allow pointing to the original location.
///
/// Example usage (allowing `()` or `{}` brackets):
/// let arg = extract_macro_single_unnamed_arg!(
///     db,
///     arg_list_syntax,
///     ast::WrappedArgList::ParenthesizedArgList(_) | ast::WrappedArgList::BracedArgList(_),
///     token_tree_syntax.stable_ptr(db)
/// );
#[macro_export]
macro_rules! extract_macro_single_unnamed_arg {
    ($db:expr, $syntax:expr, $pattern:pat, $diagnostics_ptr:expr) => {{
        let [x] = $crate::extract_macro_unnamed_args!($db, $syntax, 1, $pattern, $diagnostics_ptr);
        x
    }};
}
