use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, PluginDiagnostic, PluginGeneratedFile,
};
use cairo_lang_filesystem::ids::{DiagnosticMapping, DiagnoticOrigin};
use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use num_bigint::BigInt;

use super::unsupported_bracket_diagnostic;

#[derive(Debug)]
pub struct ConstevalIntMacro;

impl InlineMacroExprPlugin for ConstevalIntMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
    ) -> InlinePluginResult {
        let mut diagnostics = vec![];
        let ast::WrappedArgList::ParenthesizedArgList(args) = syntax.arguments(db) else {
            return unsupported_bracket_diagnostic(db, syntax);
        };
        let constant_expression =
            extract_consteval_macro_expression(db, &args.args(db), &mut diagnostics);
        if constant_expression.is_none() {
            return InlinePluginResult { code: None, diagnostics };
        }
        let code = compute_constant_expr(db, &constant_expression.unwrap(), &mut diagnostics);
        InlinePluginResult {
            code: code.map(|x| {
                let content = x.to_string();
                let span = TextSpan {
                    start: TextOffset::default(),
                    end: TextOffset::default().add_width(TextWidth::from_str(&content)),
                };
                PluginGeneratedFile {
                    name: "consteval_int_inline_macro".into(),
                    content,
                    diagnostics_mappings: vec![DiagnosticMapping {
                        span,
                        origin: DiagnoticOrigin::Span(syntax.as_syntax_node().span(db)),
                    }],
                    aux_data: None,
                }
            }),
            diagnostics,
        }
    }
}

/// Extract the actual expression from the consteval_int macro, or fail with diagnostics.
pub fn extract_consteval_macro_expression(
    db: &dyn SyntaxGroup,
    macro_arguments: &ast::ArgList,
    diagnostics: &mut Vec<PluginDiagnostic>,
) -> Option<ast::Expr> {
    if let Ok([arg]) = <[_; 1]>::try_from(macro_arguments.elements(db)) {
        if let ast::ArgClause::Unnamed(arg_clause) = arg.arg_clause(db) {
            return Some(arg_clause.value(db));
        }
    }
    diagnostics.push(PluginDiagnostic {
        stable_ptr: macro_arguments.stable_ptr().untyped(),
        message: "consteval_int macro must have a single unnamed argument.".to_string(),
    });
    None
}

/// Compute the actual value of an integer expression, or fail with diagnostics.
/// This computation handles arbitrary integers, unlike regular Cairo math.
pub fn compute_constant_expr(
    db: &dyn SyntaxGroup,
    value: &ast::Expr,
    diagnostics: &mut Vec<PluginDiagnostic>,
) -> Option<BigInt> {
    match value {
        ast::Expr::Literal(lit) => lit.numeric_value(db),
        ast::Expr::Binary(bin_expr) => match bin_expr.op(db) {
            ast::BinaryOperator::Plus(_) => Some(
                compute_constant_expr(db, &bin_expr.lhs(db), diagnostics)?
                    + compute_constant_expr(db, &bin_expr.rhs(db), diagnostics)?,
            ),
            ast::BinaryOperator::Mul(_) => Some(
                compute_constant_expr(db, &bin_expr.lhs(db), diagnostics)?
                    * compute_constant_expr(db, &bin_expr.rhs(db), diagnostics)?,
            ),
            ast::BinaryOperator::Minus(_) => Some(
                compute_constant_expr(db, &bin_expr.lhs(db), diagnostics)?
                    - compute_constant_expr(db, &bin_expr.rhs(db), diagnostics)?,
            ),
            ast::BinaryOperator::Div(_) => Some(
                compute_constant_expr(db, &bin_expr.lhs(db), diagnostics)?
                    / compute_constant_expr(db, &bin_expr.rhs(db), diagnostics)?,
            ),
            ast::BinaryOperator::Mod(_) => Some(
                compute_constant_expr(db, &bin_expr.lhs(db), diagnostics)?
                    % compute_constant_expr(db, &bin_expr.rhs(db), diagnostics)?,
            ),
            ast::BinaryOperator::And(_) => Some(
                compute_constant_expr(db, &bin_expr.lhs(db), diagnostics)?
                    & compute_constant_expr(db, &bin_expr.rhs(db), diagnostics)?,
            ),
            ast::BinaryOperator::Or(_) => Some(
                compute_constant_expr(db, &bin_expr.lhs(db), diagnostics)?
                    | compute_constant_expr(db, &bin_expr.rhs(db), diagnostics)?,
            ),
            ast::BinaryOperator::Xor(_) => Some(
                compute_constant_expr(db, &bin_expr.lhs(db), diagnostics)?
                    ^ compute_constant_expr(db, &bin_expr.rhs(db), diagnostics)?,
            ),
            _ => {
                diagnostics.push(PluginDiagnostic {
                    stable_ptr: bin_expr.stable_ptr().untyped(),
                    message: "Unsupported binary operator in consteval_int macro".to_string(),
                });
                None
            }
        },
        ast::Expr::Unary(un_expr) => match un_expr.op(db) {
            ast::UnaryOperator::Minus(_) => {
                Some(-compute_constant_expr(db, &un_expr.expr(db), diagnostics)?)
            }
            _ => {
                diagnostics.push(PluginDiagnostic {
                    stable_ptr: un_expr.stable_ptr().untyped(),
                    message: "Unsupported unary operator in consteval_int macro".to_string(),
                });
                None
            }
        },
        ast::Expr::Parenthesized(paren_expr) => {
            compute_constant_expr(db, &paren_expr.expr(db), diagnostics)
        }
        _ => {
            diagnostics.push(PluginDiagnostic {
                stable_ptr: value.stable_ptr().untyped(),
                message: "Unsupported expression in consteval_int macro".to_string(),
            });
            None
        }
    }
}
