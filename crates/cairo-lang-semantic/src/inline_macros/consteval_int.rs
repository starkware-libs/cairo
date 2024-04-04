use cairo_lang_defs::extract_macro_single_unnamed_arg;
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, NamedPlugin, PluginDiagnostic, PluginGeneratedFile,
};
use cairo_lang_filesystem::ids::{CodeMapping, CodeOrigin};
use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedStablePtr, TypedSyntaxNode};
use num_bigint::BigInt;

#[derive(Debug, Default)]
pub struct ConstevalIntMacro;
impl NamedPlugin for ConstevalIntMacro {
    const NAME: &'static str = "consteval_int";
}
impl InlineMacroExprPlugin for ConstevalIntMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
    ) -> InlinePluginResult {
        let constant_expression = extract_macro_single_unnamed_arg!(
            db,
            syntax,
            ast::WrappedArgList::ParenthesizedArgList(_)
        );

        let mut diagnostics = vec![];
        let code = compute_constant_expr(db, &constant_expression, &mut diagnostics);
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
                    code_mappings: vec![CodeMapping {
                        span,
                        origin: CodeOrigin::Span(syntax.as_syntax_node().span(db)),
                    }],
                    aux_data: None,
                }
            }),
            diagnostics,
        }
    }
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
                diagnostics.push(PluginDiagnostic::error(
                    bin_expr.stable_ptr().untyped(),
                    "Unsupported binary operator in consteval_int macro".to_string(),
                ));
                None
            }
        },
        ast::Expr::Unary(un_expr) => match un_expr.op(db) {
            ast::UnaryOperator::Minus(_) => {
                Some(-compute_constant_expr(db, &un_expr.expr(db), diagnostics)?)
            }
            _ => {
                diagnostics.push(PluginDiagnostic::error(
                    un_expr.stable_ptr().untyped(),
                    "Unsupported unary operator in consteval_int macro".to_string(),
                ));
                None
            }
        },
        ast::Expr::Parenthesized(paren_expr) => {
            compute_constant_expr(db, &paren_expr.expr(db), diagnostics)
        }
        _ => {
            diagnostics.push(PluginDiagnostic::error(
                value.stable_ptr().untyped(),
                "Unsupported expression in consteval_int macro".to_string(),
            ));
            None
        }
    }
}
