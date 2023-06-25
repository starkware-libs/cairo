use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use num_bigint::BigInt;

use crate::plugins::{InlineMacro, MacroExpanderData};

pub struct ConstevalIntMacro;

impl InlineMacro for ConstevalIntMacro {
    fn append_macro_code(
        &self,
        macro_expander_data: &mut MacroExpanderData,
        db: &dyn SyntaxGroup,
        macro_ast: &ast::ExprInlineMacro,
    ) {
        let constant_expression =
            extract_consteval_macro_expression(db, macro_ast, &mut macro_expander_data.diagnostics);
        if constant_expression.is_none() {
            return;
        }
        if let Some(new_value) = compute_constant_expr(
            db,
            &constant_expression.unwrap(),
            &mut macro_expander_data.diagnostics,
        ) {
            macro_expander_data.result_code.push_str(&new_value.to_string());
            macro_expander_data.code_changed = true;
        }
    }
}

/// Extract the actual expression from the consteval_int macro, or fail with diagnostics.
pub fn extract_consteval_macro_expression(
    db: &dyn SyntaxGroup,
    macro_ast: &ast::ExprInlineMacro,
    diagnostics: &mut Vec<PluginDiagnostic>,
) -> Option<ast::Expr> {
    let args = macro_ast.arguments(db).args(db).elements(db);
    if args.len() != 1 {
        diagnostics.push(PluginDiagnostic {
            stable_ptr: macro_ast.stable_ptr().untyped(),
            message: "consteval_int macro must have a single unnamed argument.".to_string(),
        });
        return None;
    }
    match args[0].arg_clause(db) {
        ast::ArgClause::Unnamed(arg) => Some(arg.value(db)),
        _ => {
            diagnostics.push(PluginDiagnostic {
                stable_ptr: macro_ast.stable_ptr().untyped(),
                message: "consteval_int macro must have a single unnamed argument.".to_string(),
            });
            None
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
