use std::sync::Arc;

use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, MacroPlugin, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_semantic::plugin::{AsDynMacroPlugin, SemanticPlugin, TrivialPluginAuxData};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use num_bigint::BigInt;

#[derive(Debug, Default)]
#[non_exhaustive]
pub struct ConstevalIntMacroPlugin;

impl AsDynMacroPlugin for ConstevalIntMacroPlugin {
    fn as_dyn_macro_plugin<'a>(self: Arc<Self>) -> Arc<dyn MacroPlugin + 'a>
    where
        Self: 'a,
    {
        self
    }
}
impl SemanticPlugin for ConstevalIntMacroPlugin {}

impl MacroPlugin for ConstevalIntMacroPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        match item_ast {
            ast::Item::Constant(constant_ast) => handle_constant(db, &constant_ast),
            _ => PluginResult::default(),
        }
    }
}

/// Rewrite a constant declaration that contains a consteval_int macro
/// into a constant declaration with the computed value,
/// e.g. `const a: felt252 = consteval_int!(2 * 2 * 2);` into `const a: felt252 = 8;`.
fn handle_constant(db: &dyn SyntaxGroup, constant_ast: &ast::ItemConstant) -> PluginResult {
    let constant_value = constant_ast.value(db);
    if let ast::Expr::InlineMacro(inline_macro) = constant_value {
        if inline_macro.path(db).as_syntax_node().get_text(db) != "consteval_int" {
            return PluginResult::default();
        }
        let mut diagnostics = vec![];
        let constant_expression =
            extract_consteval_macro_expression(db, &inline_macro, &mut diagnostics);
        if constant_expression.is_none() {
            return PluginResult { diagnostics, ..Default::default() };
        }
        let new_value = compute_constant_expr(db, &constant_expression.unwrap(), &mut diagnostics);
        if new_value.is_none() {
            return PluginResult { diagnostics, ..Default::default() };
        }
        return PluginResult {
            code: Some(PluginGeneratedFile {
                name: "computed_constants".into(),
                content: format!(
                    "const {}{}= {};",
                    constant_ast.name(db).text(db),
                    constant_ast.type_clause(db).as_syntax_node().get_text(db),
                    new_value.unwrap()
                ),
                aux_data: DynGeneratedFileAuxData(Arc::new(TrivialPluginAuxData {})),
            }),
            diagnostics,
            remove_original_item: true,
        };
    }
    PluginResult::default()
}

/// Extract the actual expression from the consteval_int macro, or fail with diagnostics.
fn extract_consteval_macro_expression(
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
fn compute_constant_expr(
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
