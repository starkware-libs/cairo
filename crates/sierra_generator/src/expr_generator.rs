#[cfg(test)]
#[path = "expr_generator_test.rs"]
mod tests;

use defs::ids::VarId;
use semantic;

use crate::expr_generator_context::{ExprGeneratorContext, SierraVariable};

/// Generates Sierra code that computes a given expression.
/// Returns a list of Sierra instructions and the Sierra variable in which the result
/// is stored.
pub fn generate_expression_code(
    context: &mut ExprGeneratorContext<'_>,
    expr_id: semantic::ExprId,
) -> (Vec<String>, SierraVariable) {
    generate_expression_code_by_val(context, &context.get_db().lookup_expr(expr_id))
}

fn generate_expression_code_by_val(
    context: &mut ExprGeneratorContext<'_>,
    expr: &semantic::Expr,
) -> (Vec<String>, SierraVariable) {
    match expr {
        semantic::Expr::ExprBlock(expr_block) => handle_block(context, expr_block),
        semantic::Expr::ExprFunctionCall(expr_function_call) => {
            handle_function_call(context, expr_function_call)
        }
        semantic::Expr::ExprMatch(expr_match) => handle_felt_match(context, expr_match),
        semantic::Expr::ExprVar(expr_var) => match expr_var.var {
            VarId::Param(_) => todo!(),
            VarId::Local(local_var) => (vec![], context.get_variable(local_var)),
        },
        semantic::Expr::ExprLiteral(expr_literal) => {
            let tmp_var = context.allocate_sierra_variable();
            (vec![format!("literal<{}>() -> ({});", expr_literal.value, tmp_var)], tmp_var)
        }
    }
}

/// Generates Sierra code for [semantic::ExprBlock].
fn handle_block(
    _context: &mut ExprGeneratorContext<'_>,
    _expr_block: &semantic::ExprBlock,
) -> (Vec<String>, SierraVariable) {
    todo!();
}

/// Generates Sierra code for [semantic::ExprFunctionCall].
fn handle_function_call(
    _context: &mut ExprGeneratorContext<'_>,
    _expr_function_call: &semantic::ExprFunctionCall,
) -> (Vec<String>, SierraVariable) {
    todo!();
}

/// Generates Sierra code for [semantic::ExprMatch].
/// Currently only a simple match-zero is supported.
fn handle_felt_match(
    _context: &mut ExprGeneratorContext<'_>,
    _expr_match: &semantic::ExprMatch,
) -> (Vec<String>, SierraVariable) {
    todo!();
}
