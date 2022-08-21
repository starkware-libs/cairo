#[cfg(test)]
#[path = "expr_generator_test.rs"]
mod test;

use semantic;

use crate::expr_generator_context::{ExprGeneratorContext, SierraVariable};
use crate::pre_sierra;
use crate::utils::simple_statement;

/// Generates Sierra code that computes a given expression.
/// Returns a list of Sierra instructions and the Sierra variable in which the result
/// is stored.
pub fn generate_expression_code(
    context: &mut ExprGeneratorContext<'_>,
    expr_id: semantic::ExprId,
) -> (Vec<pre_sierra::Statement>, SierraVariable) {
    generate_expression_code_by_val(context, &context.get_db().lookup_expr(expr_id))
}

fn generate_expression_code_by_val(
    context: &mut ExprGeneratorContext<'_>,
    expr: &semantic::Expr,
) -> (Vec<pre_sierra::Statement>, SierraVariable) {
    match expr {
        semantic::Expr::ExprBlock(expr_block) => handle_block(context, expr_block),
        semantic::Expr::ExprFunctionCall(expr_function_call) => {
            handle_function_call(context, expr_function_call)
        }
        semantic::Expr::ExprMatch(expr_match) => handle_felt_match(context, expr_match),
        semantic::Expr::ExprVar(expr_var) => match expr_var.var {
            defs::ids::VarId::Param(_) => todo!(),
            defs::ids::VarId::Local(local_var) => (vec![], context.get_variable(local_var)),
        },
        semantic::Expr::ExprLiteral(expr_literal) => {
            let tmp_var = context.allocate_sierra_variable();
            (
                vec![simple_statement(
                    format!("literal<{}>", expr_literal.value),
                    &[],
                    &[tmp_var.clone()],
                )],
                tmp_var,
            )
        }
    }
}

/// Generates Sierra code for [semantic::ExprBlock].
fn handle_block(
    context: &mut ExprGeneratorContext<'_>,
    expr_block: &semantic::ExprBlock,
) -> (Vec<pre_sierra::Statement>, SierraVariable) {
    // Process the statements.
    let mut instructions: Vec<pre_sierra::Statement> = vec![];
    for statement in &expr_block.statements {
        match statement {
            semantic::Statement::Expr(expr) => {
                let (statement_instructions, _res) = generate_expression_code(context, *expr);
                instructions.extend(statement_instructions);
            }
            semantic::Statement::Let(statement_let) => {
                let (statement_instructions, res) =
                    generate_expression_code(context, statement_let.expr);
                instructions.extend(statement_instructions);
                context.register_variable(statement_let.var, res);
            }
        }
    }

    // Process the tail expression.
    match expr_block.tail {
        Some(expr_id) => {
            let (tail_instructions, output_var) = generate_expression_code(context, expr_id);
            instructions.extend(tail_instructions);
            (instructions, output_var)
        }
        None => {
            // TODO(lior): Support expressions that do not return a value.
            todo!();
        }
    }
}

/// Generates Sierra code for [semantic::ExprFunctionCall].
fn handle_function_call(
    context: &mut ExprGeneratorContext<'_>,
    expr_function_call: &semantic::ExprFunctionCall,
) -> (Vec<pre_sierra::Statement>, SierraVariable) {
    let mut instructions: Vec<pre_sierra::Statement> = vec![];

    // Output instructions to compute the arguments.
    let mut args: Vec<SierraVariable> = vec![];
    for arg in &expr_function_call.args {
        let (arg_instructions, res) = generate_expression_code(context, *arg);
        instructions.extend(arg_instructions);
        args.push(res);
    }

    // Push the arguments on top of the stack.
    let mut args_on_stack: Vec<sierra::ids::VarId> = vec![];
    for arg_res in args {
        let arg_var = context.allocate_sierra_variable();
        instructions.push(simple_statement("store_temp", &[arg_res], &[arg_var.clone()]));
        args_on_stack.push(arg_var);
    }

    // Call the function.
    let res_var = context.allocate_sierra_variable();
    instructions.push(simple_statement("func", &args_on_stack, &[res_var.clone()]));
    (instructions, res_var)
}

/// Generates Sierra code for [semantic::ExprMatch].
/// Currently only a simple match-zero is supported.
fn handle_felt_match(
    _context: &mut ExprGeneratorContext<'_>,
    _expr_match: &semantic::ExprMatch,
) -> (Vec<pre_sierra::Statement>, SierraVariable) {
    todo!();
}
