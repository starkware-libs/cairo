#[cfg(test)]
#[path = "expr_generator_test.rs"]
mod test;

use semantic;
use sierra::ids::ConcreteLibFuncId;
use sierra::program;

use crate::expr_generator_context::ExprGeneratorContext;
use crate::pre_sierra;
use crate::utils::{jump_statement, simple_statement};

/// Generates Sierra code that computes a given expression.
/// Returns a list of Sierra statements and the Sierra variable in which the result
/// is stored.
pub fn generate_expression_code(
    context: &mut ExprGeneratorContext<'_>,
    expr_id: semantic::ExprId,
) -> (Vec<pre_sierra::Statement>, sierra::ids::VarId) {
    generate_expression_code_by_val(context, &context.get_db().lookup_intern_expr(expr_id))
}

fn generate_expression_code_by_val(
    context: &mut ExprGeneratorContext<'_>,
    expr: &semantic::Expr,
) -> (Vec<pre_sierra::Statement>, sierra::ids::VarId) {
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
) -> (Vec<pre_sierra::Statement>, sierra::ids::VarId) {
    // Process the statements.
    let mut statements: Vec<pre_sierra::Statement> = vec![];
    for statement in &expr_block.statements {
        match statement {
            semantic::Statement::Expr(expr) => {
                let (cur_statements, _res) = generate_expression_code(context, *expr);
                statements.extend(cur_statements);
            }
            semantic::Statement::Let(statement_let) => {
                let (cur_statements, res) = generate_expression_code(context, statement_let.expr);
                statements.extend(cur_statements);
                context.register_variable(statement_let.var, res);
            }
        }
    }

    // Process the tail expression.
    match expr_block.tail {
        Some(expr_id) => {
            let (tail_statements, output_var) = generate_expression_code(context, expr_id);
            statements.extend(tail_statements);
            (statements, output_var)
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
) -> (Vec<pre_sierra::Statement>, sierra::ids::VarId) {
    let mut statements: Vec<pre_sierra::Statement> = vec![];

    // Output statements to compute the arguments.
    let mut args: Vec<sierra::ids::VarId> = vec![];
    for arg in &expr_function_call.args {
        let (arg_statements, res) = generate_expression_code(context, *arg);
        statements.extend(arg_statements);
        args.push(res);
    }

    // Push the arguments on top of the stack.
    let mut args_on_stack: Vec<sierra::ids::VarId> = vec![];
    for arg_res in args {
        let arg_var = context.allocate_sierra_variable();
        statements.push(simple_statement("store_temp", &[arg_res], &[arg_var.clone()]));
        args_on_stack.push(arg_var);
    }

    // Call the function.
    let res_var = context.allocate_sierra_variable();
    statements.push(simple_statement("func", &args_on_stack, &[res_var.clone()]));
    (statements, res_var)
}

/// Generates Sierra code for [semantic::ExprMatch].
/// Currently only a simple match-zero is supported.
fn handle_felt_match(
    context: &mut ExprGeneratorContext<'_>,
    expr_match: &semantic::ExprMatch,
) -> (Vec<pre_sierra::Statement>, sierra::ids::VarId) {
    match &expr_match.arms[..] {
        [
            semantic::MatchBranch { pattern: semantic::Pattern::Literal(literal), block: block0 },
            semantic::MatchBranch { pattern: semantic::Pattern::Otherwise, block: block_otherwise },
        ] => {
            // Make sure the literal in the pattern is 0.
            // TODO(lior): Replace with diagnostics.
            assert_eq!(literal.value, 0);

            // Generate two labels: for the second code block (otherwise) and for the end of the
            // match.
            let (otherwise_label, otherwise_label_id) = context.new_label();
            let (end_label, end_label_id) = context.new_label();

            let mut statements: Vec<pre_sierra::Statement> = vec![];

            // Generate statements for the matched expression.
            let (match_expr_statements, match_expr_res) =
                generate_expression_code(context, expr_match.matched_expr);
            statements.extend(match_expr_statements);

            // Add the jump_nz() statement.
            statements.push(pre_sierra::Statement::SierraStatement(
                program::GenStatement::Invocation(program::GenInvocation {
                    libfunc_id: ConcreteLibFuncId::from_string("jump_nz"),
                    args: vec![match_expr_res],
                    branches: vec![
                        // If not zero, jump to the "otherwise" block.
                        program::GenBranchInfo {
                            target: program::GenBranchTarget::Statement(otherwise_label_id),
                            results: vec![],
                        },
                        // If zero, continue to the next instruction.
                        program::GenBranchInfo {
                            target: program::GenBranchTarget::Fallthrough,
                            results: vec![],
                        },
                    ],
                }),
            ));

            // Allocate a variable for the result of the match.
            let output_var = context.allocate_sierra_variable();

            // Generate the first block (0).
            let (block0_statements, block0_res) = generate_expression_code(context, *block0);
            statements.extend(block0_statements);
            statements.push(simple_statement("store_temp", &[block0_res], &[output_var.clone()]));
            statements.push(jump_statement(end_label_id));

            // Generate the second block (otherwise).
            let (block_otherwise_statements, block_otherwise_res) =
                generate_expression_code(context, *block_otherwise);
            statements.push(otherwise_label);
            statements.extend(block_otherwise_statements);
            statements.push(simple_statement(
                "store_temp",
                &[block_otherwise_res],
                &[output_var.clone()],
            ));

            // Post match.
            statements.push(end_label);

            (statements, output_var)
        }
        _ => {
            // TODO(lior): Replace with diagnostics.
            unimplemented!();
        }
    }
}
