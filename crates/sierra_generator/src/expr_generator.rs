#[cfg(test)]
#[path = "expr_generator_test.rs"]
mod test;

use defs::ids::GenericFunctionId;
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
    generate_expression_code_by_val(context, &context.get_db().expr_semantic(expr_id))
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
        semantic::Expr::ExprVar(expr_var) => (vec![], context.get_variable(expr_var.var)),
        semantic::Expr::ExprLiteral(expr_literal) => {
            let tmp_var = context.allocate_sierra_variable();
            (
                vec![simple_statement(
                    context.felt_const_libfunc_id(expr_literal.value),
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
    for statement_id in expr_block.statements.iter().copied() {
        match context.get_db().lookup_intern_statement(statement_id) {
            semantic::Statement::Expr(expr) => {
                let (cur_statements, _res) = generate_expression_code(context, expr);
                statements.extend(cur_statements);
            }
            semantic::Statement::Let(statement_let) => {
                let (cur_statements, res) = generate_expression_code(context, statement_let.expr);
                statements.extend(cur_statements);
                context.register_variable(defs::ids::VarId::Local(statement_let.var.id), res);
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

    // Check if this is a user defined function or a libcall.
    let function_long_id =
        match context.get_db().lookup_intern_function(expr_function_call.function) {
            semantic::FunctionLongId::Concrete(concrete) => concrete,
            semantic::FunctionLongId::Missing => todo!(),
        };
    match function_long_id.generic_function {
        GenericFunctionId::Free(_) => {
            // Push the arguments on top of the stack.
            let mut args_on_stack: Vec<sierra::ids::VarId> = vec![];
            for arg_res in args {
                let arg_var = context.allocate_sierra_variable();
                statements.push(simple_statement(
                    // TODO(lior): Use the real type instead of `felt`.
                    context.store_temp_libfunc_id(context.get_db().core_felt_ty()),
                    &[arg_res],
                    &[arg_var.clone()],
                ));
                args_on_stack.push(arg_var);
            }

            // Call the function.
            let res_var = context.allocate_sierra_variable();
            statements.push(simple_statement(
                context.function_call_libfunc_id(expr_function_call.function),
                &args_on_stack,
                &[res_var.clone()],
            ));
            (statements, res_var)
        }
        GenericFunctionId::Extern(extern_id) => {
            assert!(
                function_long_id.generic_args.is_empty(),
                "Calling a libfunc with generic arguments is not supported yet."
            );

            // Call the libfunc.
            let res_var = context.allocate_sierra_variable();
            statements.push(simple_statement(
                context.generic_libfunc_id(extern_id),
                &args,
                &[res_var.clone()],
            ));

            // TODO(lior): Remove the following store_temp once we have a better mechanism of
            //   automatically adding such statements.
            let res_var_on_stack = context.allocate_sierra_variable();
            statements.push(simple_statement(
                // TODO(lior): Use the real type instead of `felt`.
                context.store_temp_libfunc_id(context.get_db().core_felt_ty()),
                &[res_var],
                &[res_var_on_stack.clone()],
            ));
            (statements, res_var_on_stack)
        }
    }
}

/// Generates Sierra code for [semantic::ExprMatch].
/// Currently only a simple match-zero is supported.
fn handle_felt_match(
    context: &mut ExprGeneratorContext<'_>,
    expr_match: &semantic::ExprMatch,
) -> (Vec<pre_sierra::Statement>, sierra::ids::VarId) {
    match &expr_match.arms[..] {
        [
            semantic::MatchArm { pattern: semantic::Pattern::Literal(literal), expression: block0 },
            semantic::MatchArm {
                pattern: semantic::Pattern::Otherwise,
                expression: block_otherwise,
            },
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

            // Add the felt_jump_nz() statement.
            let non_zero_var = context.allocate_sierra_variable();
            statements.push(pre_sierra::Statement::Sierra(program::GenStatement::Invocation(
                program::GenInvocation {
                    libfunc_id: context.felt_jump_nz_libfunc_id(),
                    args: vec![match_expr_res],
                    branches: vec![
                        // If not zero, jump to the "otherwise" block.
                        program::GenBranchInfo {
                            target: program::GenBranchTarget::Statement(otherwise_label_id),
                            results: vec![non_zero_var],
                        },
                        // If zero, continue to the next instruction.
                        program::GenBranchInfo {
                            target: program::GenBranchTarget::Fallthrough,
                            results: vec![],
                        },
                    ],
                },
            )));

            // Allocate a variable for the result of the match.
            let output_var = context.allocate_sierra_variable();

            // Generate the first block (0).
            let (block0_statements, block0_res) = generate_expression_code(context, *block0);
            statements.extend(block0_statements);
            statements.push(simple_statement(
                // TODO(lior): Use the real type instead of `felt`.
                context.store_temp_libfunc_id(context.get_db().core_felt_ty()),
                &[block0_res],
                &[output_var.clone()],
            ));
            statements.push(jump_statement(context.jump_libfunc_id(), end_label_id));

            // Generate the second block (otherwise).
            let (block_otherwise_statements, block_otherwise_res) =
                generate_expression_code(context, *block_otherwise);
            statements.push(otherwise_label);
            statements.extend(block_otherwise_statements);
            statements.push(simple_statement(
                // TODO(lior): Use the real type instead of `felt`.
                context.store_temp_libfunc_id(context.get_db().core_felt_ty()),
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
