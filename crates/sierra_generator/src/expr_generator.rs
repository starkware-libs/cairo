#[cfg(test)]
#[path = "expr_generator_test.rs"]
mod test;

use defs::ids::GenericFunctionId;
use sierra::program;

use crate::diagnostic::SierraGeneratorDiagnosticKind;
use crate::expr_generator_context::ExprGeneratorContext;
use crate::pre_sierra;
use crate::utils::{jump_statement, simple_statement};

/// Generates Sierra code that computes a given expression.
/// Returns a list of Sierra statements and the Sierra variable in which the result
/// is stored.
pub fn generate_expression_code(
    context: &mut ExprGeneratorContext<'_>,
    expr_id: semantic::ExprId,
) -> Option<(Vec<pre_sierra::Statement>, sierra::ids::VarId)> {
    match &context.get_db().expr_semantic(expr_id) {
        semantic::Expr::ExprBlock(expr_block) => handle_block(context, expr_block),
        semantic::Expr::ExprFunctionCall(expr_function_call) => {
            handle_function_call(context, expr_function_call)
        }
        semantic::Expr::ExprMatch(expr_match) => handle_felt_match(context, expr_match),
        semantic::Expr::ExprVar(expr_var) => {
            Some((vec![], context.get_variable(expr_var.var, expr_var.stable_ptr)?))
        }
        semantic::Expr::ExprLiteral(expr_literal) => {
            let tmp_var = context.allocate_sierra_variable();
            Some((
                vec![simple_statement(
                    context.felt_const_libfunc_id(expr_literal.value),
                    &[],
                    &[tmp_var.clone()],
                )],
                tmp_var,
            ))
        }
        semantic::Expr::Missing { .. } => {
            // A diagnostic should have already been added by a previous stage.
            None
        }
        semantic::Expr::ExprMemberAccess(_) => todo!(),
    }
}

/// Generates Sierra code for [semantic::ExprBlock].
fn handle_block(
    context: &mut ExprGeneratorContext<'_>,
    expr_block: &semantic::ExprBlock,
) -> Option<(Vec<pre_sierra::Statement>, sierra::ids::VarId)> {
    // Process the statements.
    let mut statements: Vec<pre_sierra::Statement> = vec![];
    for statement_id in expr_block.statements.iter().copied() {
        match context.get_db().lookup_intern_statement(statement_id) {
            semantic::Statement::Expr(expr) => {
                let (cur_statements, _res) = generate_expression_code(context, expr)?;
                statements.extend(cur_statements);
            }
            semantic::Statement::Let(statement_let) => {
                let (cur_statements, res) = generate_expression_code(context, statement_let.expr)?;
                statements.extend(cur_statements);
                context.register_variable(defs::ids::VarId::Local(statement_let.var.id), res);
            }
            semantic::Statement::Return(_) => todo!(),
        }
    }

    // Process the tail expression.
    match expr_block.tail {
        Some(expr_id) => {
            let (tail_statements, output_var) = generate_expression_code(context, expr_id)?;
            statements.extend(tail_statements);
            Some((statements, output_var))
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
) -> Option<(Vec<pre_sierra::Statement>, sierra::ids::VarId)> {
    let mut statements: Vec<pre_sierra::Statement> = vec![];

    // The args to push on the stack.
    let mut args: Vec<(sierra::ids::VarId, semantic::TypeId)> = vec![];
    // Output statements to compute the arguments.
    for arg in &expr_function_call.args {
        let (arg_statements, res) = generate_expression_code(context, *arg)?;
        statements.extend(arg_statements);
        args.push((res, context.get_db().lookup_intern_expr(*arg).ty()));
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
            for (arg_var, arg_type) in args {
                let arg_on_stack = context.allocate_sierra_variable();
                statements.push(simple_statement(
                    context.store_temp_libfunc_id(arg_type),
                    &[arg_var],
                    &[arg_on_stack.clone()],
                ));
                args_on_stack.push(arg_on_stack);
            }

            // Call the function.
            let res_var = context.allocate_sierra_variable();
            statements.push(simple_statement(
                context.function_call_libfunc_id(expr_function_call.function),
                &args_on_stack,
                &[res_var.clone()],
            ));
            Some((statements, res_var))
        }
        GenericFunctionId::Extern(extern_id) => {
            if !function_long_id.generic_args.is_empty() {
                context.add_diagnostic(
                    SierraGeneratorDiagnosticKind::CallLibFuncWithGenericArgs,
                    expr_function_call.stable_ptr,
                );
                return None;
            }

            // Call the libfunc.
            let res_var = context.allocate_sierra_variable();
            statements.push(simple_statement(
                context.generic_libfunc_id(extern_id),
                &args.into_iter().map(|(var, _ty)| var).collect::<Vec<_>>()[..],
                &[res_var.clone()],
            ));

            // TODO(lior): Remove the following store_temp once we have a better mechanism of
            //   automatically adding such statements.
            let res_var_on_stack = context.allocate_sierra_variable();
            statements.push(simple_statement(
                context.store_temp_libfunc_id(function_long_id.return_type),
                &[res_var],
                &[res_var_on_stack.clone()],
            ));
            Some((statements, res_var_on_stack))
        }
    }
}

/// Generates Sierra code for [semantic::ExprMatch].
/// Currently only a simple match-zero is supported.
fn handle_felt_match(
    context: &mut ExprGeneratorContext<'_>,
    expr_match: &semantic::ExprMatch,
) -> Option<(Vec<pre_sierra::Statement>, sierra::ids::VarId)> {
    match &expr_match.arms[..] {
        [
            semantic::MatchArm { pattern: semantic::Pattern::Literal(literal), expression: block0 },
            semantic::MatchArm {
                pattern: semantic::Pattern::Otherwise,
                expression: block_otherwise,
            },
        ] => {
            // Make sure the literal in the pattern is 0.
            if literal.value != 0 {
                context.add_diagnostic(
                    SierraGeneratorDiagnosticKind::NonZeroValueInMatch,
                    literal.stable_ptr,
                );
                return None;
            }

            // Generate two labels: for the second code block (otherwise) and for the end of the
            // match.
            let (otherwise_label, otherwise_label_id) = context.new_label();
            let (end_label, end_label_id) = context.new_label();

            let mut statements: Vec<pre_sierra::Statement> = vec![];

            // Generate statements for the matched expression.
            let (match_expr_statements, match_expr_res) =
                generate_expression_code(context, expr_match.matched_expr)?;
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
                            results: vec![non_zero_var.clone()],
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
            let (block0_statements, block0_res) = generate_expression_code(context, *block0)?;
            statements.extend(block0_statements);
            statements.push(simple_statement(
                context.store_temp_libfunc_id(context.get_db().lookup_intern_expr(*block0).ty()),
                &[block0_res],
                &[output_var.clone()],
            ));
            statements.push(jump_statement(context.jump_libfunc_id(), end_label_id));

            // Generate the second block (otherwise).
            let (block_otherwise_statements, block_otherwise_res) =
                generate_expression_code(context, *block_otherwise)?;
            statements.push(otherwise_label);
            // TODO(orizi): Remove this unwrap once we know how to auto cast NonZero vars back to
            // the unwrapped type.
            let unwraped_non_zero_var = context.allocate_sierra_variable();
            statements.push(simple_statement(
                context.unwrap_nz_libfunc_id(context.get_db().core_felt_ty()),
                &[non_zero_var],
                &[unwraped_non_zero_var],
            ));

            statements.extend(block_otherwise_statements);
            statements.push(simple_statement(
                context.store_temp_libfunc_id(
                    context.get_db().lookup_intern_expr(*block_otherwise).ty(),
                ),
                &[block_otherwise_res],
                &[output_var.clone()],
            ));

            // Post match.
            statements.push(end_label);

            Some((statements, output_var))
        }
        _ => {
            context.add_diagnostic(
                SierraGeneratorDiagnosticKind::OnlyMatchZeroIsSupported,
                expr_match.stable_ptr,
            );
            None
        }
    }
}
