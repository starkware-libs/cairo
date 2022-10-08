#[cfg(test)]
#[path = "block_generator_test.rs"]
mod test;

use defs::ids::GenericFunctionId;
use itertools::zip_eq;

use crate::expr_generator_context::ExprGeneratorContext;
use crate::pre_sierra;
use crate::utils::simple_statement;

/// Generates Sierra code that computes a given [lowering::Block].
/// Returns a list of Sierra statements.
// TODO(lior): Remove dead_code attribute.
#[allow(dead_code)]
pub fn generate_block_code(
    context: &mut ExprGeneratorContext<'_>,
    block: &lowering::Block,
) -> Option<Vec<pre_sierra::Statement>> {
    // Process the statements.
    let mut statements: Vec<pre_sierra::Statement> = vec![];
    for statement in &block.statements {
        statements.extend(generate_statement_code(context, statement)?);
    }
    Some(statements)
}

/// Generates Sierra code for [lowering::Statement].
pub fn generate_statement_code(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::Statement,
) -> Option<Vec<pre_sierra::Statement>> {
    match statement {
        lowering::Statement::Literal(statement_literal) => {
            generate_statement_literal_code(context, statement_literal)
        }
        lowering::Statement::Call(statement_call) => {
            generate_statement_call_code(context, statement_call)
        }
        lowering::Statement::CallBlock(_)
        | lowering::Statement::MatchExtern(_)
        | lowering::Statement::StructConstruct
        | lowering::Statement::StructDestruct
        | lowering::Statement::EnumConstruct
        | lowering::Statement::MatchEnum
        | lowering::Statement::TupleConstruct
        | lowering::Statement::TupleDestruct(_) => {
            // TODO(lior): Replace with a diagnostic.
            todo!()
        }
    }
}

/// Generates Sierra code for [lowering::StatementLiteral].
fn generate_statement_literal_code(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementLiteral,
) -> Option<Vec<pre_sierra::Statement>> {
    let output_var = context.get_sierra_variable(statement.output);
    Some(vec![simple_statement(context.felt_const_libfunc_id(statement.value), &[], &[output_var])])
}

/// Generates Sierra code for [lowering::StatementCall].
fn generate_statement_call_code(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementCall,
) -> Option<Vec<pre_sierra::Statement>> {
    // Prepare the Sierra input variables.
    let inputs: Vec<sierra::ids::VarId> =
        statement.inputs.iter().map(|input| context.get_sierra_variable(*input)).collect();

    // Prepare the Sierra output variables.
    let outputs: Vec<sierra::ids::VarId> =
        statement.outputs.iter().map(|output| context.get_sierra_variable(*output)).collect();

    // Check if this is a user defined function or a libcall.
    let function_long_id = match context.get_db().lookup_intern_function(statement.function) {
        semantic::FunctionLongId::Concrete(concrete) => concrete,
        semantic::FunctionLongId::Missing => todo!(),
    };
    match function_long_id.generic_function {
        GenericFunctionId::Free(_) => {
            // Create [pre_sierra::PushValue] instances for the arguments.
            let mut args_on_stack: Vec<sierra::ids::VarId> = vec![];
            let mut push_values_vec: Vec<pre_sierra::PushValue> = vec![];
            for (var_id, var) in zip_eq(&statement.inputs, inputs) {
                // Allocate a temporary Sierra variable that represents the argument placed on the
                // stack.
                let arg_on_stack = context.allocate_sierra_variable();
                let ty = context.get_lowered_variable(*var_id).ty;
                push_values_vec.push(pre_sierra::PushValue {
                    var,
                    var_on_stack: arg_on_stack.clone(),
                    ty: context.get_db().get_concrete_type_id(ty)?,
                });
                args_on_stack.push(arg_on_stack);
            }

            Some(vec![
                // Push the arguments.
                pre_sierra::Statement::PushValues(push_values_vec),
                // Call the function.
                simple_statement(
                    context.function_call_libfunc_id(statement.function),
                    &args_on_stack,
                    &outputs,
                ),
            ])
        }
        GenericFunctionId::Extern(extern_id) => {
            let mut generic_args = vec![];
            for generic_arg in &function_long_id.generic_args {
                generic_args.push(match generic_arg {
                    semantic::GenericArgumentId::Type(ty) => sierra::program::GenericArg::Type(
                        // TODO(lior): How should the following unwrap() be handled?
                        context.get_db().get_concrete_type_id(*ty).unwrap(),
                    ),
                });
            }

            Some(vec![simple_statement(
                context.generic_libfunc_id(extern_id, generic_args),
                &inputs,
                &outputs,
            )])
        }
    }
}
