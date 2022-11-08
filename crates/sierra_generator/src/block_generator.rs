#[cfg(test)]
#[path = "block_generator_test.rs"]
mod test;

use defs::ids::GenericFunctionId;
use itertools::{chain, enumerate, zip_eq};
use sierra::program;

use crate::expr_generator_context::ExprGeneratorContext;
use crate::pre_sierra;
use crate::utils::{get_concrete_libfunc_id, jump_statement, return_statement, simple_statement};

/// Generates Sierra code that computes a given [lowering::Block].
/// Returns a list of Sierra statements.
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

/// Generates Sierra code that computes a given [lowering::Block].
/// Pushes the values "returned" by the block on the top of the stack, and binds them to
/// the given `binds` variables.
///
/// Returns a list of Sierra statements and a boolean indicating whether the block may continue to
/// the next instruction (true) or not (false).
pub fn generate_block_code_and_push_values(
    context: &mut ExprGeneratorContext<'_>,
    block: &lowering::Block,
    binds: &[lowering::VariableId],
) -> Option<(Vec<pre_sierra::Statement>, bool)> {
    let mut statements = generate_block_code(context, block)?;
    match &block.end {
        lowering::BlockEnd::Callsite(inner_outputs) => {
            let mut push_values = Vec::<pre_sierra::PushValue>::new();
            for (output, inner_output) in zip_eq(binds, inner_outputs) {
                let var_ty = context.get_lowered_variable(*inner_output).ty;
                let var_on_stack_ty = context.get_lowered_variable(*output).ty;
                assert_eq!(
                    var_ty, var_on_stack_ty,
                    "Internal compiler error: Inconsistent types in \
                     generate_block_code_and_push_values()."
                );
                push_values.push(pre_sierra::PushValue {
                    var: context.get_sierra_variable(*inner_output),
                    var_on_stack: context.get_sierra_variable(*output),
                    ty: context.get_db().get_concrete_type_id(var_ty)?,
                })
            }
            statements.push(pre_sierra::Statement::PushValues(push_values));
            Some((statements, true))
        }
        lowering::BlockEnd::Return(returned_variables) => {
            assert!(binds.is_empty(), "binds is expected to be empty when BlockEnd is Return.");
            statements.extend(generate_return_code(context, returned_variables)?);
            Some((statements, false))
        }
        lowering::BlockEnd::Unreachable => Some((statements, false)),
    }
}

/// Generates Sierra code for a `return` statement.
/// Pushes the given returned values on the top of the stack, and returns from the function.
///
/// Returns a list of Sierra statements.
pub fn generate_return_code(
    context: &mut ExprGeneratorContext<'_>,
    returned_variables: &Vec<id_arena::Id<lowering::Variable>>,
) -> Option<Vec<pre_sierra::Statement>> {
    let mut statements: Vec<pre_sierra::Statement> = vec![];
    // Copy the result to the top of the stack before returning.
    let mut return_variables_on_stack = vec![];
    let mut push_values = vec![];

    for returned_variable in returned_variables {
        let return_variable_on_stack = context.allocate_sierra_variable();
        return_variables_on_stack.push(return_variable_on_stack.clone());
        let var_ty = context.get_lowered_variable(*returned_variable).ty;
        push_values.push(pre_sierra::PushValue {
            var: context.get_sierra_variable(*returned_variable),
            var_on_stack: return_variable_on_stack,
            ty: context.get_db().get_concrete_type_id(var_ty)?,
        });
    }

    statements.push(pre_sierra::Statement::PushValues(push_values));
    // Add burn_gas to equalize gas costs across all return paths.
    statements.push(simple_statement(context.burn_gas_libfunc_id(), &[], &[]));
    statements.push(return_statement(return_variables_on_stack));

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
        lowering::Statement::MatchExtern(statement_match_extern) => {
            generate_statement_match_extern_code(context, statement_match_extern)
        }
        lowering::Statement::CallBlock(statement_call_block) => {
            generate_statement_call_block_code(context, statement_call_block)
        }
        lowering::Statement::EnumConstruct(statement_enum_construct) => {
            generate_statement_enum_construct(context, statement_enum_construct)
        }
        lowering::Statement::MatchEnum(statement_match_enum) => {
            generate_statement_match_enum(context, statement_match_enum)
        }
        lowering::Statement::StructConstruct
        | lowering::Statement::StructDestructure
        | lowering::Statement::TupleConstruct(_)
        | lowering::Statement::TupleDestructure(_) => {
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
    Some(vec![simple_statement(
        context.felt_const_libfunc_id(statement.value.clone()),
        &[],
        &[output_var],
    )])
}

/// Generates Sierra code for [lowering::StatementCall].
fn generate_statement_call_code(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementCall,
) -> Option<Vec<pre_sierra::Statement>> {
    // Prepare the Sierra input and output variables.
    let inputs = context.get_sierra_variables(&statement.inputs);
    let outputs = context.get_sierra_variables(&statement.outputs);

    // Check if this is a user defined function or a libfunc.
    let (function_long_id, libfunc_id) =
        get_concrete_libfunc_id(context.get_db(), statement.function);

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
                simple_statement(libfunc_id, &args_on_stack, &outputs),
            ])
        }
        GenericFunctionId::Extern(_) => Some(vec![simple_statement(libfunc_id, &inputs, &outputs)]),
    }
}

/// Generates Sierra code for [lowering::StatementMatchExtern].
fn generate_statement_match_extern_code(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementMatchExtern,
) -> Option<Vec<pre_sierra::Statement>> {
    // Prepare the Sierra input and output variables.
    let args = context.get_sierra_variables(&statement.inputs);

    // Generate labels for all the arms, except for the first (which will be Fallthrough).
    let arm_labels: Vec<(pre_sierra::Statement, pre_sierra::LabelId)> =
        (1..statement.arms.len()).map(|_i| context.new_label()).collect();
    // Generate a label for the end of the match.
    let (end_label, end_label_id) = context.new_label();

    // Get the [ConcreteLibFuncId].
    let (_function_long_id, libfunc_id) =
        get_concrete_libfunc_id(context.get_db(), statement.function);

    let mut statements: Vec<pre_sierra::Statement> = vec![];

    // Create the arm branches.
    let arm_targets: Vec<program::GenBranchTarget<pre_sierra::LabelId>> = chain!(
        [program::GenBranchTarget::Fallthrough],
        arm_labels
            .iter()
            .map(|(_statement, label_id)| program::GenBranchTarget::Statement(*label_id)),
    )
    .collect();

    let branches: Vec<_> = zip_eq(&statement.arms, arm_targets)
        .map(|(arm, target)| program::GenBranchInfo {
            target,
            results: context.get_sierra_variables(&context.get_lowered_block(*arm).inputs),
        })
        .collect();

    // Call the match libfunc.
    statements.push(pre_sierra::Statement::Sierra(program::GenStatement::Invocation(
        program::GenInvocation { libfunc_id, args, branches },
    )));

    // Generate the blocks.
    for (i, arm) in enumerate(&statement.arms) {
        // Add a label for each of the arm blocks, except for the first.
        if i > 0 {
            statements.push(arm_labels[i - 1].0.clone());
        }

        // TODO(lior): Try to avoid the following clone().
        let lowered_block = context.get_lowered_block(*arm);
        let (code, is_reachable) =
            generate_block_code_and_push_values(context, lowered_block, &statement.outputs)?;
        statements.extend(code);

        if is_reachable {
            // Add burn_gas to equalize gas costs across the merging paths.
            statements.push(simple_statement(context.burn_gas_libfunc_id(), &[], &[]));

            // Add jump statement to the end of the match. The last block does not require a jump.
            if i < statement.arms.len() - 1 {
                statements.push(jump_statement(context.jump_libfunc_id(), end_label_id));
            }
        }
    }

    // Post match.
    statements.push(end_label);

    Some(statements)
}

/// Generates Sierra code for [lowering::StatementCallBlock].
fn generate_statement_call_block_code(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementCallBlock,
) -> Option<Vec<pre_sierra::Statement>> {
    let lowered_block = context.get_lowered_block(statement.block);
    // TODO(lior): Rename instead of using PushValues.
    Some(generate_block_code_and_push_values(context, lowered_block, &statement.outputs)?.0)
}

/// Generates Sierra code for [lowering::StatementEnumConstruct].
fn generate_statement_enum_construct(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementEnumConstruct,
) -> Option<Vec<pre_sierra::Statement>> {
    let input_sierra_variable = context.get_sierra_variable(statement.input);
    let output_sierra_variable = context.get_sierra_variable(statement.output);
    let concrete_enum_type =
        context.get_db().get_concrete_type_id(context.get_lowered_variable(statement.output).ty)?;
    let libfunc_id = context.enum_init_libfunc_id(concrete_enum_type, statement.variant.idx);
    Some(vec![simple_statement(libfunc_id, &[input_sierra_variable], &[output_sierra_variable])])
}

/// Generates Sierra code for [lowering::StatementMatchEnum].
fn generate_statement_match_enum(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementMatchEnum,
) -> Option<Vec<pre_sierra::Statement>> {
    let matched_enum = context.get_sierra_variable(statement.input);
    let concrete_enum_type =
        context.get_db().get_concrete_type_id(context.get_lowered_variable(statement.input).ty)?;

    // Generate labels for all the arms.
    let arm_labels: Vec<(pre_sierra::Statement, pre_sierra::LabelId)> =
        (0..statement.arms.len()).map(|_i| context.new_label()).collect();
    // Generate a label for the end of the match.
    let (end_label, end_label_id) = context.new_label();

    let mut statements: Vec<pre_sierra::Statement> = vec![];

    // Create the arm branches.
    let arm_targets = arm_labels
        .iter()
        .map(|(_statement, label_id)| program::GenBranchTarget::Statement(*label_id));

    let branches: Vec<_> = zip_eq(&statement.arms, arm_targets)
        .map(|((_variant, arm), target)| program::GenBranchInfo {
            target,
            results: context.get_sierra_variables(&context.get_lowered_block(*arm).inputs),
        })
        .collect();

    let libfunc_id = context.match_enum_libfunc_id(concrete_enum_type);

    // Call the match libfunc.
    statements.push(pre_sierra::Statement::Sierra(program::GenStatement::Invocation(
        program::GenInvocation { libfunc_id, args: vec![matched_enum], branches },
    )));

    // Generate the blocks.
    // TODO(Gil): Consider unifying with the similar logic in generate_statement_match_extern_code.
    for (i, (_variant, arm)) in enumerate(&statement.arms) {
        statements.push(arm_labels[i].0.clone());

        let lowered_block = context.get_lowered_block(*arm);
        let (code, is_reachable) =
            generate_block_code_and_push_values(context, lowered_block, &statement.outputs)?;
        statements.extend(code);

        if is_reachable {
            // Add burn_gas to equalize gas costs across the merging paths.
            statements.push(simple_statement(context.burn_gas_libfunc_id(), &[], &[]));

            // Add jump statement to the end of the match. The last block does not require a jump.
            if i < statement.arms.len() - 1 {
                statements.push(jump_statement(context.jump_libfunc_id(), end_label_id));
            }
        }
    }
    // Post match.
    statements.push(end_label);

    Some(statements)
}
