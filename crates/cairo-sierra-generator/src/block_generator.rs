#[cfg(test)]
#[path = "block_generator_test.rs"]
mod test;

use defs::ids::GenericFunctionId;
use diagnostics::Maybe;
use itertools::{chain, enumerate, zip_eq};
use sierra::program;

use crate::expr_generator_context::ExprGeneratorContext;
use crate::pre_sierra;
use crate::utils::{
    branch_align_libfunc_id, const_libfunc_id_by_type, enum_init_libfunc_id,
    get_concrete_libfunc_id, jump_libfunc_id, jump_statement, match_enum_libfunc_id,
    return_statement, simple_statement, struct_construct_libfunc_id, struct_deconstruct_libfunc_id,
};

/// Generates Sierra code that computes a given [lowering::Block].
/// Returns a list of Sierra statements.
pub fn generate_block_code(
    context: &mut ExprGeneratorContext<'_>,
    block: &lowering::Block,
) -> Maybe<Vec<pre_sierra::Statement>> {
    // Process the statements.
    let mut statements: Vec<pre_sierra::Statement> = vec![];
    for statement in &block.statements {
        statements.extend(generate_statement_code(context, statement)?);
    }
    Ok(statements)
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
) -> Maybe<(Vec<pre_sierra::Statement>, bool)> {
    let mut statements = generate_block_code(context, block)?;
    match &block.end {
        lowering::BlockEnd::Callsite(inner_outputs) => {
            let mut push_values = Vec::<pre_sierra::PushValue>::new();
            for (output, inner_output) in zip_eq(binds, inner_outputs) {
                let ty = context.get_variable_sierra_type(*inner_output)?;
                let var_on_stack_ty = context.get_variable_sierra_type(*output)?;
                assert_eq!(
                    ty, var_on_stack_ty,
                    "Internal compiler error: Inconsistent types in \
                     generate_block_code_and_push_values()."
                );
                push_values.push(pre_sierra::PushValue {
                    var: context.get_sierra_variable(*inner_output),
                    var_on_stack: context.get_sierra_variable(*output),
                    ty,
                })
            }
            statements.push(pre_sierra::Statement::PushValues(push_values));
            Ok((statements, true))
        }
        lowering::BlockEnd::Return(returned_variables) => {
            statements.extend(generate_return_code(context, returned_variables)?);
            Ok((statements, false))
        }
        lowering::BlockEnd::Unreachable => Ok((statements, false)),
    }
}

/// Generates Sierra code for a `return` statement.
/// Pushes the given returned values on the top of the stack, and returns from the function.
///
/// Returns a list of Sierra statements.
pub fn generate_return_code(
    context: &mut ExprGeneratorContext<'_>,
    returned_variables: &Vec<id_arena::Id<lowering::Variable>>,
) -> Maybe<Vec<pre_sierra::Statement>> {
    let mut statements: Vec<pre_sierra::Statement> = vec![];
    // Copy the result to the top of the stack before returning.
    let mut return_variables_on_stack = vec![];
    let mut push_values = vec![];

    for returned_variable in returned_variables {
        let return_variable_on_stack = context.allocate_sierra_variable();
        return_variables_on_stack.push(return_variable_on_stack.clone());
        push_values.push(pre_sierra::PushValue {
            var: context.get_sierra_variable(*returned_variable),
            var_on_stack: return_variable_on_stack,
            ty: context.get_variable_sierra_type(*returned_variable)?,
        });
    }

    statements.push(pre_sierra::Statement::PushValues(push_values));
    statements.push(return_statement(return_variables_on_stack));

    Ok(statements)
}

/// Generates Sierra code for [lowering::Statement].
pub fn generate_statement_code(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::Statement,
) -> Maybe<Vec<pre_sierra::Statement>> {
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
        lowering::Statement::StructConstruct(statement) => {
            generate_statement_struct_construct_code(context, statement)
        }
        lowering::Statement::StructDestructure(statement) => {
            generate_statement_struct_destructure_code(context, statement)
        }
    }
}

/// Generates Sierra code for [lowering::StatementLiteral].
fn generate_statement_literal_code(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementLiteral,
) -> Maybe<Vec<pre_sierra::Statement>> {
    let output_var = context.get_sierra_variable(statement.output);
    Ok(vec![simple_statement(
        const_libfunc_id_by_type(context.get_db(), statement.ty, statement.value.clone()),
        &[],
        &[output_var],
    )])
}

/// Generates Sierra code for [lowering::StatementCall].
fn generate_statement_call_code(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementCall,
) -> Maybe<Vec<pre_sierra::Statement>> {
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
                push_values_vec.push(pre_sierra::PushValue {
                    var,
                    var_on_stack: arg_on_stack.clone(),
                    ty: context.get_variable_sierra_type(*var_id)?,
                });
                args_on_stack.push(arg_on_stack);
            }

            Ok(vec![
                // Push the arguments.
                pre_sierra::Statement::PushValues(push_values_vec),
                // Call the function.
                simple_statement(libfunc_id, &args_on_stack, &outputs),
            ])
        }
        GenericFunctionId::Extern(_) => Ok(vec![simple_statement(libfunc_id, &inputs, &outputs)]),
        GenericFunctionId::TraitFunction(_) => {
            panic!("Trait function should be replaced with concrete functions.")
        }
        GenericFunctionId::ImplFunction(_) => todo!(),
    }
}

/// Generates Sierra code for [lowering::StatementMatchExtern].
fn generate_statement_match_extern_code(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementMatchExtern,
) -> Maybe<Vec<pre_sierra::Statement>> {
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
        .map(|((_, block_id), target)| program::GenBranchInfo {
            target,
            results: context.get_sierra_variables(&context.get_lowered_block(*block_id).inputs),
        })
        .collect();

    // Call the match libfunc.
    statements.push(pre_sierra::Statement::Sierra(program::GenStatement::Invocation(
        program::GenInvocation { libfunc_id, args, branches },
    )));

    // Generate the blocks.
    for (i, (_, block_id)) in enumerate(&statement.arms) {
        // Add a label for each of the arm blocks, except for the first.
        if i > 0 {
            statements.push(arm_labels[i - 1].0.clone());
        }
        // Add branch_align to equalize gas costs across the merging paths.
        statements.push(simple_statement(branch_align_libfunc_id(context.get_db()), &[], &[]));

        // TODO(lior): Try to avoid the following clone().
        let lowered_block = context.get_lowered_block(*block_id);
        let (code, is_reachable) =
            generate_block_code_and_push_values(context, lowered_block, &statement.outputs)?;
        statements.extend(code);

        if is_reachable {
            // Add jump statement to the end of the match. The last block does not require a jump.
            if i < statement.arms.len() - 1 {
                statements.push(jump_statement(jump_libfunc_id(context.get_db()), end_label_id));
            }
        }
    }

    // Post match.
    statements.push(end_label);

    Ok(statements)
}

/// Generates Sierra code for [lowering::StatementCallBlock].
fn generate_statement_call_block_code(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementCallBlock,
) -> Maybe<Vec<pre_sierra::Statement>> {
    let lowered_block = context.get_lowered_block(statement.block);
    // TODO(lior): Rename instead of using PushValues.
    Ok(generate_block_code_and_push_values(context, lowered_block, &statement.outputs)?.0)
}

/// Generates Sierra code for [lowering::StatementEnumConstruct].
fn generate_statement_enum_construct(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementEnumConstruct,
) -> Maybe<Vec<pre_sierra::Statement>> {
    Ok(vec![simple_statement(
        enum_init_libfunc_id(
            context.get_db(),
            context.get_variable_sierra_type(statement.output)?,
            statement.variant.idx,
        ),
        &[context.get_sierra_variable(statement.input)],
        &[context.get_sierra_variable(statement.output)],
    )])
}

/// Generates Sierra code for [lowering::StatementStructConstruct].
fn generate_statement_struct_construct_code(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementStructConstruct,
) -> Maybe<Vec<pre_sierra::Statement>> {
    Ok(vec![simple_statement(
        struct_construct_libfunc_id(
            context.get_db(),
            context.get_variable_sierra_type(statement.output)?,
        ),
        &context.get_sierra_variables(&statement.inputs),
        &[context.get_sierra_variable(statement.output)],
    )])
}

/// Generates Sierra code for [lowering::StatementStructDestructure].
fn generate_statement_struct_destructure_code(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementStructDestructure,
) -> Maybe<Vec<pre_sierra::Statement>> {
    Ok(vec![simple_statement(
        struct_deconstruct_libfunc_id(
            context.get_db(),
            context.get_variable_sierra_type(statement.input)?,
        ),
        &[context.get_sierra_variable(statement.input)],
        &context.get_sierra_variables(&statement.outputs),
    )])
}

/// Generates Sierra code for [lowering::StatementMatchEnum].
fn generate_statement_match_enum(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementMatchEnum,
) -> Maybe<Vec<pre_sierra::Statement>> {
    let matched_enum = context.get_sierra_variable(statement.input);
    let concrete_enum_type = context.get_variable_sierra_type(statement.input)?;
    // Generate labels for all the arms.
    let (arm_label_statements, arm_label_ids): (
        Vec<pre_sierra::Statement>,
        Vec<pre_sierra::LabelId>,
    ) = (0..statement.arms.len()).map(|_i| context.new_label()).unzip();
    // Generate a label for the end of the match.
    let (end_label, end_label_id) = context.new_label();

    let mut statements: Vec<pre_sierra::Statement> = vec![];

    let branches: Vec<_> = zip_eq(&statement.arms, arm_label_ids)
        .map(|((_variant, arm), label_id)| program::GenBranchInfo {
            target: program::GenBranchTarget::Statement(label_id),
            results: context.get_sierra_variables(&context.get_lowered_block(*arm).inputs),
        })
        .collect();

    let libfunc_id = match_enum_libfunc_id(context.get_db(), concrete_enum_type);

    // Call the match libfunc.
    statements.push(pre_sierra::Statement::Sierra(program::GenStatement::Invocation(
        program::GenInvocation { libfunc_id, args: vec![matched_enum], branches },
    )));

    // Generate the blocks.
    // TODO(Gil): Consider unifying with the similar logic in generate_statement_match_extern_code.
    for (i, (label_statement, (_variant, arm))) in
        enumerate(zip_eq(arm_label_statements, &statement.arms))
    {
        statements.push(label_statement);
        // Add branch_align to equalize gas costs across the merging paths.
        statements.push(simple_statement(branch_align_libfunc_id(context.get_db()), &[], &[]));

        let lowered_block = context.get_lowered_block(*arm);
        let (code, is_reachable) =
            generate_block_code_and_push_values(context, lowered_block, &statement.outputs)?;
        statements.extend(code);

        if is_reachable {
            // Add jump statement to the end of the match. The last block does not require a jump.
            if i < statement.arms.len() - 1 {
                statements.push(jump_statement(jump_libfunc_id(context.get_db()), end_label_id));
            }
        }
    }
    // Post match.
    statements.push(end_label);

    Ok(statements)
}
