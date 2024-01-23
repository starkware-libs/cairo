#[cfg(test)]
#[path = "block_generator_test.rs"]
mod test;

use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::{chain, enumerate, zip_eq};
use lowering::borrow_check::analysis::StatementLocation;
use lowering::{MatchArm, VarUsage};
use sierra::extensions::lib_func::SierraApChange;
use sierra::program;
use {cairo_lang_lowering as lowering, cairo_lang_sierra as sierra};

use crate::block_generator::sierra::ids::ConcreteLibfuncId;
use crate::expr_generator_context::ExprGeneratorContext;
use crate::lifetime::{DropLocation, SierraGenVar, UseLocation};
use crate::pre_sierra::{self, StatementWithLocation};
use crate::replace_ids::{DebugReplacer, SierraIdReplacer};
use crate::utils::{
    branch_align_libfunc_id, const_libfunc_id_by_type, disable_ap_tracking_libfunc_id,
    drop_libfunc_id, dup_libfunc_id, enable_ap_tracking_libfunc_id,
    enum_from_bounded_int_libfunc_id, enum_init_libfunc_id, get_concrete_libfunc_id,
    get_libfunc_signature, jump_libfunc_id, jump_statement, match_enum_libfunc_id,
    rename_libfunc_id, return_statement, simple_statement, snapshot_take_libfunc_id,
    struct_construct_libfunc_id, struct_deconstruct_libfunc_id,
};

/// Generates Sierra code for the body of the given [lowering::FlatBlock].
/// Returns a list of Sierra statements.
pub fn generate_block_body_code(
    context: &mut ExprGeneratorContext<'_>,
    block_id: lowering::BlockId,
    block: &lowering::FlatBlock,
) -> Maybe<Vec<pre_sierra::StatementWithLocation>> {
    let mut statements: Vec<pre_sierra::StatementWithLocation> = vec![];

    if context.should_disable_ap_tracking(&block_id) {
        context.set_ap_tracking(false);
        statements.push(simple_statement(
            disable_ap_tracking_libfunc_id(context.get_db()),
            &[],
            &[],
        ));
    }

    let drops = context.get_drops();

    add_drop_statements(
        context,
        drops,
        &DropLocation::BeginningOfBlock(block_id),
        &mut statements,
    )?;

    // Process the statements.
    for (i, statement) in block.statements.iter().enumerate() {
        let statement_lowering_location = (block_id, i);
        let statement_cairo_location = statement.location().map(|location_id| {
            context.get_db().lookup_intern_location(location_id).stable_location
        });
        let mut sierra_statements =
            generate_statement_code(context, statement, &statement_lowering_location)?;
        sierra_statements.iter_mut().for_each(|sierra_statement| {
            sierra_statement.set_location(statement_cairo_location);
        });
        statements.extend(sierra_statements);
        let drop_location = &DropLocation::PostStatement(statement_lowering_location);
        add_drop_statements(context, drops, drop_location, &mut statements)?;
    }

    add_drop_statements(
        context,
        drops,
        &DropLocation::PostStatement((block_id, block.statements.len())),
        &mut statements,
    )?;

    Ok(statements)
}

/// Adds calls to the `drop` libfunc for the given [DropLocation], according to the `drops`
/// argument (computed by [find_variable_lifetime](crate::lifetime::find_variable_lifetime)).
fn add_drop_statements(
    context: &mut ExprGeneratorContext<'_>,
    drops: &OrderedHashMap<DropLocation, Vec<SierraGenVar>>,
    drop_location: &DropLocation,
    statements: &mut Vec<pre_sierra::StatementWithLocation>,
) -> Maybe<()> {
    let Some(vars) = drops.get(drop_location) else { return Ok(()) };

    for sierra_gen_var in vars {
        let sierra_var = context.get_sierra_variable(*sierra_gen_var);
        let ty = context.get_variable_sierra_type(*sierra_gen_var)?;
        statements.push(simple_statement(
            drop_libfunc_id(context.get_db(), ty),
            &[sierra_var],
            &[],
        ));
    }

    Ok(())
}

/// Generates Sierra for a given [lowering::FlatBlock].
///
/// Returns a list of Sierra statements.
/// Assumes `block_id` exists in `self.lowered.blocks`.
pub fn generate_block_code(
    context: &mut ExprGeneratorContext<'_>,
    block_id: lowering::BlockId,
) -> Maybe<Vec<pre_sierra::StatementWithLocation>> {
    let block = context.get_lowered_block(block_id);
    let statement_location: StatementLocation = (block_id, block.statements.len());

    let mut statements = generate_block_body_code(context, block_id, block)?;

    match &block.end {
        lowering::FlatBlockEnd::Return(returned_variables) => {
            statements.extend(generate_return_code(
                context,
                returned_variables,
                &statement_location,
            )?);
        }
        lowering::FlatBlockEnd::Panic(_) => {
            unreachable!("Panics should have been stripped in a previous phase.")
        }
        lowering::FlatBlockEnd::Goto(target_block_id, remapping) => {
            statements.push(generate_push_values_statement_for_remapping(
                context,
                statement_location,
                remapping,
            )?);

            if *target_block_id == block_id.next_block_id() {
                statements.push(
                    pre_sierra::Statement::Label(pre_sierra::Label {
                        id: *context.block_label(*target_block_id),
                    })
                    .into_statement_without_location(),
                );

                let code = generate_block_code(context, *target_block_id)?;
                statements.extend(code);
            } else {
                statements.push(jump_statement(
                    jump_libfunc_id(context.get_db()),
                    *context.block_label(*target_block_id),
                ));
            }
        }
        lowering::FlatBlockEnd::NotSet => unreachable!(),
        // Process the block end if it's a match.
        lowering::FlatBlockEnd::Match { info } => {
            let statement_location = (block_id, block.statements.len());
            let statement_cairo_location =
                Some(context.get_db().lookup_intern_location(*info.location()).stable_location);
            if context.should_enable_ap_tracking(&block_id) {
                context.set_ap_tracking(true);
                statements.push(simple_statement(
                    enable_ap_tracking_libfunc_id(context.get_db()),
                    &[],
                    &[],
                ));
            }
            statements.extend(match info {
                lowering::MatchInfo::Extern(s) => generate_match_extern_code(
                    context,
                    s,
                    &statement_location,
                    statement_cairo_location,
                )?,
                lowering::MatchInfo::Enum(s) => generate_match_enum_code(
                    context,
                    s,
                    &statement_location,
                    statement_cairo_location,
                )?,
                lowering::MatchInfo::Value(s) => generate_match_value_code(
                    context,
                    s,
                    &statement_location,
                    statement_cairo_location,
                )?,
            });
        }
    }
    Ok(statements)
}

/// Generates a push_values statement that corresponds to `remapping`.
fn generate_push_values_statement_for_remapping(
    context: &mut ExprGeneratorContext<'_>,
    statement_location: (lowering::BlockId, usize),
    remapping: &lowering::VarRemapping,
) -> Maybe<pre_sierra::StatementWithLocation> {
    let mut push_values = Vec::<pre_sierra::PushValue>::new();
    for (idx, (output, inner_output)) in remapping.iter().enumerate() {
        let ty = context.get_variable_sierra_type(*inner_output)?;
        let var_on_stack_ty = context.get_variable_sierra_type(*output)?;

        if ty != var_on_stack_ty {
            let debug_replacer = DebugReplacer { db: context.get_db() };
            panic!(
                "Internal compiler error: Inconsistent types in \
                 generate_push_values_statement_for_remapping(): ty: `{}`, var_on_stack_ty: `{}`",
                debug_replacer.replace_type_id(&ty),
                debug_replacer.replace_type_id(&var_on_stack_ty),
            );
        }
        let dup = !context.is_last_use(&UseLocation { statement_location, idx });
        push_values.push(pre_sierra::PushValue {
            var: context.get_sierra_variable(*inner_output),
            var_on_stack: context.get_sierra_variable(*output),
            ty,
            dup,
        })
    }
    Ok(pre_sierra::Statement::PushValues(push_values).into_statement_without_location())
}

/// Generates Sierra code for a `return` statement.
/// Pushes the given returned values on the top of the stack, and returns from the function.
///
/// Returns a list of Sierra statements.
pub fn generate_return_code(
    context: &mut ExprGeneratorContext<'_>,
    returned_variables: &[lowering::VarUsage],
    statement_location: &StatementLocation,
) -> Maybe<Vec<pre_sierra::StatementWithLocation>> {
    let mut statements: Vec<pre_sierra::StatementWithLocation> = vec![];
    // Copy the result to the top of the stack before returning.
    let mut return_variables_on_stack = vec![];
    let mut push_values = vec![];

    for (idx, returned_variable) in returned_variables.iter().enumerate() {
        let use_location = UseLocation { statement_location: *statement_location, idx };
        let should_dup = should_dup(context, &use_location);
        let var = context.get_sierra_variable(*returned_variable);
        let return_variable_on_stack =
            if should_dup { context.allocate_sierra_variable() } else { var.clone() };
        return_variables_on_stack.push(return_variable_on_stack.clone());
        push_values.push(pre_sierra::PushValue {
            var,
            var_on_stack: return_variable_on_stack,
            ty: context.get_variable_sierra_type(*returned_variable)?,
            dup: should_dup,
        });
    }
    let location = returned_variables
        .last()
        .map(|var| context.get_db().lookup_intern_location(var.location).stable_location);
    let mut push_statement =
        pre_sierra::Statement::PushValues(push_values).into_statement_without_location();
    push_statement.set_location(location);
    statements.push(push_statement);
    let mut return_statement = return_statement(return_variables_on_stack);

    return_statement.set_location(location);
    statements.push(return_statement);

    Ok(statements)
}

/// Generates Sierra code for [lowering::Statement].
pub fn generate_statement_code(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::Statement,
    statement_location: &StatementLocation,
) -> Maybe<Vec<pre_sierra::StatementWithLocation>> {
    match statement {
        lowering::Statement::Literal(statement_literal) => {
            generate_statement_literal_code(context, statement_literal)
        }
        lowering::Statement::Call(statement_call) => {
            generate_statement_call_code(context, statement_call, statement_location)
        }
        lowering::Statement::EnumConstruct(statement_enum_construct) => {
            generate_statement_enum_construct(context, statement_enum_construct, statement_location)
        }
        lowering::Statement::StructConstruct(statement) => {
            generate_statement_struct_construct_code(context, statement, statement_location)
        }
        lowering::Statement::StructDestructure(statement) => {
            generate_statement_struct_destructure_code(context, statement, statement_location)
        }
        lowering::Statement::Snapshot(statement) => {
            generate_statement_snapshot(context, statement, statement_location)
        }
        lowering::Statement::Desnap(statement) => {
            generate_statement_desnap(context, statement, statement_location)
        }
    }
}

/// Generates Sierra code for [lowering::StatementLiteral].
fn generate_statement_literal_code(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementLiteral,
) -> Maybe<Vec<pre_sierra::StatementWithLocation>> {
    let output_var = context.get_sierra_variable(statement.output);
    Ok(vec![simple_statement(
        const_libfunc_id_by_type(
            context.get_db(),
            context.get_var_type(statement.output),
            statement.value.clone(),
        ),
        &[],
        &[output_var],
    )])
}

/// Generates Sierra code for [lowering::StatementCall].
fn generate_statement_call_code(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementCall,
    statement_location: &StatementLocation,
) -> Maybe<Vec<pre_sierra::StatementWithLocation>> {
    // Check if this is a user defined function or a libfunc.
    let (body, libfunc_id) = get_concrete_libfunc_id(context.get_db(), statement.function);
    // Checks if the call invalidates ap tracking.
    let libfunc_signature = get_libfunc_signature(context.get_db(), libfunc_id.clone());
    let [branch_signature] = &libfunc_signature.branch_signatures[..] else {
        panic!(
            "Unexpected branches in '{}'.",
            DebugReplacer { db: context.get_db() }.replace_libfunc_id(&libfunc_id)
        );
    };
    if matches!(branch_signature.ap_change, SierraApChange::Unknown) {
        context.set_ap_tracking(false)
    }

    if body.is_some() {
        // Create [pre_sierra::PushValue] instances for the arguments.
        let mut args_on_stack: Vec<sierra::ids::VarId> = vec![];
        let mut push_values_vec: Vec<pre_sierra::PushValue> = vec![];

        for (idx, var_usage) in statement.inputs.iter().enumerate() {
            let use_location = UseLocation { statement_location: *statement_location, idx };
            let should_dup = should_dup(context, &use_location);
            let var = context.get_sierra_variable(var_usage.var_id);
            let arg_on_stack =
                if should_dup { context.allocate_sierra_variable() } else { var.clone() };
            push_values_vec.push(pre_sierra::PushValue {
                var,
                var_on_stack: arg_on_stack.clone(),
                ty: context.get_variable_sierra_type(var_usage.var_id)?,
                dup: should_dup,
            });
            args_on_stack.push(arg_on_stack);
        }

        Ok(vec![
            // Push the arguments.
            pre_sierra::Statement::PushValues(push_values_vec).into_statement_without_location(),
            // Call the function.
            simple_statement(
                libfunc_id,
                &args_on_stack,
                &context.get_sierra_variables(&statement.outputs),
            ),
        ])
    } else {
        // Dup variables as needed.
        let mut statements: Vec<pre_sierra::StatementWithLocation> = vec![];
        let inputs_after_dup = maybe_add_dup_statements(
            context,
            statement_location,
            &statement.inputs,
            &mut statements,
        )?;

        statements.push(simple_statement(
            libfunc_id,
            &inputs_after_dup,
            &context.get_sierra_variables(&statement.outputs),
        ));
        Ok(statements)
    }
}

/// Returns if the variable at the given location should be duplicated.
fn should_dup(context: &mut ExprGeneratorContext<'_>, use_location: &UseLocation) -> bool {
    !context.is_last_use(use_location)
}

/// Adds calls to the `dup` libfunc for the given [StatementLocation] and the given statement's
/// inputs.
fn maybe_add_dup_statements(
    context: &mut ExprGeneratorContext<'_>,
    statement_location: &StatementLocation,
    lowering_vars: &[VarUsage],
    statements: &mut Vec<pre_sierra::StatementWithLocation>,
) -> Maybe<Vec<sierra::ids::VarId>> {
    lowering_vars
        .iter()
        .enumerate()
        .map(|(idx, lowering_var)| {
            maybe_add_dup_statement(context, statement_location, idx, lowering_var, statements)
        })
        .collect()
}

/// If necessary, adds a call to the `dup` libfunc for the given [StatementLocation] and the given
/// statement's input, and returns the duplicated copy. Otherwise, returns the original variable.
fn maybe_add_dup_statement(
    context: &mut ExprGeneratorContext<'_>,
    statement_location: &StatementLocation,
    idx: usize,
    lowering_var: &VarUsage,
    statements: &mut Vec<pre_sierra::StatementWithLocation>,
) -> Maybe<sierra::ids::VarId> {
    let sierra_var = context.get_sierra_variable(*lowering_var);

    // Check whether the variable should be dupped.
    if context.is_last_use(&UseLocation { statement_location: *statement_location, idx }) {
        // Dup is not required.
        Ok(sierra_var)
    } else {
        let ty = context.get_variable_sierra_type(*lowering_var)?;
        let dup_var = context.allocate_sierra_variable();
        statements.push(simple_statement(
            dup_libfunc_id(context.get_db(), ty),
            &[sierra_var.clone()],
            &[sierra_var, dup_var.clone()],
        ));
        Ok(dup_var)
    }
}

/// Generates Sierra code for [lowering::MatchExternInfo].
fn generate_match_extern_code(
    context: &mut ExprGeneratorContext<'_>,
    match_info: &lowering::MatchExternInfo,
    statement_location: &StatementLocation,
    statement_cairo_location: Option<StableLocation>,
) -> Maybe<Vec<pre_sierra::StatementWithLocation>> {
    let mut statements: Vec<pre_sierra::StatementWithLocation> = vec![];

    // Prepare the Sierra input variables.
    let args =
        maybe_add_dup_statements(context, statement_location, &match_info.inputs, &mut statements)?;
    // Get the [ConcreteLibfuncId].
    let (_function_long_id, libfunc_id) =
        get_concrete_libfunc_id(context.get_db(), match_info.function);

    generate_match_code(
        context,
        libfunc_id,
        args,
        &match_info.arms,
        &mut statements,
        statement_cairo_location,
    )?;
    Ok(statements)
}
/// Generates Sierra code for the match a [lowering::MatchExternInfo] or [lowering::MatchEnumInfo]
/// statement.
fn generate_match_code(
    context: &mut ExprGeneratorContext<'_>,
    libfunc_id: ConcreteLibfuncId,
    args: Vec<sierra::ids::VarId>,
    arms: &[lowering::MatchArm],
    statements: &mut Vec<pre_sierra::StatementWithLocation>,
    statement_cairo_location: Option<StableLocation>,
) -> Maybe<()> {
    // Generate labels for all the arms, except for the first (which will be Fallthrough).
    let arm_labels: Vec<(pre_sierra::StatementWithLocation, pre_sierra::LabelId)> =
        (1..arms.len()).map(|_i| context.new_label()).collect();
    // Generate a label for the end of the match.
    let (end_label, _) = context.new_label();

    // Create the arm branches.
    let arm_targets: Vec<program::GenBranchTarget<pre_sierra::LabelId>> = if arms.is_empty() {
        vec![]
    } else {
        chain!(
            [program::GenBranchTarget::Fallthrough],
            arm_labels
                .iter()
                .map(|(_statement, label_id)| program::GenBranchTarget::Statement(*label_id)),
        )
        .collect()
    };

    let branches: Vec<_> = zip_eq(arms, arm_targets)
        .map(|(arm, target)| program::GenBranchInfo {
            target,
            results: context.get_sierra_variables(&arm.var_ids),
        })
        .collect();

    // Call the match libfunc.
    statements.push(StatementWithLocation {
        statement: pre_sierra::Statement::Sierra(program::GenStatement::Invocation(
            program::GenInvocation { libfunc_id, args, branches },
        )),
        location: statement_cairo_location,
    });

    let ap_tracking_enabled = context.get_ap_tracking();

    // Generate the blocks.
    for (i, MatchArm { arm_selector: _, block_id, var_ids: _ }) in enumerate(arms) {
        // Reset ap_tracking to the state before the match.
        context.set_ap_tracking(ap_tracking_enabled);

        // Add a label for each of the arm blocks, except for the first.
        if i > 0 {
            statements.push(arm_labels[i - 1].0.clone());
        }
        // Add branch_align to equalize gas costs across the merging paths.
        let mut branch_align_statement =
            simple_statement(branch_align_libfunc_id(context.get_db()), &[], &[]);
        branch_align_statement.set_location(statement_cairo_location);
        statements.push(branch_align_statement);

        let code = generate_block_code(context, *block_id)?;
        statements.extend(code);
    }

    // Post match.
    statements.push(end_label);

    Ok(())
}

/// Generates Sierra code for [lowering::StatementEnumConstruct].
fn generate_statement_enum_construct(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementEnumConstruct,
    statement_location: &StatementLocation,
) -> Maybe<Vec<pre_sierra::StatementWithLocation>> {
    let mut statements: Vec<pre_sierra::StatementWithLocation> = vec![];

    let input =
        maybe_add_dup_statement(context, statement_location, 0, &statement.input, &mut statements)?;

    statements.push(simple_statement(
        enum_init_libfunc_id(
            context.get_db(),
            context.get_variable_sierra_type(statement.output)?,
            statement.variant.idx,
        ),
        &[input],
        &[context.get_sierra_variable(statement.output)],
    ));
    Ok(statements)
}

/// Generates Sierra code for [lowering::StatementStructConstruct].
fn generate_statement_struct_construct_code(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementStructConstruct,
    statement_location: &StatementLocation,
) -> Maybe<Vec<pre_sierra::StatementWithLocation>> {
    let mut statements: Vec<pre_sierra::StatementWithLocation> = vec![];

    let inputs =
        maybe_add_dup_statements(context, statement_location, &statement.inputs, &mut statements)?;

    statements.push(simple_statement(
        struct_construct_libfunc_id(
            context.get_db(),
            context.get_variable_sierra_type(statement.output)?,
        ),
        &inputs,
        &[context.get_sierra_variable(statement.output)],
    ));
    Ok(statements)
}

/// Generates Sierra code for [lowering::StatementStructDestructure].
fn generate_statement_struct_destructure_code(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementStructDestructure,
    statement_location: &StatementLocation,
) -> Maybe<Vec<pre_sierra::StatementWithLocation>> {
    let mut statements: Vec<pre_sierra::StatementWithLocation> = vec![];

    let input =
        maybe_add_dup_statement(context, statement_location, 0, &statement.input, &mut statements)?;

    statements.push(simple_statement(
        struct_deconstruct_libfunc_id(
            context.get_db(),
            context.get_variable_sierra_type(statement.input.var_id)?,
        )?,
        &[input],
        &context.get_sierra_variables(&statement.outputs),
    ));
    Ok(statements)
}

/// Generates Sierra code for [lowering::MatchEnumInfo]. where the matched value is a felt252.
/// Note that the input is a bounded_int and we convert it to an enum, using the
/// enum_from_bounded_int libfunc.
fn generate_match_value_code(
    context: &mut ExprGeneratorContext<'_>,
    match_info: &lowering::MatchEnumValue,
    statement_location: &StatementLocation,
    statement_cairo_location: Option<StableLocation>,
) -> Maybe<Vec<pre_sierra::StatementWithLocation>> {
    let mut statements: Vec<pre_sierra::StatementWithLocation> = vec![];

    // Prepare the Sierra input variables.
    let bounded_int = maybe_add_dup_statement(
        context,
        statement_location,
        0,
        &match_info.input,
        &mut statements,
    )?;

    // Get the [ConcreteLibfuncId].
    let concrete_enum_type = context.get_db().get_index_enum_type_id(match_info.num_of_arms)?;
    let enum_var = context.allocate_sierra_variable();
    statements.push(simple_statement(
        enum_from_bounded_int_libfunc_id(context.get_db().upcast(), concrete_enum_type.clone()),
        &[bounded_int],
        &[enum_var.clone()],
    ));

    let libfunc_id = match_enum_libfunc_id(context.get_db(), concrete_enum_type)?;

    let args = vec![enum_var];
    generate_match_code(
        context,
        libfunc_id,
        args,
        &match_info.arms,
        &mut statements,
        statement_cairo_location,
    )?;
    Ok(statements)
}

/// Generates Sierra code for [lowering::MatchEnumInfo].
fn generate_match_enum_code(
    context: &mut ExprGeneratorContext<'_>,
    match_info: &lowering::MatchEnumInfo,
    statement_location: &StatementLocation,
    statement_cairo_location: Option<StableLocation>,
) -> Maybe<Vec<pre_sierra::StatementWithLocation>> {
    let mut statements: Vec<pre_sierra::StatementWithLocation> = vec![];

    // Prepare the Sierra input variables.
    let matched_enum = maybe_add_dup_statement(
        context,
        statement_location,
        0,
        &match_info.input,
        &mut statements,
    )?;
    // Get the [ConcreteLibfuncId].
    let concrete_enum_type = context.get_variable_sierra_type(match_info.input)?;
    let libfunc_id = match_enum_libfunc_id(context.get_db(), concrete_enum_type)?;

    let args = vec![matched_enum];
    generate_match_code(
        context,
        libfunc_id,
        args,
        &match_info.arms,
        &mut statements,
        statement_cairo_location,
    )?;
    Ok(statements)
}

/// Generates Sierra code for [lowering::StatementSnapshot].
fn generate_statement_snapshot(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementSnapshot,
    statement_location: &StatementLocation,
) -> Maybe<Vec<pre_sierra::StatementWithLocation>> {
    let mut statements: Vec<pre_sierra::StatementWithLocation> = vec![];

    // Prepare the Sierra input variables.
    let input =
        maybe_add_dup_statement(context, statement_location, 0, &statement.input, &mut statements)?;

    let ty = context.get_variable_sierra_type(statement.input.var_id)?;
    let func = snapshot_take_libfunc_id(context.get_db(), ty);

    statements.push(simple_statement(
        func,
        &[input],
        &[
            context.get_sierra_variable(statement.output_original),
            context.get_sierra_variable(statement.output_snapshot),
        ],
    ));
    Ok(statements)
}

/// Generates Sierra code for [lowering::StatementDesnap].
fn generate_statement_desnap(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementDesnap,
    statement_location: &StatementLocation,
) -> Maybe<Vec<pre_sierra::StatementWithLocation>> {
    // Dup variables as needed.
    let mut statements: Vec<pre_sierra::StatementWithLocation> = vec![];
    let inputs_after_dup =
        maybe_add_dup_statements(context, statement_location, &[statement.input], &mut statements)?;

    statements.push(simple_statement(
        rename_libfunc_id(
            context.get_db(),
            context.get_variable_sierra_type(statement.input.var_id)?,
        ),
        &inputs_after_dup,
        &[context.get_sierra_variable(statement.output)],
    ));
    Ok(statements)
}
