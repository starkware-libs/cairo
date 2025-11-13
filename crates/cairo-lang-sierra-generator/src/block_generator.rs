#[cfg(test)]
#[path = "block_generator_test.rs"]
mod test;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_lowering::BlockId;
use cairo_lang_lowering::ids::LocationId;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::{chain, enumerate, zip_eq};
use lowering::borrow_check::analysis::StatementLocation;
use lowering::{MatchArm, VarUsage};
use sierra::extensions::lib_func::SierraApChange;
use sierra::program;
use {cairo_lang_lowering as lowering, cairo_lang_sierra as sierra};

use crate::block_generator::sierra::ids::ConcreteLibfuncId;
use crate::db::SierraGenGroup;
use crate::expr_generator_context::{ExprGenerationResult, ExprGeneratorContext};
use crate::lifetime::{DropLocation, SierraGenVar, UseLocation};
use crate::pre_sierra::{self, StatementWithLocation};
use crate::replace_ids::{DebugReplacer, SierraIdReplacer};
use crate::utils::{
    branch_align_libfunc_id, const_libfunc_id_by_type, disable_ap_tracking_libfunc_id,
    drop_libfunc_id, dup_libfunc_id, enable_ap_tracking_libfunc_id,
    enum_from_bounded_int_libfunc_id, enum_init_libfunc_id, get_concrete_libfunc_id,
    get_libfunc_signature, jump_libfunc_id, jump_statement, match_enum_libfunc_id,
    rename_libfunc_id, return_statement, simple_basic_statement, snapshot_take_libfunc_id,
    struct_construct_libfunc_id, struct_deconstruct_libfunc_id,
};

/// Generates Sierra code for the body of the given [lowering::Block].
/// Returns a list of Sierra statements.
pub fn generate_block_body_code<'db>(
    context: &mut ExprGeneratorContext<'db, '_>,
    block_id: lowering::BlockId,
    block: &lowering::Block<'db>,
) -> Maybe<()> {
    if context.should_disable_ap_tracking(&block_id) {
        context.set_ap_tracking(false);
        context.push_statement(simple_basic_statement(
            disable_ap_tracking_libfunc_id(context.get_db()),
            &[],
            &[],
        ));
    }

    let drops = context.get_drops();

    add_drop_statements(context, drops, &DropLocation::BeginningOfBlock(block_id))?;

    // Process the statements.
    for (i, statement) in block.statements.iter().enumerate() {
        let statement_lowering_location = (block_id, i);
        let statement_cairo_location = statement.location();
        context.maybe_set_cairo_location(statement_cairo_location);
        generate_statement_code(context, statement, &statement_lowering_location)?;
        let drop_location = &DropLocation::PostStatement(statement_lowering_location);
        add_drop_statements(context, drops, drop_location)?;
    }

    add_drop_statements(
        context,
        drops,
        &DropLocation::PostStatement((block_id, block.statements.len())),
    )?;

    Ok(())
}

/// Adds calls to the `drop` libfunc for the given [DropLocation], according to the `drops`
/// argument (computed by [find_variable_lifetime](crate::lifetime::find_variable_lifetime)).
fn add_drop_statements<'db>(
    context: &mut ExprGeneratorContext<'db, '_>,
    drops: &OrderedHashMap<DropLocation, Vec<SierraGenVar>>,
    drop_location: &DropLocation,
) -> Maybe<()> {
    let Some(vars) = drops.get(drop_location) else { return Ok(()) };

    for sierra_gen_var in vars {
        let sierra_var = context.get_sierra_variable(*sierra_gen_var);
        let ty = context.get_variable_sierra_type(*sierra_gen_var)?;
        context.push_statement(simple_basic_statement(
            drop_libfunc_id(context.get_db(), ty),
            &[sierra_var],
            &[],
        ));
    }

    Ok(())
}

/// Elements to be processed for block code generation.
enum BlockGenStackElement<'db> {
    /// Generated code for the given block.
    Block(BlockId),
    /// Output the given Sierra statement.
    Statement(pre_sierra::Statement<'db>),
    /// Configuration for the following blocks.
    Config { starting_cairo_location: Option<LocationId<'db>>, ap_tracking_state: bool },
}

/// Generates Sierra statements for a function from the given [ExprGeneratorContext].
///
/// Returns a vector of Sierra statements.
pub fn generate_function_result<'db>(
    mut context: ExprGeneratorContext<'db, '_>,
) -> Maybe<ExprGenerationResult<'db>> {
    let mut block_gen_stack = vec![BlockGenStackElement::Block(BlockId::root())];
    while let Some(element) = block_gen_stack.pop() {
        match element {
            BlockGenStackElement::Block(block_id) => {
                generate_block_code(&mut context, &mut block_gen_stack, block_id)?
            }
            BlockGenStackElement::Statement(statement) => context.push_statement(statement),
            BlockGenStackElement::Config { starting_cairo_location, ap_tracking_state } => {
                context.curr_cairo_location = starting_cairo_location;
                context.set_ap_tracking(ap_tracking_state);
            }
        }
    }
    Ok(context.result())
}

/// Generates Sierra for a given [lowering::Block].
///
/// Returns a list of Sierra statements.
/// Assumes `block_id` exists in `self.lowered.blocks`.
fn generate_block_code<'db>(
    context: &mut ExprGeneratorContext<'db, '_>,
    block_gen_stack: &mut Vec<BlockGenStackElement<'db>>,
    block_id: BlockId,
) -> Maybe<()> {
    let block = context.get_lowered_block(block_id);
    let statement_location: StatementLocation = (block_id, block.statements.len());

    generate_block_body_code(context, block_id, block)?;

    match &block.end {
        lowering::BlockEnd::Return(returned_variables, _location) => {
            generate_return_code(context, returned_variables, &statement_location)?;
        }
        lowering::BlockEnd::Panic(_) => {
            unreachable!("Panics should have been stripped in a previous phase.")
        }
        lowering::BlockEnd::Goto(target_block_id, remapping) => {
            let StatementWithLocation { statement: push_values_statement, location } =
                generate_push_values_statement_for_remapping(
                    context,
                    statement_location,
                    remapping,
                )?;
            context.maybe_set_cairo_location(location);
            context.push_statement(push_values_statement);

            if *target_block_id == block_id.next_block_id() {
                let label = pre_sierra::Statement::Label(pre_sierra::Label {
                    id: *context.block_label(*target_block_id),
                });
                context.push_statement(label);
                block_gen_stack.push(BlockGenStackElement::Block(*target_block_id));
            } else {
                let jump = jump_statement(
                    jump_libfunc_id(context.get_db()),
                    *context.block_label(*target_block_id),
                );
                context.push_statement(jump);
            }
        }
        lowering::BlockEnd::NotSet => unreachable!(),
        // Process the block end if it's a match.
        lowering::BlockEnd::Match { info } => {
            let statement_location = (block_id, block.statements.len());
            let statement_cairo_location = info.location();
            if context.should_enable_ap_tracking(&block_id) {
                context.set_ap_tracking(true);
                context.push_statement(simple_basic_statement(
                    enable_ap_tracking_libfunc_id(context.get_db()),
                    &[],
                    &[],
                ));
            }

            context.maybe_set_cairo_location(Some(*statement_cairo_location));

            match info {
                lowering::MatchInfo::Extern(s) => {
                    generate_match_extern_code(context, block_gen_stack, s, &statement_location)
                }
                lowering::MatchInfo::Enum(s) => {
                    generate_match_enum_code(context, block_gen_stack, s, &statement_location)
                }
                lowering::MatchInfo::Value(s) => {
                    generate_match_value_code(context, block_gen_stack, s, &statement_location)
                }
            }?;
        }
    }
    Ok(())
}

/// Generates a push_values statement that corresponds to `remapping`.
fn generate_push_values_statement_for_remapping<'db, 'mt>(
    context: &mut ExprGeneratorContext<'db, 'mt>,
    statement_location: (lowering::BlockId, usize),
    remapping: &lowering::VarRemapping<'db>,
) -> Maybe<pre_sierra::StatementWithLocation<'db>> {
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
    let location = remapping.iter().next_back().map(|(_, inner_output)| inner_output.location);
    Ok(StatementWithLocation {
        statement: pre_sierra::Statement::PushValues(push_values),
        location,
    })
}

/// Generates Sierra code for a `return` statement.
/// Pushes the given returned values on the top of the stack, and returns from the function.
///
/// Returns a list of Sierra statements.
pub fn generate_return_code<'db>(
    context: &mut ExprGeneratorContext<'db, '_>,
    returned_variables: &[lowering::VarUsage<'db>],
    statement_location: &StatementLocation,
) -> Maybe<()> {
    // Copy the result to the top of the stack before returning.
    let mut return_variables_on_stack = vec![];
    let mut push_values = vec![];

    for (idx, returned_variable) in returned_variables.iter().enumerate() {
        let use_location = UseLocation { statement_location: *statement_location, idx };
        let should_dup = should_dup(context, &use_location);
        let var = context.get_sierra_variable(*returned_variable);
        let return_variable_on_stack = if should_dup {
            context.allocate_sierra_variable(returned_variable.var_id)
        } else {
            var.clone()
        };
        return_variables_on_stack.push(return_variable_on_stack.clone());
        push_values.push(pre_sierra::PushValue {
            var,
            var_on_stack: return_variable_on_stack,
            ty: context.get_variable_sierra_type(*returned_variable)?,
            dup: should_dup,
        });
    }
    let location = returned_variables.last().map(|var| var.location);
    context.maybe_set_cairo_location(location);
    context.push_statement(pre_sierra::Statement::PushValues(push_values));
    context.push_statement(return_statement(return_variables_on_stack));
    Ok(())
}

/// Generates Sierra code for [lowering::Statement].
pub fn generate_statement_code<'db>(
    context: &mut ExprGeneratorContext<'db, '_>,
    statement: &lowering::Statement<'db>,
    statement_location: &StatementLocation,
) -> Maybe<()> {
    match statement {
        lowering::Statement::Const(statement_literal) => {
            generate_statement_const_code(context, statement_literal)
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

/// Generates Sierra code for [lowering::StatementConst].
fn generate_statement_const_code<'db>(
    context: &mut ExprGeneratorContext<'db, '_>,
    statement: &lowering::StatementConst<'db>,
) -> Maybe<()> {
    let output_var = context.get_sierra_variable(statement.output);
    context.push_statement(simple_basic_statement(
        const_libfunc_id_by_type(context.get_db(), statement.value, statement.boxed),
        &[],
        &[output_var],
    ));
    Ok(())
}

/// Generates Sierra code for [lowering::StatementCall].
fn generate_statement_call_code<'db>(
    context: &mut ExprGeneratorContext<'db, '_>,
    statement: &lowering::StatementCall<'db>,
    statement_location: &StatementLocation,
) -> Maybe<()> {
    // Check if this is a user defined function or a libfunc.
    let (body, libfunc_id) =
        get_concrete_libfunc_id(context.get_db(), statement.function, statement.with_coupon);
    // Checks if the call invalidates ap tracking.
    let libfunc_signature = get_libfunc_signature(context.get_db(), &libfunc_id);
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
            let arg_on_stack = if should_dup {
                context.allocate_sierra_variable(var_usage.var_id)
            } else {
                var.clone()
            };
            push_values_vec.push(pre_sierra::PushValue {
                var,
                var_on_stack: arg_on_stack.clone(),
                ty: context.get_variable_sierra_type(var_usage.var_id)?,
                dup: should_dup,
            });
            args_on_stack.push(arg_on_stack);
        }

        // Push the arguments.
        context.push_statement(pre_sierra::Statement::PushValues(push_values_vec));
        // Call the function.
        let call_stmt = simple_basic_statement(
            libfunc_id,
            &args_on_stack,
            &context.get_sierra_variables(&statement.outputs),
        );
        context.push_statement(call_stmt);
    } else {
        assert!(!statement.with_coupon, "Extern functions cannot have a __coupon__ argument.");
        // Dup variables as needed.
        let inputs_after_dup =
            maybe_add_dup_statements(context, statement_location, &statement.inputs)?;
        let stmt = simple_basic_statement(
            libfunc_id,
            &inputs_after_dup,
            &context.get_sierra_variables(&statement.outputs),
        );
        context.push_statement(stmt);
    }
    Ok(())
}

/// Returns if the variable at the given location should be duplicated.
fn should_dup<'db>(
    context: &mut ExprGeneratorContext<'db, '_>,
    use_location: &UseLocation,
) -> bool {
    !context.is_last_use(use_location)
}

/// Adds calls to the `dup` libfunc for the given [StatementLocation] and the given statement's
/// inputs.
fn maybe_add_dup_statements<'db>(
    context: &mut ExprGeneratorContext<'db, '_>,
    statement_location: &StatementLocation,
    lowering_vars: &[VarUsage<'db>],
) -> Maybe<Vec<sierra::ids::VarId>> {
    lowering_vars
        .iter()
        .enumerate()
        .map(|(idx, lowering_var)| {
            maybe_add_dup_statement(context, statement_location, idx, lowering_var)
        })
        .collect()
}

/// If necessary, adds a call to the `dup` libfunc for the given [StatementLocation] and the given
/// statement's input, and returns the duplicated copy. Otherwise, returns the original variable.
fn maybe_add_dup_statement<'db>(
    context: &mut ExprGeneratorContext<'db, '_>,
    statement_location: &StatementLocation,
    idx: usize,
    lowering_var: &VarUsage<'db>,
) -> Maybe<sierra::ids::VarId> {
    let sierra_var = context.get_sierra_variable(*lowering_var);

    // Check whether the variable should be dupped.
    if context.is_last_use(&UseLocation { statement_location: *statement_location, idx }) {
        // Dup is not required.
        Ok(sierra_var)
    } else {
        let ty = context.get_variable_sierra_type(*lowering_var)?;
        let dup_var = context.allocate_sierra_variable(lowering_var.var_id);
        context.push_statement(simple_basic_statement(
            dup_libfunc_id(context.get_db(), ty),
            std::slice::from_ref(&sierra_var),
            &[sierra_var.clone(), dup_var.clone()],
        ));
        Ok(dup_var)
    }
}

/// Generates Sierra code for [lowering::MatchExternInfo].
fn generate_match_extern_code<'db>(
    context: &mut ExprGeneratorContext<'db, '_>,
    block_gen_stack: &mut Vec<BlockGenStackElement<'db>>,
    match_info: &lowering::MatchExternInfo<'db>,
    statement_location: &StatementLocation,
) -> Maybe<()> {
    // Prepare the Sierra input variables.
    let args = maybe_add_dup_statements(context, statement_location, &match_info.inputs)?;
    // Get the [ConcreteLibfuncId].
    let (_function_long_id, libfunc_id) =
        get_concrete_libfunc_id(context.get_db(), match_info.function, false);

    generate_match_code(context, block_gen_stack, libfunc_id, args, &match_info.arms)
}
/// Generates Sierra code for the match a [lowering::MatchExternInfo] or [lowering::MatchEnumInfo]
/// statement.
fn generate_match_code<'db>(
    context: &mut ExprGeneratorContext<'db, '_>,
    block_gen_stack: &mut Vec<BlockGenStackElement<'db>>,
    libfunc_id: ConcreteLibfuncId,
    args: Vec<sierra::ids::VarId>,
    arms: &[lowering::MatchArm<'db>],
) -> Maybe<()> {
    // Generate labels for all the arms, except for the first (which will be Fallthrough).
    let arm_labels: Vec<(pre_sierra::Statement<'_>, pre_sierra::LabelId<'_>)> =
        (1..arms.len()).map(|_i| context.new_label()).collect();
    // Generate a label for the end of the match.
    let (end_label, _) = context.new_label();

    // Create the arm branches.
    let arm_targets: Vec<program::GenBranchTarget<pre_sierra::LabelId<'_>>> = if arms.is_empty() {
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
    context.push_statement(pre_sierra::Statement::Sierra(program::GenStatement::Invocation(
        program::GenInvocation { libfunc_id, args, branches },
    )));

    let ap_tracking_enabled = context.get_ap_tracking();

    let match_block_location = context.curr_cairo_location.take();
    block_gen_stack.push(BlockGenStackElement::Statement(end_label));
    // Generate the blocks.
    for (i, MatchArm { arm_selector: _, block_id, var_ids: _ }) in enumerate(arms).rev() {
        block_gen_stack.push(BlockGenStackElement::Block(*block_id));
        block_gen_stack.push(BlockGenStackElement::Statement(simple_basic_statement(
            branch_align_libfunc_id(context.get_db()),
            &[],
            &[],
        )));
        if i > 0 {
            block_gen_stack.push(BlockGenStackElement::Statement(arm_labels[i - 1].0.clone()));
        }
        block_gen_stack.push(BlockGenStackElement::Config {
            starting_cairo_location: match_block_location,
            ap_tracking_state: ap_tracking_enabled,
        });
    }
    Ok(())
}

/// Generates Sierra code for [lowering::StatementEnumConstruct].
fn generate_statement_enum_construct<'db>(
    context: &mut ExprGeneratorContext<'db, '_>,
    statement: &lowering::StatementEnumConstruct<'db>,
    statement_location: &StatementLocation,
) -> Maybe<()> {
    let input = maybe_add_dup_statement(context, statement_location, 0, &statement.input)?;
    let stmt = simple_basic_statement(
        enum_init_libfunc_id(
            context.get_db(),
            context.get_variable_sierra_type(statement.output)?,
            statement.variant.idx,
        ),
        &[input],
        &[context.get_sierra_variable(statement.output)],
    );
    context.push_statement(stmt);
    Ok(())
}

/// Generates Sierra code for [lowering::StatementStructConstruct].
fn generate_statement_struct_construct_code<'db>(
    context: &mut ExprGeneratorContext<'db, '_>,
    statement: &lowering::StatementStructConstruct<'db>,
    statement_location: &StatementLocation,
) -> Maybe<()> {
    let inputs = maybe_add_dup_statements(context, statement_location, &statement.inputs)?;
    let stmt = simple_basic_statement(
        struct_construct_libfunc_id(
            context.get_db(),
            context.get_variable_sierra_type(statement.output)?,
        ),
        &inputs,
        &[context.get_sierra_variable(statement.output)],
    );
    context.push_statement(stmt);
    Ok(())
}

/// Generates Sierra code for [lowering::StatementStructDestructure].
fn generate_statement_struct_destructure_code<'db>(
    context: &mut ExprGeneratorContext<'db, '_>,
    statement: &lowering::StatementStructDestructure<'db>,
    statement_location: &StatementLocation,
) -> Maybe<()> {
    let input = maybe_add_dup_statement(context, statement_location, 0, &statement.input)?;
    let stmt = simple_basic_statement(
        struct_deconstruct_libfunc_id(
            context.get_db(),
            context.get_variable_sierra_type(statement.input.var_id)?,
        )?,
        &[input],
        &context.get_sierra_variables(&statement.outputs),
    );
    context.push_statement(stmt);
    Ok(())
}

/// Generates Sierra code for [lowering::MatchEnumInfo]. where the matched value is a felt252.
/// Note that the input is a bounded_int and we convert it to an enum, using the
/// enum_from_bounded_int libfunc.
fn generate_match_value_code<'db>(
    context: &mut ExprGeneratorContext<'db, '_>,
    block_gen_stack: &mut Vec<BlockGenStackElement<'db>>,
    match_info: &lowering::MatchEnumValue<'db>,
    statement_location: &StatementLocation,
) -> Maybe<()> {
    // Prepare the Sierra input variables.
    let bounded_int = maybe_add_dup_statement(context, statement_location, 0, &match_info.input)?;

    // Get the [ConcreteLibfuncId].
    let concrete_enum_type = context.get_db().get_index_enum_type_id(match_info.num_of_arms)?;
    let enum_var = context.allocate_sierra_variable(match_info.input.var_id);
    context.push_statement(simple_basic_statement(
        enum_from_bounded_int_libfunc_id(context.get_db(), concrete_enum_type.clone()),
        &[bounded_int],
        std::slice::from_ref(&enum_var),
    ));

    let libfunc_id = match_enum_libfunc_id(context.get_db(), concrete_enum_type.clone())?;

    let args = vec![enum_var];
    generate_match_code(context, block_gen_stack, libfunc_id, args, &match_info.arms)
}

/// Generates Sierra code for [lowering::MatchEnumInfo].
fn generate_match_enum_code<'db>(
    context: &mut ExprGeneratorContext<'db, '_>,
    block_gen_stack: &mut Vec<BlockGenStackElement<'db>>,
    match_info: &lowering::MatchEnumInfo<'db>,
    statement_location: &StatementLocation,
) -> Maybe<()> {
    // Prepare the Sierra input variables.
    let matched_enum = maybe_add_dup_statement(context, statement_location, 0, &match_info.input)?;
    // Get the [ConcreteLibfuncId].
    let concrete_enum_type = context.get_variable_sierra_type(match_info.input)?;
    let libfunc_id = match_enum_libfunc_id(context.get_db(), concrete_enum_type)?;

    let args = vec![matched_enum];
    generate_match_code(context, block_gen_stack, libfunc_id, args, &match_info.arms)
}

/// Generates Sierra code for [lowering::StatementSnapshot].
fn generate_statement_snapshot<'db>(
    context: &mut ExprGeneratorContext<'db, '_>,
    statement: &lowering::StatementSnapshot<'db>,
    statement_location: &StatementLocation,
) -> Maybe<()> {
    // Prepare the Sierra input variables.
    let input = maybe_add_dup_statement(context, statement_location, 0, &statement.input)?;

    let ty = context.get_variable_sierra_type(statement.input.var_id)?;
    let func = snapshot_take_libfunc_id(context.get_db(), ty);
    let stmt = simple_basic_statement(
        func,
        &[input],
        &[
            context.get_sierra_variable(statement.original()),
            context.get_sierra_variable(statement.snapshot()),
        ],
    );
    context.push_statement(stmt);
    Ok(())
}

/// Generates Sierra code for [lowering::StatementDesnap].
fn generate_statement_desnap<'db>(
    context: &mut ExprGeneratorContext<'db, '_>,
    statement: &lowering::StatementDesnap<'db>,
    statement_location: &StatementLocation,
) -> Maybe<()> {
    // Dup variables as needed.
    let inputs_after_dup =
        maybe_add_dup_statements(context, statement_location, &[statement.input])?;
    let rename_stmt = simple_basic_statement(
        rename_libfunc_id(
            context.get_db(),
            context.get_variable_sierra_type(statement.input.var_id)?,
        ),
        &inputs_after_dup,
        &[context.get_sierra_variable(statement.output)],
    );
    context.push_statement(rename_stmt);
    Ok(())
}
