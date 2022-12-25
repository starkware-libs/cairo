#[cfg(test)]
#[path = "local_variables_test.rs"]
mod test;

use diagnostics::Maybe;
use itertools::zip_eq;
use lowering::lower::Lowered;
use lowering::{BlockId, VariableId};
use sierra::extensions::lib_func::OutputVarInfo;
use utils::ordered_hash_map::OrderedHashMap;
use utils::ordered_hash_set::OrderedHashSet;

use crate::db::SierraGenGroup;
use crate::replace_ids::{DebugReplacer, SierraIdReplacer};
use crate::utils::{
    enum_init_libfunc_id, get_concrete_libfunc_id, get_libfunc_signature, match_enum_libfunc_id,
    struct_construct_libfunc_id, struct_deconstruct_libfunc_id,
};

/// Given the lowering of a function, returns the set of variables which should be stored as local
/// variables.
pub fn find_local_variables(
    db: &dyn SierraGenGroup,
    lowered_function: &Lowered,
) -> Maybe<OrderedHashSet<VariableId>> {
    let mut res = OrderedHashSet::<VariableId>::default();
    inner_find_local_variables(
        db,
        lowered_function,
        lowered_function.root?,
        LocalVariablesState::default(),
        &mut res,
    )?;
    Ok(res)
}

/// Helper function for [find_local_variables].
///
/// Returns true if the code has a known ap change.
fn inner_find_local_variables(
    db: &dyn SierraGenGroup,
    lowered_function: &Lowered,
    block_id: BlockId,
    mut state: LocalVariablesState,
    res: &mut OrderedHashSet<VariableId>,
) -> Maybe<bool> {
    let block = &lowered_function.blocks[block_id];
    let mut known_ap_change = true;

    for statement in &block.statements {
        // All the input variables should be available.
        state.use_variables(&statement.inputs(), res);

        match statement {
            lowering::Statement::Literal(statement_literal) => {
                // Treat literal as a temporary variable.
                state.set_variable_status(
                    statement_literal.output,
                    VariableStatus::TemporaryVariable,
                );
            }
            lowering::Statement::Call(statement_call) => {
                let (_, concrete_function_id) =
                    get_concrete_libfunc_id(db, statement_call.function);

                handle_function_call(
                    db,
                    &mut state,
                    &mut known_ap_change,
                    concrete_function_id,
                    &statement_call.inputs,
                    &statement_call.outputs,
                );
            }
            lowering::Statement::CallBlock(statement_call_block) => {
                let block_known_ap_change = inner_find_local_variables(
                    db,
                    lowered_function,
                    statement_call_block.block,
                    state.clone(),
                    res,
                )?;
                if !block_known_ap_change {
                    state.revoke_temporary_variables();
                    known_ap_change = false;
                }
                state.mark_outputs_as_temporary(statement);
            }
            lowering::Statement::MatchExtern(statement_match_extern) => {
                let (_, concrete_function_id) =
                    get_concrete_libfunc_id(db, statement_match_extern.function);
                let arm_blocks: Vec<_> =
                    statement_match_extern.arms.iter().map(|(_, block_id)| *block_id).collect();
                known_ap_change &= handle_match(
                    db,
                    lowered_function,
                    concrete_function_id,
                    &arm_blocks,
                    statement,
                    &mut state,
                    res,
                )?;
            }
            lowering::Statement::MatchEnum(statement_match_enum) => {
                let concrete_enum_type = db.get_concrete_type_id(
                    lowered_function.variables[statement_match_enum.input].ty,
                )?;
                let concrete_function_id = match_enum_libfunc_id(db, concrete_enum_type);

                known_ap_change &= handle_match(
                    db,
                    lowered_function,
                    concrete_function_id,
                    &statement_match_enum
                        .arms
                        .iter()
                        .map(|(_variant, block_id)| *block_id)
                        .collect::<Vec<_>>(),
                    statement,
                    &mut state,
                    res,
                )?;
            }
            lowering::Statement::StructConstruct(statement_struct_construct) => {
                let ty = db.get_concrete_type_id(
                    lowered_function.variables[statement_struct_construct.output].ty,
                )?;
                handle_function_call(
                    db,
                    &mut state,
                    &mut known_ap_change,
                    struct_construct_libfunc_id(db, ty),
                    &statement_struct_construct.inputs,
                    &[statement_struct_construct.output],
                );
            }
            lowering::Statement::StructDestructure(statement_struct_destructure) => {
                let ty = db.get_concrete_type_id(
                    lowered_function.variables[statement_struct_destructure.input].ty,
                )?;
                handle_function_call(
                    db,
                    &mut state,
                    &mut known_ap_change,
                    struct_deconstruct_libfunc_id(db, ty),
                    &[statement_struct_destructure.input],
                    &statement_struct_destructure.outputs,
                );
            }
            lowering::Statement::EnumConstruct(statement_enum_construct) => {
                let ty = db.get_concrete_type_id(
                    lowered_function.variables[statement_enum_construct.output].ty,
                )?;
                handle_function_call(
                    db,
                    &mut state,
                    &mut known_ap_change,
                    enum_init_libfunc_id(db, ty, statement_enum_construct.variant.idx),
                    &[statement_enum_construct.input],
                    &[statement_enum_construct.output],
                );
            }
        }
    }

    // TODO(lior): Handle block.drops.

    match &block.end {
        lowering::BlockEnd::Callsite(vars) | lowering::BlockEnd::Return(vars) => {
            state.use_variables(vars, res);
        }
        lowering::BlockEnd::Unreachable => {}
    }
    Ok(known_ap_change)
}

/// Handles a match ([lowering::Statement::MatchExtern] and [lowering::Statement::MatchEnum]).
///
/// Returns true if executing the entire match results in a known ap change.
fn handle_match(
    db: &dyn SierraGenGroup,
    lowered_function: &Lowered,
    concrete_function_id: sierra::ids::ConcreteLibFuncId,
    arm_blocks: &[BlockId],
    statement: &lowering::Statement,
    state: &mut LocalVariablesState,
    res: &mut OrderedHashSet<id_arena::Id<lowering::Variable>>,
) -> Maybe<bool> {
    // The number of branches that continue to the next statement after the match.
    let mut reachable_branches: usize = 0;
    // Is the ap change known after all of the branches.
    let mut reachable_branches_known_ap_change: bool = true;

    let libfunc_signature = get_libfunc_signature(db, concrete_function_id);
    for (block_id, branch_signature) in zip_eq(arm_blocks, libfunc_signature.branch_signatures) {
        let mut state_clone = state.clone();

        state_clone.register_outputs(
            &statement.inputs(),
            &lowered_function.blocks[*block_id].inputs,
            &branch_signature.vars,
        );

        let inner_known_ap_change =
            inner_find_local_variables(db, lowered_function, *block_id, state_clone, res)?;

        // Update reachable_branches and reachable_branches_known_ap_change.
        if let lowering::BlockEnd::Callsite(_) = lowered_function.blocks[*block_id].end {
            reachable_branches += 1;
            if !inner_known_ap_change {
                reachable_branches_known_ap_change = false;
            }
        }
    }

    // If there is more than one branch that reaches this point, or ap change is unknown
    // for at least one of them, revoke the temporary variables.
    let known_ap_change = if reachable_branches > 1 || !reachable_branches_known_ap_change {
        state.revoke_temporary_variables();
        false
    } else {
        true
    };
    state.mark_outputs_as_temporary(statement);

    Ok(known_ap_change)
}

/// Helper function for statements that result in a simple function call, such as
/// [lowering::Statement::Call] and [lowering::Statement::StructConstruct].
fn handle_function_call(
    db: &dyn SierraGenGroup,
    state: &mut LocalVariablesState,
    known_ap_change: &mut bool,
    concrete_function_id: sierra::ids::ConcreteLibFuncId,
    inputs: &[lowering::VariableId],
    outputs: &[lowering::VariableId],
) {
    let libfunc_signature = get_libfunc_signature(db, concrete_function_id.clone());
    assert_eq!(
        libfunc_signature.branch_signatures.len(),
        1,
        "Unexpected branches in '{}'.",
        DebugReplacer { db }.replace_libfunc_id(&concrete_function_id)
    );

    match libfunc_signature.branch_signatures[0].ap_change {
        sierra::extensions::lib_func::SierraApChange::Known { .. } => {}
        _ => {
            state.revoke_temporary_variables();
            *known_ap_change = false;
        }
    }

    let vars = &libfunc_signature.branch_signatures[0].vars;
    assert_eq!(
        outputs.len(),
        vars.len(),
        "Wrong number of outputs for '{}'. The 'extern' declaration of the libfunc does not match \
         the Sierra definition.",
        DebugReplacer { db }.replace_libfunc_id(&concrete_function_id)
    );
    state.register_outputs(inputs, outputs, vars);
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum VariableStatus {
    TemporaryVariable,
    Revoked,
    /// Indicates that the variable is essentially the same as another variable.
    /// If this variables needs to be stored as local variable, the aliased variable will be stored
    /// instead.
    Alias(VariableId),
}

#[derive(Clone, Debug)]
struct LocalVariablesState {
    /// A map from a variable id to its status. See [VariableStatus].
    variables: OrderedHashMap<VariableId, VariableStatus>,
}
impl LocalVariablesState {
    fn default() -> Self {
        LocalVariablesState { variables: OrderedHashMap::default() }
    }

    /// Marks all temporary variables as revoked.
    fn revoke_temporary_variables(&mut self) {
        for (_var_id, status) in self.variables.iter_mut() {
            if *status == VariableStatus::TemporaryVariable {
                *status = VariableStatus::Revoked;
            }
        }
    }

    /// Registers output variables of a libfunc.
    fn register_outputs(
        &mut self,
        params: &[VariableId],
        var_ids: &[VariableId],
        var_infos: &[OutputVarInfo],
    ) {
        for (var_id, var_info) in zip_eq(var_ids, var_infos) {
            match var_info.ref_info {
                sierra::extensions::OutputVarReferenceInfo::SameAsParam { param_idx } => {
                    self.set_variable_status(*var_id, VariableStatus::Alias(params[param_idx]));
                }
                sierra::extensions::OutputVarReferenceInfo::NewTempVar { .. }
                | sierra::extensions::OutputVarReferenceInfo::Deferred(_) => {
                    self.set_variable_status(*var_id, VariableStatus::TemporaryVariable);
                }
                sierra::extensions::OutputVarReferenceInfo::NewLocalVar => {}
            }
        }
    }

    fn set_variable_status(&mut self, var_id: VariableId, status: VariableStatus) {
        assert!(
            self.variables.insert(var_id, status).is_none(),
            "Variable {var_id:?} defined more than once."
        );
    }

    /// Prepares the given `args` to be used as arguments for a libfunc.
    fn use_variables(&mut self, var_ids: &[VariableId], res: &mut OrderedHashSet<VariableId>) {
        for var_id in var_ids {
            self.use_variable(*var_id, res);
        }
    }

    /// Prepares the given `arg` to be used as an argument for a libfunc.
    fn use_variable(&mut self, var_id: VariableId, res: &mut OrderedHashSet<VariableId>) {
        match self.variables.get(&var_id) {
            Some(VariableStatus::Revoked) => {
                res.insert(var_id);
            }
            Some(VariableStatus::Alias(alias)) => {
                // Recursively visit `alias`.
                self.use_variable(*alias, res);
            }
            Some(VariableStatus::TemporaryVariable) | None => {}
        }
    }

    /// Marks all the outputs of the statement as [VariableStatus::TemporaryVariable].
    fn mark_outputs_as_temporary(&mut self, statement: &lowering::Statement) {
        for var_id in statement.outputs() {
            self.set_variable_status(var_id, VariableStatus::TemporaryVariable);
        }
    }
}
