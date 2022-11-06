#[cfg(test)]
#[path = "local_variables_test.rs"]
mod test;

use debug::DebugWithDb;
use itertools::zip_eq;
use lowering::lower::Lowered;
use lowering::{BlockId, VariableId};
use sierra::extensions::lib_func::OutputVarInfo;
use utils::ordered_hash_map::OrderedHashMap;
use utils::ordered_hash_set::OrderedHashSet;

use crate::db::SierraGenGroup;
use crate::utils::{get_concrete_libfunc_id, get_libfunc_signature};

/// Given the lowering of a function, returns the set of variables which should be stored as local
/// variables.
#[allow(dead_code)]
fn find_local_variables(
    db: &dyn SierraGenGroup,
    lowered_function: &Lowered,
) -> Option<OrderedHashSet<VariableId>> {
    let mut res = OrderedHashSet::<VariableId>::default();
    inner_find_local_variables(
        db,
        lowered_function,
        lowered_function.root?,
        LocalVariablesState::default(),
        &mut res,
    )?;
    Some(res)
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
) -> Option<bool> {
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
                let libfunc_signature = get_libfunc_signature(db, concrete_function_id);
                assert_eq!(
                    libfunc_signature.branch_signatures.len(),
                    1,
                    "Unexpected branches in function '{:?}'",
                    statement_call.function.debug(db)
                );

                match libfunc_signature.branch_signatures[0].ap_change {
                    sierra::extensions::lib_func::SierraApChange::Known(_) => {}
                    _ => {
                        state.revoke_temporary_variables();
                        known_ap_change = false;
                    }
                }
                state.register_outputs(
                    &statement_call.inputs,
                    &statement_call.outputs,
                    &libfunc_signature.branch_signatures[0].vars,
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
                let libfunc_signature = get_libfunc_signature(db, concrete_function_id);
                for (block_id, branch_signature) in
                    zip_eq(&statement_match_extern.arms, libfunc_signature.branch_signatures)
                {
                    let mut state_clone = state.clone();

                    state_clone.register_outputs(
                        &statement_match_extern.inputs,
                        &lowered_function.blocks[*block_id].inputs,
                        &branch_signature.vars,
                    );

                    inner_find_local_variables(db, lowered_function, *block_id, state_clone, res)?;
                }
                state.revoke_temporary_variables();
                known_ap_change = false;
                state.mark_outputs_as_temporary(statement);
            }
            lowering::Statement::StructConstruct => todo!(),
            lowering::Statement::StructDestructure => todo!(),
            lowering::Statement::EnumConstruct(_) => todo!(),
            lowering::Statement::MatchEnum(statement_match_enum) => {
                for (_variant, block_id) in &statement_match_enum.arms {
                    let mut state_clone = state.clone();

                    for var_id in &lowered_function.blocks[*block_id].inputs {
                        state_clone.set_variable_status(*var_id, VariableStatus::TemporaryVariable);
                    }

                    inner_find_local_variables(db, lowered_function, *block_id, state_clone, res)?;
                }
                state.revoke_temporary_variables();
                known_ap_change = false;
                state.mark_outputs_as_temporary(statement);
            }
            lowering::Statement::TupleConstruct(_) => todo!(),
            lowering::Statement::TupleDestructure(_) => todo!(),
        }
    }

    // TODO(lior): Handle block.drops.

    match &block.end {
        lowering::BlockEnd::Callsite(vars) | lowering::BlockEnd::Return(vars) => {
            state.use_variables(vars, res);
        }
        lowering::BlockEnd::Unreachable => {}
    }
    Some(known_ap_change)
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
                | sierra::extensions::OutputVarReferenceInfo::Deferred(_)
                | sierra::extensions::OutputVarReferenceInfo::Const => {
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
