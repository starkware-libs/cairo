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
        LocalVariableState::default(),
        &mut res,
    )?;
    Some(res)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum VariableStatus {
    TemporaryVariable,
    Revoked,
}

#[derive(Clone, Debug)]
struct LocalVariableState {
    variables: OrderedHashMap<VariableId, VariableStatus>,
}
impl LocalVariableState {
    fn default() -> Self {
        LocalVariableState { variables: OrderedHashMap::default() }
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
    fn register_outputs(&mut self, var_ids: &[VariableId], var_infos: &[OutputVarInfo]) {
        for (var_id, var_info) in zip_eq(var_ids, var_infos) {
            match var_info.ref_info {
                sierra::extensions::OutputVarReferenceInfo::SameAsParam { .. } => todo!(),
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
        if let Some(VariableStatus::Revoked) = self.variables.get(&var_id) {
            res.insert(var_id);
        }
    }
}

fn inner_find_local_variables(
    db: &dyn SierraGenGroup,
    lowered_function: &Lowered,
    block_id: BlockId,
    mut state: LocalVariableState,
    res: &mut OrderedHashSet<VariableId>,
) -> Option<()> {
    let block = &lowered_function.blocks[block_id];
    for statement in &block.statements {
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
                state.use_variables(&statement_call.inputs, res);
                match libfunc_signature.branch_signatures[0].ap_change {
                    sierra::extensions::lib_func::SierraApChange::Known(_) => {}
                    _ => state.revoke_temporary_variables(),
                }
                state.register_outputs(
                    &statement_call.outputs,
                    &libfunc_signature.branch_signatures[0].vars,
                );
            }
            lowering::Statement::CallBlock(_) => todo!(),
            lowering::Statement::MatchExtern(_) => todo!(),
            lowering::Statement::StructConstruct => todo!(),
            lowering::Statement::StructDestruct => todo!(),
            lowering::Statement::EnumConstruct(_) => todo!(),
            lowering::Statement::MatchEnum(_) => todo!(),
            lowering::Statement::TupleConstruct(_) => todo!(),
            lowering::Statement::TupleDestruct(_) => todo!(),
        }
    }
    Some(())
}
