#[cfg(test)]
#[path = "local_variables_test.rs"]
mod test;

use lowering::lower::Lowered;
use lowering::{BlockId, VariableId};
use utils::ordered_hash_map::OrderedHashMap;
use utils::ordered_hash_set::OrderedHashSet;

use crate::db::SierraGenGroup;

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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum VariableStatus {
    TemporaryVariable,
    Revoked,
}

#[derive(Clone, Debug)]
struct LocalVariablesState {
    variables: OrderedHashMap<VariableId, VariableStatus>,
}
impl LocalVariablesState {
    fn default() -> Self {
        LocalVariablesState { variables: OrderedHashMap::default() }
    }

    /// Marks all temporary variables as revoked.
    // TODO(lior): Remove allow(dead_code).
    #[allow(dead_code)]
    fn revoke_temporary_variables(&mut self) {
        for (_var_id, status) in self.variables.iter_mut() {
            if *status == VariableStatus::TemporaryVariable {
                *status = VariableStatus::Revoked;
            }
        }
    }

    fn set_variable_status(&mut self, var_id: VariableId, status: VariableStatus) {
        assert!(
            self.variables.insert(var_id, status).is_none(),
            "Variable {var_id:?} defined more than once."
        );
    }
}

fn inner_find_local_variables(
    _db: &dyn SierraGenGroup,
    lowered_function: &Lowered,
    block_id: BlockId,
    mut state: LocalVariablesState,
    _res: &mut OrderedHashSet<VariableId>,
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
            lowering::Statement::Call(_) => todo!(),
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
