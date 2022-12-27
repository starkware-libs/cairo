#[cfg(test)]
#[path = "lifetime_test.rs"]
mod test;

use diagnostics::Maybe;
use lowering::lower::Lowered;
use lowering::{BlockId, VariableId};
use utils::ordered_hash_map::OrderedHashMap;
use utils::unordered_hash_set::UnorderedHashSet;

pub type StatementLocation = (BlockId, usize);

/// Represents the location where a drop statement for a variable should be added.
// TODO(lior): Remove the following `allow(dead_code)` once all variants are used.
#[allow(dead_code)]
#[derive(Debug)]
pub enum DropLocation {
    BeginningOfBlock(BlockId),
    PostStatement(StatementLocation),
}

/// Information returned by [find_variable_lifetime] regarding the lifetime of variables.
#[derive(Default)]
pub struct VariableLifetimeResult {
    /// A map from [VariableId] to the statements where it is used, but not required after.
    ///
    /// Note that a variable may be mentioned twice. For example, when it's last used within two
    /// branches.
    ///
    /// StatementLocation may point to a nonexisting statement after the end of the block -
    /// this means that the last use was in `block.end`.
    last_use: OrderedHashMap<VariableId, Vec<StatementLocation>>,
    /// A map from [VariableId] to the statements where it should be dropped.
    drops: OrderedHashMap<VariableId, Vec<DropLocation>>,
}

/// Given the lowering of a function, returns lifetime information for all the variables.
/// See [VariableLifetimeResult].
// TODO(lior): Remove the following `allow(dead_code)` once this function is used.
#[allow(dead_code)]
pub fn find_variable_lifetime(lowered_function: &Lowered) -> Maybe<VariableLifetimeResult> {
    let mut res = VariableLifetimeResult::default();
    inner_find_variable_lifetime(
        lowered_function,
        lowered_function.root?,
        &mut VariableLifetimeState::default(),
        &mut res,
    );
    Ok(res)
}

/// Helper function for [find_variable_lifetime].
fn inner_find_variable_lifetime(
    lowered_function: &Lowered,
    block_id: BlockId,
    state: &mut VariableLifetimeState,
    res: &mut VariableLifetimeResult,
) {
    let block = &lowered_function.blocks[block_id];

    // Go over the block in reverse order, starting from handling the block end.
    match &block.end {
        lowering::BlockEnd::Callsite(vars) | lowering::BlockEnd::Return(vars) => {
            state.use_variables(vars, (block_id, block.statements.len()), res);
        }
        lowering::BlockEnd::Unreachable => {}
    }

    for (idx, statement) in block.statements.iter().enumerate().rev() {
        let statement_location = (block_id, idx);

        // Add the new variables from the statement's output.
        state.handle_new_variables(&statement.outputs(), statement_location, res);

        match statement {
            lowering::Statement::Literal(_) | lowering::Statement::Call(_) => {}
            lowering::Statement::CallBlock(_statement_call_block) => todo!(),
            lowering::Statement::MatchExtern(_statement_match_extern) => todo!(),
            lowering::Statement::MatchEnum(_statement_match_enum) => todo!(),
            lowering::Statement::StructConstruct(_statement_struct_construct) => todo!(),
            lowering::Statement::StructDestructure(_statement_struct_destructure) => todo!(),
            lowering::Statement::EnumConstruct(_statement_enum_construct) => todo!(),
        }

        // Mark the input variables as required.
        state.use_variables(&statement.inputs(), statement_location, res);
    }
}

/// Helper struct with the state maintained by [inner_find_variable_lifetime].
#[derive(Clone, Debug)]
struct VariableLifetimeState {
    /// A set of all the variables used after the current processed statement.
    used_variables: UnorderedHashSet<VariableId>,
}
impl VariableLifetimeState {
    fn default() -> Self {
        VariableLifetimeState { used_variables: UnorderedHashSet::default() }
    }

    /// Handles new variables in the following cases:
    ///
    /// 1. Returned by a simple call.
    /// 2. Returned by a branching libfunc (called once per branch).
    fn handle_new_variables(
        &mut self,
        var_ids: &[VariableId],
        statement_location: StatementLocation,
        res: &mut VariableLifetimeResult,
    ) {
        for var_id in var_ids {
            if !self.used_variables.contains(var_id) {
                // The variable will not be used, drop it immediately after the statement.
                let new_drop_location = DropLocation::PostStatement(statement_location);
                if let Some(drop_locations) = res.drops.get_mut(var_id) {
                    drop_locations.push(new_drop_location);
                } else {
                    res.drops.insert(*var_id, vec![new_drop_location]);
                }
            }
        }
    }

    /// Mark the given `args` as required.
    fn use_variables(
        &mut self,
        var_ids: &[VariableId],
        statement_location: StatementLocation,
        res: &mut VariableLifetimeResult,
    ) {
        for var_id in var_ids {
            if self.used_variables.insert(*var_id) {
                // This is the last use of the variable.
                if let Some(statement_locations) = res.last_use.get_mut(var_id) {
                    statement_locations.push(statement_location);
                } else {
                    res.last_use.insert(*var_id, vec![statement_location]);
                }
            }
        }
    }
}
