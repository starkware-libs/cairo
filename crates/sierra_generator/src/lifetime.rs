#[cfg(test)]
#[path = "lifetime_test.rs"]
mod test;

use diagnostics::Maybe;
use lowering::lower::Lowered;
use lowering::{BlockId, VariableId};
use utils::ordered_hash_map::OrderedHashMap;
use utils::ordered_hash_set::OrderedHashSet;
use utils::unordered_hash_set::UnorderedHashSet;

pub type StatementLocation = (BlockId, usize);

/// Represents the location where a drop statement for a variable should be added.
#[derive(Clone, Copy, Debug)]
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
impl VariableLifetimeResult {
    /// Registers where a drop statement should appear.
    fn add_drop(&mut self, var_id: VariableId, drop_location: DropLocation) {
        if let Some(drop_locations) = self.drops.get_mut(&var_id) {
            drop_locations.push(drop_location);
        } else {
            self.drops.insert(var_id, vec![drop_location]);
        }
    }
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
        state.handle_new_variables(
            &statement.outputs(),
            DropLocation::PostStatement(statement_location),
            res,
        );

        match statement {
            lowering::Statement::Literal(_)
            | lowering::Statement::Call(_)
            | lowering::Statement::StructConstruct(_)
            | lowering::Statement::StructDestructure(_) => {}
            lowering::Statement::CallBlock(statement_call_block) => {
                inner_find_variable_lifetime(
                    lowered_function,
                    statement_call_block.block,
                    state,
                    res,
                );
            }
            lowering::Statement::MatchExtern(statement_match_extern) => {
                let arm_blocks: Vec<_> =
                    statement_match_extern.arms.iter().map(|(_, block_id)| *block_id).collect();
                handle_match(lowered_function, &arm_blocks, state, res);
            }
            lowering::Statement::MatchEnum(_statement_match_enum) => todo!(),
            lowering::Statement::EnumConstruct(_statement_enum_construct) => todo!(),
        }

        // Mark the input variables as required.
        state.use_variables(&statement.inputs(), statement_location, res);
    }

    // Handle the block's inputs.
    state.handle_new_variables(&block.inputs, DropLocation::BeginningOfBlock(block_id), res);
}

/// Handles a match statement ([lowering::Statement::MatchExtern] and
/// [lowering::Statement::MatchEnum]):
///
/// * Updates the state with the used variables of all the branches.
/// * Adds drop statements for variables which are last-used in only part of the branches.
fn handle_match(
    lowered_function: &Lowered,
    arm_blocks: &[BlockId],
    state: &mut VariableLifetimeState,
    res: &mut VariableLifetimeResult,
) {
    // A map from sub-blocks to the set of new last-used variables.
    let mut block_to_new_used_vars =
        OrderedHashMap::<BlockId, OrderedHashSet<VariableId>>::default();

    for block_id in arm_blocks {
        let mut state_clone = state.clone_for_subblock();

        inner_find_variable_lifetime(lowered_function, *block_id, &mut state_clone, res);
        assert!(
            block_to_new_used_vars.insert(*block_id, state_clone.new_used_variables).is_none(),
            "Using the same block for multiple arms is not supported."
        );
    }

    // Collect all the new used variables, from all the arms.
    let mut all_new_used_variables = OrderedHashSet::<VariableId>::default();
    for (_block_id, new_used_variables) in block_to_new_used_vars.iter() {
        all_new_used_variables.extend(new_used_variables.clone());
    }

    // If a variable was last-used in one arm but not in others, it should be dropped
    // in the other arms.
    for (block_id, new_used_variables) in block_to_new_used_vars {
        let drop_location = DropLocation::BeginningOfBlock(block_id);
        for var_id in &all_new_used_variables - &new_used_variables {
            res.add_drop(var_id, drop_location);
        }
    }

    state.extend_with_used_variables(all_new_used_variables);
}

/// Helper struct with the state maintained by [inner_find_variable_lifetime].
#[derive(Clone, Debug)]
struct VariableLifetimeState {
    /// A set of all the variables used after the current processed statement.
    used_variables: UnorderedHashSet<VariableId>,
    /// A subset of used_variables with the variables that were added by the current block.
    new_used_variables: OrderedHashSet<VariableId>,
}
impl VariableLifetimeState {
    fn default() -> Self {
        Self {
            used_variables: UnorderedHashSet::default(),
            new_used_variables: OrderedHashSet::default(),
        }
    }

    /// Returns a clone of the state for a sub-block of a match statement.
    /// In particular, `new_used_variables` is not cloned, and variables added in the sub-block
    /// should later be registered to the parent state by [Self::extend_with_used_variables].
    fn clone_for_subblock(&self) -> Self {
        Self {
            used_variables: self.used_variables.clone(),
            new_used_variables: OrderedHashSet::default(),
        }
    }

    /// Marks the given set of variables as used.
    ///
    /// Called with the new used variables of sub-blocks of a match statement.
    fn extend_with_used_variables(&mut self, vars: OrderedHashSet<VariableId>) {
        self.used_variables.extend(vars.clone());
        self.new_used_variables.extend(vars);
    }

    /// Handles new variables in the following cases:
    ///
    /// 1. Returned by a simple call.
    /// 2. Returned by a branching libfunc (called once per branch).
    fn handle_new_variables(
        &mut self,
        var_ids: &[VariableId],
        drop_location: DropLocation,
        res: &mut VariableLifetimeResult,
    ) {
        for var_id in var_ids {
            if !self.used_variables.contains(var_id) {
                // The variable will not be used, drop it.
                res.add_drop(*var_id, drop_location);
            } else {
                // When a variable is defined and used in one match branch, we don't need to drop
                // it in the other branch (unlike the cases where it is defined before the match).
                // Therefore, we remove it from new_used_variables.
                self.new_used_variables.swap_remove(var_id);
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
                self.new_used_variables.insert(*var_id);
            }
        }
    }
}
