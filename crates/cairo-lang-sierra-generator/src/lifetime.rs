#[cfg(test)]
#[path = "lifetime_test.rs"]
mod test;

use std::fmt::Debug;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_lowering as lowering;
use cairo_lang_lowering::{BlockId, VariableId};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use itertools::Itertools;
use lowering::FlatLowered;

use crate::utils::statement_outputs;

pub type StatementLocation = (BlockId, usize);

/// Represents the location where a drop statement for a variable should be added.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum DropLocation {
    BeginningOfBlock(BlockId),
    PostStatement(StatementLocation),
}

/// Represents a location where a variable is being used.
/// Contains the statement location, and the index of the argument within this statement.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct UseLocation {
    /// The statement where the variable is used.
    pub statement_location: StatementLocation,
    /// The index of the argument within the statement.
    pub idx: usize,
}

impl Debug for UseLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?}, {})", self.statement_location, self.idx)
    }
}

/// Represents a Sierra variable by its corresponding lowering [VariableId].
///
/// For example, uninitialized local variables do not have a representation as lowering
/// [VariableId], since they are created in the sierra-generation phase.
/// Instead, we refer to it as [SierraGenVar::UninitializedLocal] by the actual local variable
/// (not the uninitialized version).
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub enum SierraGenVar {
    /// Represents a regular variable.
    LoweringVar(VariableId),
    /// Represents an uninitialized local variable, by the corresponding local variable.
    UninitializedLocal(VariableId),
}

impl From<VariableId> for SierraGenVar {
    fn from(var: VariableId) -> Self {
        SierraGenVar::LoweringVar(var)
    }
}

impl Debug for SierraGenVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LoweringVar(var) => write!(f, "v{}", var.index()),
            Self::UninitializedLocal(var) => write!(f, "UninitializedLocal(v{})", var.index()),
        }
    }
}

/// Information returned by [find_variable_lifetime] regarding the lifetime of variables.
#[derive(Default)]
pub struct VariableLifetimeResult {
    /// A set of [UseLocation] where a variable is used, but not required after.
    ///
    /// Note that a variable may be mentioned twice. For example, when it's last used within two
    /// branches.
    ///
    /// StatementLocation may point to a nonexisting statement after the end of the block -
    /// this means that the last use was in `block.end`.
    pub last_use: OrderedHashSet<UseLocation>,
    /// A map from [DropLocation] to the list of variables that should be dropped at this location.
    pub drops: OrderedHashMap<DropLocation, Vec<SierraGenVar>>,
}
impl VariableLifetimeResult {
    /// Registers where a drop statement should appear.
    fn add_drop(&mut self, var_id: SierraGenVar, drop_location: DropLocation) {
        if let Some(vars) = self.drops.get_mut(&drop_location) {
            vars.push(var_id);
        } else {
            self.drops.insert(drop_location, vec![var_id]);
        }
    }
}

/// Given the lowering of a function, returns lifetime information for all the variables.
/// See [VariableLifetimeResult].
pub fn find_variable_lifetime(
    lowered_function: &FlatLowered,
    local_vars: &OrderedHashSet<VariableId>,
) -> Maybe<VariableLifetimeResult> {
    let mut context = VariableLifetimeContext {
        lowered_function,
        local_vars,
        res: VariableLifetimeResult::default(),
    };
    let mut state = VariableLifetimeState::default();
    let root_block_id = lowered_function.root?;
    inner_find_variable_lifetime(&mut context, root_block_id, &mut state);

    Ok(context.res)
}

/// Context information for [inner_find_variable_lifetime] and its helper functions.
struct VariableLifetimeContext<'a> {
    lowered_function: &'a FlatLowered,
    local_vars: &'a OrderedHashSet<VariableId>,
    res: VariableLifetimeResult,
}

/// Helper function for [find_variable_lifetime].
fn inner_find_variable_lifetime(
    context: &mut VariableLifetimeContext<'_>,
    block_id: BlockId,
    state: &mut VariableLifetimeState,
) {
    let block = &context.lowered_function.blocks[block_id];

    // Go over the block in reverse order, starting from handling the block end.
    match &block.end {
        lowering::FlatBlockEnd::Callsite(remapping) => {
            let vars = remapping.values().copied().collect_vec();
            state.use_variables(context, &vars, (block_id, block.statements.len()));
        }
        lowering::FlatBlockEnd::Return(vars) => {
            state.clear();
            state.use_variables(context, vars, (block_id, block.statements.len()));
        }
        lowering::FlatBlockEnd::Unreachable => {}
    }

    for (idx, statement) in block.statements.iter().enumerate().rev() {
        let statement_location = (block_id, idx);

        // Add the new variables from the statement's output.
        let outputs = statement_outputs(statement, context.lowered_function);
        state.handle_new_variables(
            context,
            &outputs,
            DropLocation::PostStatement(statement_location),
        );

        match statement {
            lowering::Statement::Literal(_)
            | lowering::Statement::Call(_)
            | lowering::Statement::StructConstruct(_)
            | lowering::Statement::StructDestructure(_)
            | lowering::Statement::EnumConstruct(_) => {}
            lowering::Statement::CallBlock(statement_call_block) => {
                inner_find_variable_lifetime(context, statement_call_block.block, state);
            }
            lowering::Statement::MatchExtern(statement_match_extern) => {
                let arm_blocks: Vec<_> =
                    statement_match_extern.arms.iter().map(|(_, block_id)| *block_id).collect();
                handle_match(context, &arm_blocks, state);
            }
            lowering::Statement::MatchEnum(statement_match_enum) => {
                let arm_blocks: Vec<_> =
                    statement_match_enum.arms.iter().map(|(_, block_id)| *block_id).collect();
                handle_match(context, &arm_blocks, state);
            }
        }

        // Mark the input variables as required.
        state.use_variables(context, &statement.inputs(), statement_location);
    }

    // Handle the block's inputs.
    state.handle_new_variables(context, &block.inputs, DropLocation::BeginningOfBlock(block_id));
}

/// Handles a match statement ([lowering::Statement::MatchExtern] and
/// [lowering::Statement::MatchEnum]):
///
/// * Updates the state with the used variables of all the branches.
/// * Adds drop statements for variables which are last-used in only part of the branches.
fn handle_match(
    context: &mut VariableLifetimeContext<'_>,
    arm_blocks: &[BlockId],
    state: &mut VariableLifetimeState,
) {
    // A map from sub-blocks to the set of new last-used variables.
    let mut block_to_used_vars = OrderedHashMap::<BlockId, OrderedHashSet<SierraGenVar>>::default();

    for block_id in arm_blocks {
        let mut state_clone = state.clone();

        inner_find_variable_lifetime(context, *block_id, &mut state_clone);
        assert!(
            block_to_used_vars.insert(*block_id, state_clone.used_variables).is_none(),
            "Using the same block for multiple arms is not supported."
        );
    }

    // Collect all the new used variables, from all the arms.
    let mut all_used_variables = OrderedHashSet::<SierraGenVar>::default();
    for (_block_id, used_variables) in block_to_used_vars.iter() {
        all_used_variables.extend(used_variables.clone());
    }

    // If a variable was last-used in one arm but not in others, it should be dropped
    // in the other arms.
    for (block_id, used_variables) in block_to_used_vars {
        let drop_location = DropLocation::BeginningOfBlock(block_id);
        for var_id in &all_used_variables - &used_variables {
            context.res.add_drop(var_id, drop_location);
        }
    }

    state.extend_with_used_variables(all_used_variables);
}

/// Helper struct with the state maintained by [inner_find_variable_lifetime].
#[derive(Clone, Debug)]
struct VariableLifetimeState {
    /// A set of all the variables used after the current processed statement.
    used_variables: OrderedHashSet<SierraGenVar>,
}
impl VariableLifetimeState {
    fn default() -> Self {
        Self { used_variables: OrderedHashSet::default() }
    }

    /// Marks the given set of variables as used.
    ///
    /// Called with the new used variables of sub-blocks of a match statement.
    fn extend_with_used_variables(&mut self, vars: OrderedHashSet<SierraGenVar>) {
        self.used_variables.extend(vars);
    }

    /// Clears the state.
    fn clear(&mut self) {
        self.used_variables.clear();
    }

    /// Handles new variables in the following cases:
    ///
    /// 1. Returned by a simple call.
    /// 2. Returned by a branching libfunc (called once per branch).
    fn handle_new_variables(
        &mut self,
        context: &mut VariableLifetimeContext<'_>,
        var_ids: &[VariableId],
        drop_location: DropLocation,
    ) {
        for var_id in var_ids {
            let sierra_gen_var = SierraGenVar::LoweringVar(*var_id);
            if !self.used_variables.contains(&sierra_gen_var) {
                // The variable will not be used, drop it.
                context.res.add_drop(sierra_gen_var, drop_location);
            } else {
                // When a variable is defined and used in one match branch, we don't need to drop
                // it in the other branch (unlike the cases where it is defined before the match).
                // Therefore, we remove it from `used_variables`.
                self.used_variables.swap_remove(&sierra_gen_var);
            }

            if context.local_vars.contains(var_id) {
                // When a local variable is defined, the corresponding uninitialized variable is
                // used (by the `store_local` libfunc).
                self.used_variables.insert(SierraGenVar::UninitializedLocal(*var_id));
            }
        }
    }

    /// Mark the given `args` as required.
    fn use_variables(
        &mut self,
        context: &mut VariableLifetimeContext<'_>,
        var_ids: &[VariableId],
        statement_location: StatementLocation,
    ) {
        // Iterate over the variables in reverse order, since we want the last use of each variable.
        for (idx, var_id) in var_ids.iter().enumerate().rev() {
            let sierra_gen_var = SierraGenVar::LoweringVar(*var_id);
            if self.used_variables.insert(sierra_gen_var) {
                // This is the last use of the variable.
                let use_location = UseLocation { statement_location, idx };
                assert!(
                    context.res.last_use.insert(use_location),
                    "Internal compiler error: UseLocation {use_location:?} visited more than once."
                );
            }
        }
    }
}
