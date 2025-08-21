#[cfg(test)]
#[path = "cse_test.rs"]
mod test;

use std::iter::zip;

use cairo_lang_semantic::{ConcreteVariant, TypeId};
use cairo_lang_utils::unordered_hash_map::{Entry, UnorderedHashMap};

use crate::ids::FunctionId;
use crate::optimizations::var_renamer::VarRenamer;
use crate::utils::RebuilderEx;
use crate::{BlockId, Lowered, Statement, VariableArena, VariableId};

/// A key that uniquely identifies a sub-expression that can be eliminated.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum ExpressionKey<'db> {
    /// A struct construction with given inputs.
    StructConstruct(TypeId<'db>, Vec<VariableId>),
    /// A member access of a struct.
    StructDestructure(VariableId),
    /// An enum construction with given variant and input.
    EnumConstruct(ConcreteVariant<'db>, VariableId),
    /// A snapshot operation with given input.
    Snapshot(VariableId),
    /// A desnap operation with given input.
    Desnap(VariableId),
    /// A pure function call with given function and inputs.
    PureCall(FunctionId<'db>, Vec<VariableId>),
}

/// Context for the CSE optimization pass.
struct CseContext<'db> {
    /// Maps expression keys to the variable that holds the result of that expression
    expression_map: UnorderedHashMap<ExpressionKey<'db>, Vec<VariableId>>,
    /// Maps variables to their replacement (for variables that have been eliminated)
    var_replacements: UnorderedHashMap<VariableId, VariableId>,
    /// A mapping from new non-snapshot variables after taking snapshot, to the original variable
    /// the snapshot was taken from. Used to canonicalize the variable after taking snapshot.
    snapshot_remappings: UnorderedHashMap<VariableId, VariableId>,
    /// Set of statements to remove (by block and statement index)
    statements_to_remove: Vec<(BlockId, usize)>,
    /// Arena.
    variables: &'db VariableArena<'db>,
}

impl<'db> CseContext<'db> {
    fn new(variables: &'db VariableArena<'db>) -> Self {
        Self {
            expression_map: UnorderedHashMap::default(),
            var_replacements: UnorderedHashMap::default(),
            snapshot_remappings: UnorderedHashMap::default(),
            statements_to_remove: Vec::new(),
            variables,
        }
    }

    /// Processes a statement and potentially marks it for elimination
    fn process_statement(&mut self, block_id: BlockId, stmt_idx: usize, stmt: &Statement<'db>) {
        if stmt.inputs().iter().any(|input| self.variables[input.var_id].info.droppable.is_err())
            || stmt.outputs().iter().any(|output| self.variables[*output].info.copyable.is_err())
        {
            return;
        }
        // Check if this statement can be eliminated
        let key = match stmt {
            Statement::StructConstruct(s) => ExpressionKey::StructConstruct(
                self.variables[s.output].ty,
                s.inputs.iter().map(|usage| self.resolve_var(usage.var_id)).collect(),
            ),
            Statement::StructDestructure(s) => {
                ExpressionKey::StructDestructure(self.resolve_var(s.input.var_id))
            }
            Statement::EnumConstruct(s) => {
                ExpressionKey::EnumConstruct(s.variant, self.resolve_var(s.input.var_id))
            }
            Statement::Snapshot(s) => {
                self.snapshot_remappings.insert(s.original(), s.input.var_id);
                ExpressionKey::Snapshot(self.resolve_var(s.input.var_id))
            }
            Statement::Desnap(s) => ExpressionKey::Desnap(self.resolve_var(s.input.var_id)),
            Statement::Call(s) if self.is_pure_function(&s.function) => ExpressionKey::PureCall(
                s.function,
                s.inputs.iter().map(|usage| self.resolve_var(usage.var_id)).collect(),
            ),
            _ => return, // Not optimizable
        };
        match self.expression_map.entry(key) {
            Entry::Vacant(entry) => {
                entry.insert(stmt.outputs().to_vec());
            }
            Entry::Occupied(entry) => {
                self.statements_to_remove.push((block_id, stmt_idx));
                self.var_replacements
                    .extend(zip(stmt.outputs().iter().copied(), entry.get().iter().copied()));
            }
        }
    }

    /// Resolves a variable through the replacement chain
    fn resolve_var(&self, var: VariableId) -> VariableId {
        match self.var_replacements.get(&var).or_else(|| self.snapshot_remappings.get(&var)) {
            Some(&replacement) => self.resolve_var(replacement),
            None => var,
        }
    }

    /// Determines if a function is pure and safe to optimize
    /// For now, we're conservative and only optimize very basic operations
    fn is_pure_function(&self, _function: &FunctionId<'db>) -> bool {
        // TODO: Implement logic to determine if a function is pure
        // For now, we're being conservative and not optimizing any function calls
        false
    }
}

/// Performs common sub-expression elimination on the lowered program.
/// This optimization identifies identical expressions and replaces redundant computations
/// with references to the first computed result.
pub fn cse<'db>(lowered: &mut Lowered<'db>) {
    if lowered.blocks.is_empty() {
        return;
    }
    let mut ctx = CseContext::new(&lowered.variables);

    for block_id in (0..lowered.blocks.len()).map(BlockId) {
        let block = &lowered.blocks[block_id];
        for (stmt_idx, stmt) in block.statements.iter().enumerate() {
            ctx.process_statement(block_id, stmt_idx, stmt);
        }
        // Clearing the map to not propagate the optimization between blocks.
        ctx.expression_map.clear();
    }
    let CseContext { statements_to_remove, var_replacements: renamed_vars, .. } = ctx;
    // Remove statements in reverse order to maintain correct indices.
    for (block_id, stmt_idx) in statements_to_remove.into_iter().rev() {
        lowered.blocks[block_id].statements.remove(stmt_idx);
    }
    let mut renamer = VarRenamer { renamed_vars };
    for block in lowered.blocks.iter_mut() {
        *block = renamer.rebuild_block(block);
    }
}
