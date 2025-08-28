#[cfg(test)]
#[path = "cse_test.rs"]
mod test;

use std::iter::zip;

use cairo_lang_semantic::items::constant::ConstValueId;
use cairo_lang_semantic::{ConcreteVariant, TypeId};
use cairo_lang_utils::unordered_hash_map::{Entry, UnorderedHashMap};
use itertools::Itertools;

use crate::ids::FunctionId;
use crate::optimizations::var_renamer::VarRenamer;
use crate::utils::RebuilderEx;
use crate::{BlockEnd, BlockId, Lowered, Statement, VariableArena, VariableId};

/// A key that uniquely identifies a sub-expression that can be eliminated.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum ExpressionKey<'db> {
    /// A constant with given value.
    Const(ConstValueId<'db>, bool),
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
    /// Maps expression keys to the variable that holds the result of that expression in the
    /// current block.
    expression_map: UnorderedHashMap<ExpressionKey<'db>, Vec<VariableId>>,
    /// Maps variables to their replacement (for variables that have been eliminated)
    var_replacements: UnorderedHashMap<VariableId, VariableId>,
    /// A mapping from new non-snapshot variables after taking snapshot, to the original variable
    /// the snapshot was taken from. Used to canonicalize the variable after taking snapshot.
    /// Separate from `var_replacements` to avoid breaking SSA, as this is added for non-removed
    /// statements as well.
    snapshot_remappings: UnorderedHashMap<VariableId, VariableId>,
    /// Arena.
    variables: &'db VariableArena<'db>,
}

impl<'db> CseContext<'db> {
    fn new(variables: &'db VariableArena<'db>) -> Self {
        Self {
            expression_map: UnorderedHashMap::default(),
            var_replacements: UnorderedHashMap::default(),
            snapshot_remappings: UnorderedHashMap::default(),
            variables,
        }
    }

    /// Processes a statement and returns if it should be eliminated.
    fn process_statement(&mut self, stmt: &Statement<'db>) -> bool {
        let outputs = stmt.outputs();
        if outputs.iter().any(|output| self.variables[*output].info.copyable.is_err())
            || stmt
                .inputs()
                .iter()
                .any(|input| self.variables[input.var_id].info.droppable.is_err())
        {
            return false;
        }
        // Check if this statement can be eliminated
        let key = match stmt {
            Statement::Const(c) => ExpressionKey::Const(c.value, c.boxed),
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
            _ => return false, // Not optimizable
        };
        match self.expression_map.entry(key) {
            Entry::Vacant(entry) => {
                entry.insert(outputs.to_vec());
                false
            }
            Entry::Occupied(entry) => {
                self.var_replacements
                    .extend(zip(outputs.iter().copied(), entry.get().iter().copied()));
                true
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
///
/// Blocks must be ordered topologically for the execution to be valid.
pub fn cse<'db>(lowered: &mut Lowered<'db>) {
    if lowered.blocks.is_empty() {
        return;
    }
    let mut ctx = CseContext::new(&lowered.variables);
    let mut block_expression_map = UnorderedHashMap::<BlockId, _>::default();
    block_expression_map.insert(BlockId::root(), Default::default());
    for block_id in (0..lowered.blocks.len()).map(BlockId) {
        let block = &mut lowered.blocks[block_id];
        ctx.expression_map = block_expression_map
            .remove(&block_id)
            .unwrap_or_else(|| panic!("{block_id:?} expressions were not prepared"));
        let mut statements_to_remove = Vec::new();
        for (stmt_idx, stmt) in block.statements.iter().enumerate() {
            if ctx.process_statement(stmt) {
                statements_to_remove.push(stmt_idx);
            }
        }
        for stmt_idx in statements_to_remove.into_iter().rev() {
            block.statements.remove(stmt_idx);
        }
        match &block.end {
            // No following blocks, no need to propagate expressions.
            BlockEnd::NotSet | BlockEnd::Return(..) | BlockEnd::Panic(..) => {}
            BlockEnd::Match { info } => {
                // Propagating expressions to all match arms.
                for arm in info.arms() {
                    let next = arm.block_id;
                    assert!(
                        block_expression_map.insert(next, ctx.expression_map.clone()).is_none(),
                        "{next:?} was previously propagated - should not happen on match arms.",
                    );
                }
            }
            // TODO(orizi): Use the remapping on the propagated expressions.
            BlockEnd::Goto(block_id, _remapping) => match block_expression_map.entry(*block_id) {
                Entry::Vacant(entry) => {
                    // First time meeting this block, propagate expressions.
                    entry.insert(std::mem::take(&mut ctx.expression_map));
                }
                Entry::Occupied(mut entry) => {
                    let e = std::mem::take(entry.get_mut());
                    // Merge expressions from different `BlockEnd::Goto`s to assure we only leave
                    // values known by all leading blocks.
                    entry.insert(e.filter(|k, v| {
                        if let Some(new_val) = ctx.expression_map.remove(k) {
                            new_val == *v
                        } else {
                            false
                        }
                    }));
                }
            },
        }
    }
    assert!(
        block_expression_map.is_empty(),
        "Some blocks were not processed: [{}]",
        block_expression_map
            .iter_sorted_by_key(|(k, _)| k.0)
            .map(|(k, _)| format!("{k:?}"))
            .join(", ")
    );
    let CseContext { var_replacements: renamed_vars, .. } = ctx;
    let mut renamer = VarRenamer { renamed_vars };
    for block in lowered.blocks.iter_mut() {
        *block = renamer.rebuild_block(block);
    }
}
