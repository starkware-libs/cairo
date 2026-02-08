//! Variable forwarding optimization using equality analysis.
//!
//! Replaces variable usages with their canonical representative from
//! equality analysis. Only copy-type variables are forwarded, since
//! they can have multiple simultaneous uses without ownership issues.

#[cfg(test)]
#[path = "variable_forwarding_test.rs"]
mod test;

use salsa::Database;

use crate::analysis::StatementLocation;
use crate::analysis::def_site::{DefSiteAnalysis, DefSites};
use crate::analysis::dom::Dominators;
use crate::analysis::equality_analysis::{EqualityAnalysis, EqualityState};
use crate::objects::blocks::Blocks;
use crate::{BlockEnd, BlockId, Lowered, VariableArena, VariableId};

/// Applies variable forwarding optimization.
///
/// This optimization replaces variable usages with their canonical representative
/// from equality analysis, reducing the number of live variables and enabling
/// further optimizations. Only copy-type variables are forwarded.
pub fn variable_forwarding(db: &dyn Database, lowered: &mut Lowered<'_>) {
    if lowered.blocks.is_empty() {
        return;
    }

    let exit_states = EqualityAnalysis::analyze(db, lowered);
    let dominators = Dominators::analyze(lowered);
    let def_sites = DefSiteAnalysis::analyze(lowered);
    let mut ctx = ForwardingCtx {
        variables: &lowered.variables,
        blocks: &mut lowered.blocks,
        dominators,
        def_sites,
    };

    for (block_idx, state) in exit_states.iter().enumerate() {
        let block_id = BlockId(block_idx);
        let Some(state) = state else {
            continue;
        };

        // Forward statement inputs.
        let num_stmts = ctx.blocks[block_id].statements.len();
        for stmt_idx in 0..num_stmts {
            let loc = (block_id, stmt_idx);
            let num_inputs = ctx.blocks[block_id].statements[stmt_idx].inputs().len();
            for input_idx in 0..num_inputs {
                let var_id = ctx.blocks[block_id].statements[stmt_idx].inputs()[input_idx].var_id;
                if let Some(rep) = ctx.try_forward(var_id, loc, state) {
                    ctx.blocks[block_id].statements[stmt_idx].inputs_mut()[input_idx].var_id = rep;
                }
            }
        }

        // Forward block-end inputs.
        ctx.forward_block_end(block_id, state);
    }
}

struct ForwardingCtx<'a, 'db> {
    variables: &'a VariableArena<'db>,
    blocks: &'a mut Blocks<'db>,
    dominators: Dominators,
    def_sites: DefSites,
}

impl<'a, 'db> ForwardingCtx<'a, 'db> {
    fn is_visible(&self, loc: StatementLocation, var_id: VariableId) -> bool {
        match self.def_sites[var_id.index()] {
            crate::analysis::DefLocation::Statement((block_id, stmt_idx)) => {
                self.dominators.dominates(block_id, loc.0) && stmt_idx <= loc.1
            }
            crate::analysis::DefLocation::BlockEntry(block_id) => {
                self.dominators.dominates(block_id, loc.0)
            }
        }
    }

    /// Returns `Some(rep)` when `var_id` can be forwarded to its representative at `loc`.
    /// Only forwards copy-type variables.
    fn try_forward(
        &self,
        var_id: VariableId,
        loc: StatementLocation,
        state: &EqualityState<'_>,
    ) -> Option<VariableId> {
        let rep = state.find_immut(var_id);
        if rep == var_id || !self.is_visible(loc, rep) {
            return None;
        }
        if self.variables[var_id].info.copyable.is_err() {
            return None;
        }
        Some(rep)
    }

    /// Forwards variable usages in a block's end node.
    fn forward_block_end(&mut self, block_id: BlockId, state: &EqualityState<'_>) {
        let loc = (block_id, self.blocks[block_id].statements.len());
        let mut end = std::mem::replace(&mut self.blocks[block_id].end, BlockEnd::NotSet);
        let end_inputs: Vec<_> = match &mut end {
            BlockEnd::Return(returns, _) => returns.iter_mut().collect(),
            BlockEnd::Panic(var) => vec![var],
            BlockEnd::Goto(_, remapping) => remapping.iter_mut().map(|(_, src)| src).collect(),
            BlockEnd::Match { info } => info.inputs_mut().iter_mut().collect(),
            BlockEnd::NotSet => vec![],
        };
        for input in end_inputs {
            if let Some(rep) = self.try_forward(input.var_id, loc, state) {
                input.var_id = rep;
            }
        }
        self.blocks[block_id].end = end;
    }
}
