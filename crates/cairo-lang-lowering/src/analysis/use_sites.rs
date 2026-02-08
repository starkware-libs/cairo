//! Mutable use-site tracking for lowered IR variables.
//!
//! Tracks where each variable is used (statement inputs and block-end inputs).
//! Updated incrementally as forwarding runs, so later checks see current use counts.

use crate::analysis::StatementLocation;
use crate::{BlockEnd, Lowered, VariableId};

/// Tracks use sites for each variable, indexed by variable arena index.
pub struct UseSites {
    sites: Vec<Vec<StatementLocation>>,
}

impl UseSites {
    /// Builds use-site information by scanning all statements and block ends.
    pub fn analyze(lowered: &Lowered<'_>) -> Self {
        let mut sites = vec![vec![]; lowered.variables.len()];
        for (block_id, block) in lowered.blocks.iter() {
            for (stmt_idx, stmt) in block.statements.iter().enumerate() {
                for input in stmt.inputs() {
                    sites[input.var_id.index()].push((block_id, stmt_idx));
                }
            }
            let end_loc = (block_id, block.statements.len());
            match &block.end {
                BlockEnd::Return(returns, _) => {
                    for ret in returns {
                        sites[ret.var_id.index()].push(end_loc);
                    }
                }
                BlockEnd::Panic(var) => {
                    sites[var.var_id.index()].push(end_loc);
                }
                BlockEnd::Goto(_, remapping) => {
                    for (_, src) in remapping.iter() {
                        sites[src.var_id.index()].push(end_loc);
                    }
                }
                BlockEnd::Match { info } => {
                    for input in info.inputs() {
                        sites[input.var_id.index()].push(end_loc);
                    }
                }
                BlockEnd::NotSet => {}
            }
        }
        Self { sites }
    }

    /// Returns how many use sites remain for `var`.
    pub fn use_count(&self, var: VariableId) -> usize {
        self.sites[var.index()].len()
    }

    /// Removes one use of `var` at `loc`.
    pub fn remove_use(&mut self, var: VariableId, loc: StatementLocation) {
        let sites = &mut self.sites[var.index()];
        if let Some(pos) = sites.iter().position(|s| *s == loc) {
            sites.swap_remove(pos);
        }
    }

    /// Adds a use of `var` at `loc`.
    pub fn add_use(&mut self, var: VariableId, loc: StatementLocation) {
        self.sites[var.index()].push(loc);
    }
}
