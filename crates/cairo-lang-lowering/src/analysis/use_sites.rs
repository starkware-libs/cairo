//! Mutable use-site tracking for lowered IR variables.
//!
//! Tracks where each variable is used (statement inputs and block-end inputs).
//! Updated incrementally as forwarding runs, so later checks see current use counts.
//!
//! A use is identified by the `(variable, UseLocation)` pair: a single statement
//! that consumes the same variable in multiple input slots (e.g. `felt252_add(v, v)`)
//! is recorded once. This matches forwarding's rename semantics — a single rename
//! at a `UseLocation` rewrites every input slot at that location.

use cairo_lang_utils::ordered_hash_map::{Entry, OrderedHashMap};

use crate::analysis::UseLocation;
use crate::{BlockEnd, Lowered, VariableId};

/// Tracks use sites for each variable, indexed by variable arena index.
pub struct UseSites {
    sites: Vec<OrderedHashMap<UseLocation, usize>>,
}

impl UseSites {
    /// Builds use-site information by scanning all statements and block ends.
    pub fn analyze(lowered: &Lowered<'_>) -> Self {
        let mut sites: Vec<OrderedHashMap<UseLocation, usize>> =
            (0..lowered.variables.len()).map(|_| OrderedHashMap::default()).collect();
        for (block_id, block) in lowered.blocks.iter() {
            for (stmt_idx, stmt) in block.statements.iter().enumerate() {
                let loc = UseLocation::Statement((block_id, stmt_idx));
                for input in stmt.inputs() {
                    *sites[input.var_id.index()].entry(loc).or_default() += 1;
                }
            }
            let end_loc = UseLocation::BlockEnd(block_id);
            match &block.end {
                BlockEnd::Return(returns, _) => {
                    for ret in returns {
                        *sites[ret.var_id.index()].entry(end_loc).or_default() += 1;
                    }
                }
                BlockEnd::Panic(var) => {
                    *sites[var.var_id.index()].entry(end_loc).or_default() += 1;
                }
                BlockEnd::Goto(_, remapping) => {
                    for (_, src) in remapping.iter() {
                        *sites[src.var_id.index()].entry(end_loc).or_default() += 1;
                    }
                }
                BlockEnd::Match { info } => {
                    for input in info.inputs() {
                        *sites[input.var_id.index()].entry(end_loc).or_default() += 1;
                    }
                }
                BlockEnd::NotSet => {}
            }
        }
        Self { sites }
    }

    /// Returns how many distinct use sites remain for `var`.
    pub fn use_count(&self, var: VariableId) -> usize {
        self.sites[var.index()].iter().map(|(_, count)| *count).sum()
    }

    /// Removes the use of `var` at `loc`. Idempotent.
    pub fn remove_use(&mut self, var: VariableId, loc: UseLocation) {
        match self.sites[var.index()].entry(loc) {
            Entry::Occupied(mut entry) => {
                *entry.get_mut() -= 1;
                if *entry.get() == 0 {
                    entry.swap_remove();
                }
            }
            Entry::Vacant(_) => {}
        }
    }

    /// Adds a use of `var` at `loc`. Idempotent.
    pub fn add_use(&mut self, var: VariableId, loc: UseLocation) {
        *self.sites[var.index()].entry(loc).or_default() += 1;
    }

    /// Returns the use-site locations for `var`.
    pub fn use_locs(
        &self,
        var: VariableId,
    ) -> impl ExactSizeIterator<Item = (UseLocation, usize)> + '_ {
        self.sites[var.index()].iter().map(|(loc, count)| (*loc, *count))
    }
}

impl std::fmt::Debug for UseSites {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();
        for (idx, sites) in self.sites.iter().enumerate() {
            let locs: Vec<_> = sites.iter().collect();
            list.entry(&format_args!("v{idx}: {locs:?}"));
        }
        list.finish()
    }
}
