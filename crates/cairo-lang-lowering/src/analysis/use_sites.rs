//! Mutable use-site tracking for lowered IR variables.
//!
//! Tracks where each variable is used (statement inputs and block-end inputs).
//! Updated incrementally as forwarding runs, so later checks see current use counts.
//!
//! Each `(variable, UseLocation)` pair stores a multiplicity: the number of input
//! slots consuming the variable at that location (e.g. `felt252_add(v, v)` records `v`
//! at that location with count 2). `use_count` sums these, so it is the total number
//! of consuming slots. A rename rewrites every slot at a location at once, so moving a
//! variable's uses to another (see `move_uses`) transfers the whole per-location count.

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

    /// Returns the total number of consuming slots remaining for `var` (the summed
    /// multiplicity across all of its use-site locations).
    pub fn use_count(&self, var: VariableId) -> usize {
        self.sites[var.index()].iter().map(|(_, count)| *count).sum()
    }

    /// Moves all uses of `from` at `loc` to `to`, mirroring a rename that rewrites
    /// every consuming slot at that location in one go: the whole per-location count is
    /// transferred and added to any uses `to` already has there. No-op if `from` has no
    /// use at `loc`.
    pub fn move_uses(&mut self, from: VariableId, to: VariableId, loc: UseLocation) {
        let Entry::Occupied(entry) = self.sites[from.index()].entry(loc) else {
            return;
        };
        let count = entry.swap_remove();
        *self.sites[to.index()].entry(loc).or_default() += count;
    }

    /// Returns each use-site location for `var` together with its multiplicity (the
    /// number of consuming slots at that location).
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
