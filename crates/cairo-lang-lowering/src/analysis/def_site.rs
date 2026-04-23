//! Def-site analysis for lowered IR.
//!
//! Records where each variable is first defined as a `DefLocation`:
//! either `BlockEntry` (parameters, goto remappings, match arm bindings)
//! or `Statement` (output of a statement). Uses the forward analysis framework
//! and all results are accumulated in the analyzer's global `def_sites` vector.

use std::fmt;

use crate::analysis::DefLocation;
use crate::analysis::core::{DataflowAnalyzer, Direction, Edge, StatementLocation};
use crate::analysis::forward::ForwardDataflowAnalysis;
use crate::{BlockEnd, BlockId, Lowered, Statement};

/// Result of def-site analysis: the def location for each variable, indexed by arena index.
///
/// Each entry is `Some(loc)` when the variable's defining block is reachable from the function
/// entry, and `None` when it is unreachable (and therefore never visited by the analysis).
pub struct DefSites(pub Vec<Option<DefLocation>>);

impl std::ops::Deref for DefSites {
    type Target = [Option<DefLocation>];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl fmt::Debug for DefSites {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut list = f.debug_list();
        for (idx, def) in self.0.iter().enumerate() {
            match def {
                Some(loc) => list.entry(&format_args!("v{idx}: {loc:?}")),
                None => list.entry(&format_args!("v{idx}: undefined")),
            };
        }
        list.finish()
    }
}

/// Forward analysis that records where each variable first becomes available.
pub struct DefSiteAnalysis<'db, 'a> {
    lowered: &'a Lowered<'db>,
    /// First-available location for each variable, indexed by variable arena index.
    /// Slots stay `None` until the traversal reaches them; slots that remain `None` correspond
    /// to variables in unreachable blocks.
    def_sites: Vec<Option<DefLocation>>,
}

impl DefSiteAnalysis<'_, '_> {
    /// Runs def-site analysis on a lowered function.
    ///
    /// Returns a vector indexed by variable arena index, mapping each variable to the
    /// `DefLocation` where it first becomes available:
    /// - `BlockEntry(block)`: available at block entry (parameters, goto remappings, match arm
    ///   bindings).
    /// - `Statement((block, i))`: available after statement `i` in `block`.
    /// - `None`: the variable's defining block is unreachable from the function entry.
    pub fn analyze(lowered: &Lowered<'_>) -> DefSites {
        let analyzer = DefSiteAnalysis { lowered, def_sites: vec![None; lowered.variables.len()] };
        let mut fwd = ForwardDataflowAnalysis::new(lowered, analyzer);
        fwd.run();
        DefSites(fwd.analyzer.def_sites)
    }
}

impl<'db, 'a> DataflowAnalyzer<'db, 'a> for DefSiteAnalysis<'db, 'a> {
    type Info = ();
    const DIRECTION: Direction = Direction::Forward;

    fn initial_info(&mut self, _block_id: BlockId, _block_end: &'a BlockEnd<'db>) -> Self::Info {
        for &param in &self.lowered.parameters {
            self.def_sites[param.index()] = Some(DefLocation::BlockEntry(BlockId::root()));
        }
    }

    fn merge(
        &mut self,
        _lowered: &Lowered<'db>,
        _statement_location: StatementLocation,
        _info1: Self::Info,
        _info2: Self::Info,
    ) -> Self::Info {
    }

    fn transfer_stmt(
        &mut self,
        _info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &'a Statement<'db>,
    ) {
        for &output in stmt.outputs() {
            self.def_sites[output.index()] = Some(DefLocation::Statement(statement_location));
        }
    }

    fn transfer_edge(&mut self, _info: &Self::Info, edge: &Edge<'db, 'a>) -> Self::Info {
        match edge {
            Edge::Goto { target, remapping } => {
                for (&dst, _) in remapping.iter() {
                    self.def_sites[dst.index()] = Some(DefLocation::BlockEntry(*target));
                }
            }
            Edge::MatchArm { arm, .. } => {
                for &var in &arm.var_ids {
                    self.def_sites[var.index()] = Some(DefLocation::BlockEntry(arm.block_id));
                }
            }
            Edge::Return { .. } | Edge::Panic { .. } => {}
        }
    }
}
