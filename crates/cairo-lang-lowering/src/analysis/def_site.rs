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

/// Result of def-site analysis, wrapping per-variable access locations.
pub struct DefSites(pub Vec<DefLocation>);

impl std::ops::Deref for DefSites {
    type Target = Vec<DefLocation>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl fmt::Display for DefSites {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (idx, def) in self.0.iter().enumerate() {
            if idx > 0 {
                writeln!(f)?;
            }
            write!(f, "v{idx}: {def:?}")?;
        }
        Ok(())
    }
}

/// Forward analysis that records where each variable first becomes available.
pub struct DefSiteAnalysis<'db, 'a> {
    lowered: &'a Lowered<'db>,
    /// First-available location for each variable, indexed by variable arena index.
    /// Slots hold [`UNRESOLVED`] until the traversal reaches them; `analyze` asserts the
    /// placeholder is gone before returning.
    def_sites: Vec<DefLocation>,
}

/// Sentinel placeholder for `def_sites` slots before the traversal reaches them. Uses an
/// out-of-range `BlockId` so it cannot collide with any real def location.
const UNRESOLVED: DefLocation = DefLocation::Statement((BlockId(usize::MAX), usize::MAX));

impl DefSiteAnalysis<'_, '_> {
    /// Runs def-site analysis on a lowered function.
    ///
    /// Returns a vector indexed by variable arena index, mapping each variable to the
    /// `DefLocation` where it first becomes available:
    /// - `BlockEntry(block)`: available at block entry (parameters, goto remappings, match arm
    ///   bindings).
    /// - `Statement((block, i))`: available after statement `i` in `block`.
    pub fn analyze(lowered: &Lowered<'_>) -> DefSites {
        let analyzer =
            DefSiteAnalysis { lowered, def_sites: vec![UNRESOLVED; lowered.variables.len()] };
        let mut fwd = ForwardDataflowAnalysis::new(lowered, analyzer);
        fwd.run();
        assert!(
            fwd.analyzer.def_sites.iter().enumerate().all(|(_, d)| *d != UNRESOLVED),
            "DefSiteAnalysis left variables unresolved: {unresolved:?}"
        );
        DefSites(fwd.analyzer.def_sites)
    }
}

impl<'db, 'a> DataflowAnalyzer<'db, 'a> for DefSiteAnalysis<'db, 'a> {
    type Info = ();
    const DIRECTION: Direction = Direction::Forward;

    fn initial_info(&mut self, _block_id: BlockId, _block_end: &'a BlockEnd<'db>) -> Self::Info {
        for &param in &self.lowered.parameters {
            self.def_sites[param.index()] = DefLocation::BlockEntry(BlockId::root());
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
            self.def_sites[output.index()] = DefLocation::Statement(statement_location);
        }
    }

    fn transfer_edge(&mut self, _info: &Self::Info, edge: &Edge<'db, 'a>) -> Self::Info {
        match edge {
            Edge::Goto { target, remapping } => {
                for (&dst, _) in remapping.iter() {
                    self.def_sites[dst.index()] = DefLocation::BlockEntry(*target);
                }
            }
            Edge::MatchArm { arm, .. } => {
                for &var in &arm.var_ids {
                    self.def_sites[var.index()] = DefLocation::BlockEntry(arm.block_id);
                }
            }
            Edge::Return { .. } | Edge::Panic { .. } => {}
        }
    }
}
