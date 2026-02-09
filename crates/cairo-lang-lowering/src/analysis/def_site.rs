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
            match def {
                DefLocation::Statement((block, stmt_idx)) => {
                    write!(f, "v{idx}: stmt(blk{}, {stmt_idx})", block.0)?;
                }
                DefLocation::BlockEntry(block) => {
                    write!(f, "v{idx}: entry(blk{})", block.0)?;
                }
            }
        }
        Ok(())
    }
}

/// Forward analysis that records where each variable first becomes available.
pub struct DefSiteAnalysis<'db, 'a> {
    lowered: &'a Lowered<'db>,
    /// First-available location for each variable, indexed by variable arena index.
    def_sites: Vec<StatementLocation>,
}

impl DefSiteAnalysis<'_, '_> {
    /// Runs def-site analysis on a lowered function.
    ///
    /// Returns a vector indexed by variable arena index, mapping each variable
    /// to the `StatementLocation` where it first becomes available.
    /// - `(block, 0)`: available at block entry (parameters, goto remappings, match arm bindings).
    /// - `(block, i+1)`: available after statement `i`.
    pub fn analyze(lowered: &Lowered<'_>) -> DefSites {
        let analyzer = DefSiteAnalysis {
            lowered,
            def_sites: vec![(BlockId(lowered.blocks.len()), 0); lowered.variables.len()],
        };
        let mut fwd = ForwardDataflowAnalysis::new(lowered, analyzer);
        fwd.run();
        DefSites(
            fwd.analyzer
                .def_sites
                .into_iter()
                .map(|(block, raw)| {
                    if raw == 0 {
                        DefLocation::BlockEntry(block)
                    } else {
                        DefLocation::Statement((block, raw - 1))
                    }
                })
                .collect(),
        )
    }
}

impl<'db, 'a> DataflowAnalyzer<'db, 'a> for DefSiteAnalysis<'db, 'a> {
    type Info = ();
    const DIRECTION: Direction = Direction::Forward;

    fn initial_info(&mut self, _block_id: BlockId, _block_end: &'a BlockEnd<'db>) -> Self::Info {
        for &param in &self.lowered.parameters {
            self.def_sites[param.index()] = (BlockId::root(), 0);
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
        (block_id, stmt_idx): StatementLocation,
        stmt: &'a Statement<'db>,
    ) {
        for &output in stmt.outputs() {
            self.def_sites[output.index()] = (block_id, stmt_idx + 1);
        }
    }

    fn transfer_edge(&mut self, _info: &Self::Info, edge: &Edge<'db, 'a>) -> Self::Info {
        match edge {
            Edge::Goto { target, remapping } => {
                for (&dst, _) in remapping.iter() {
                    self.def_sites[dst.index()] = (*target, 0);
                }
            }
            Edge::MatchArm { arm, .. } => {
                for &var in &arm.var_ids {
                    self.def_sites[var.index()] = (arm.block_id, 0);
                }
            }
            Edge::Return { .. } | Edge::Panic { .. } => {}
        }
    }
}
