//! Core dataflow analysis traits.
//!
//! This module provides a unified trait for both forward and backward dataflow analysis.
//!
//! # Design
//!
//! The `DataflowAnalyzer` trait provides a layered API:
//!
//! 1. **Core methods** (must implement): `initial_info`, `merge`
//! 2. **Transfer granularity** (choose one):
//!    - Block-level: override `transfer_block` for coarse-grained analysis
//!    - Statement-level: override `transfer_stmt` (default `transfer_block` iterates statements)
//! 3. **Variable tracking** (optional): `transfer_edge` - override if tracking per-variable state
//!
//! The Runner's (backward/forward/etc) should handle control flow mechanics automatically:
//! - Goto with remapping/Match split: calls `transfer_edge`
//! - Call transfer block for each block in need of processing

use crate::ids::LocationId;
use crate::{Block, BlockEnd, BlockId, MatchArm, MatchInfo, Statement, VarRemapping, VarUsage};

/// Location of a lowering statement inside a block.
pub type StatementLocation = (BlockId, usize);

/// The direction of dataflow analysis.
#[expect(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Forward,
    Backward,
}

/// Represents an edge in the control flow graph.
///
/// Each variant captures the specific information needed for that edge type,
/// enabling analyzers to handle variable introductions and remappings.
#[derive(Debug)]
#[expect(dead_code)]
pub enum Edge<'db, 'a> {
    /// A goto edge with variable remapping.
    Goto { target: BlockId, remapping: &'a VarRemapping<'db> },
    /// A match arm edge with the arm's introduced variables.
    MatchArm { arm: &'a MatchArm<'db>, match_info: &'a MatchInfo<'db> },
    /// A return edge (terminal).
    Return { vars: &'a [VarUsage<'db>], location: &'a LocationId<'db> },
    /// A panic edge (terminal).
    Panic { var: &'a VarUsage<'db> },
}

/// Unified analyzer trait for dataflow analysis.
///
/// Implementors specify the direction via `DIRECTION` and implement the core methods.
/// The framework specifies the "behaviour" of the dataflow (essentially updates to lattice state
/// for lattice analysis). Running an analysis is done by a runner (backward/forward/etc) which will
/// handle control flow and dataflow mechanics.
///
/// # Transfer Granularity
///
/// You can work at either block or statement level:
/// - **Block-level**: Override `transfer_block` for coarse-grained analysis
/// - **Statement-level**: Override `transfer_stmt`; default `transfer_block` iterates statements
///
/// # Example (block-level analysis)
/// ```ignore
/// impl<'db, 'a> DataflowAnalyzer<'db, 'a> for BlockCounter {
///     type Info = usize;
///     const DIRECTION: Direction = Direction::Forward;
///
///     fn initial_info(&mut self) -> Self::Info { 0 }
///     fn transfer_block(&mut self, info: &mut Self::Info, block_id: BlockId, block: &Block<'db>) {
///         *info += 1;  // Just count blocks
///     }
///     fn merge(&mut self, _loc: StatementLocation, _end_loc: &LocationId, infos: ..) -> Self::Info {
///         infos.map(|(_, i)| i).max().unwrap_or(0)
///     }
/// }
/// ```
#[expect(dead_code)]
pub trait DataflowAnalyzer<'db, 'a> {
    /// The analysis state/info type.
    type Info: Clone;

    /// The direction of this analysis.
    const DIRECTION: Direction;

    /// Create the initial analysis state at a terminal block.
    ///
    /// - Backward: called at return/panic blocks (what we know at function exit)
    /// - Forward: called at function entry
    ///
    /// For backward analysis, `block_end` provides access to return variables or panic data.
    /// For forward analysis, this is typically called once at the root block.
    fn initial_info(&mut self, block_id: BlockId, block_end: &'a BlockEnd<'db>) -> Self::Info;

    /// Merge/join states from multiple control flow paths.
    /// Called at join points (match merge for backward, block entry for forward).
    ///
    /// - `statement_location`: where the merge occurs in the CFG.
    /// - `location`: source location of the merge.
    /// - `infos`: iterator of (source_block_id, info) pairs from each incoming path.
    fn merge(
        &mut self,
        statement_location: StatementLocation,
        location: &'a LocationId<'db>,
        infos: impl Iterator<Item = (BlockId, Self::Info)>,
    ) -> Self::Info;

    /// Transfer function for an entire block.
    /// - Backward: transforms post-block state to pre-block state
    /// - Forward: transforms pre-block state to post-block state
    ///
    /// Default implementation iterates statements and calls `transfer_stmt`.
    /// Override this for block-level analysis (ignoring individual statements).
    fn transfer_block(&mut self, info: &mut Self::Info, block_id: BlockId, block: &'a Block<'db>) {
        match Self::DIRECTION {
            Direction::Forward => {
                for (i, stmt) in block.statements.iter().enumerate() {
                    self.transfer_stmt(info, (block_id, i), stmt);
                }
            }
            Direction::Backward => {
                for (i, stmt) in block.statements.iter().enumerate().rev() {
                    self.transfer_stmt(info, (block_id, i), stmt);
                }
            }
        }
    }

    /// Transfer function for a single statement.
    /// - Backward: transforms post-state to pre-state
    /// - Forward: transforms pre-state to post-state
    ///
    /// Default is no-op. Override this for statement-level analysis.
    fn transfer_stmt(
        &mut self,
        _info: &mut Self::Info,
        _statement_location: StatementLocation,
        _stmt: &'a Statement<'db>,
    ) {
    }

    /// Transfer state along a CFG edge.
    /// Called when traversing between blocks via control flow edges.
    ///
    /// - `info`: the state to transfer
    /// - `edge`: the edge being traversed, containing all relevant information
    ///
    /// Default implementation clones the state.
    /// Override to modify state based on edge properties (e.g., variable remapping,
    /// introduced variables in match arms).
    fn transfer_edge(&mut self, info: &Self::Info, _edge: &Edge<'db, 'a>) -> Self::Info {
        info.clone()
    }

    /// Called when entering a block during traversal (before transfer_block).
    fn visit_block_start(
        &mut self,
        _info: &mut Self::Info,
        _block_id: BlockId,
        _block: &Block<'db>,
    ) {
    }
}
