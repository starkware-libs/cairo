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
//! 3. **Variable tracking** (optional): `apply_remapping` - override if tracking per-variable state
//! 4. **Advanced hooks** (optional): `merge_match`, `split_match`, etc. - override for special
//!    cases
//!
//! The framework handles control flow mechanics automatically:
//! - Goto with remapping: calls `apply_remapping`
//! - Match (backward): calls `merge_match` which defaults to `merge`
//! - Match (forward): calls `split_match` which defaults to cloning

use crate::{Block, BlockEnd, BlockId, MatchInfo, Statement, VarRemapping};

/// Location of a lowering statement inside a block.
pub type StatementLocation = (BlockId, usize);

/// The direction of dataflow analysis.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[expect(dead_code)]
pub enum Direction {
    Forward,
    Backward,
}

/// Unified analyzer trait for dataflow analysis.
///
/// Implementors specify the direction via `DIRECTION` and implement the core methods.
/// The framework handles control flow mechanics (remapping, match arms) automatically,
/// with sensible defaults that can be overridden for complex analyses.
///
/// # Transfer Granularity
///
/// You can work at either block or statement level:
/// - **Block-level**: Override `transfer_block` for coarse-grained analysis
/// - **Statement-level**: Override `transfer_stmt`; default `transfer_block` iterates statements
///
/// # Lifetime parameters
/// - `'db`: The database lifetime (for interned types).
/// - `'a`: The lifetime of borrowed lowering data.
///
/// # Example (statement-level forward analysis)
/// ```ignore
/// impl<'db, 'a> DataflowAnalyzer<'db, 'a> for MyAnalyzer {
///     type Info = HashSet<VariableId>;
///     const DIRECTION: Direction = Direction::Forward;
///
///     fn initial_info(&mut self) -> Self::Info { HashSet::new() }
///     fn transfer_stmt(&mut self, info: &mut Self::Info, ..) { /* update info */ }
///     fn merge(&mut self, infos: impl Iterator<Item = Self::Info>) -> Self::Info {
///         infos.fold(HashSet::new(), |mut acc, i| { acc.extend(i); acc })
///     }
/// }
/// ```
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
///     fn merge(&mut self, infos: impl Iterator<Item = Self::Info>) -> Self::Info {
///         infos.max().unwrap_or(0)
///     }
/// }
/// ```
#[expect(dead_code)]
pub trait DataflowAnalyzer<'db, 'a> {
    /// The analysis state/info type.
    type Info: Clone;

    /// The direction of this analysis.
    const DIRECTION: Direction;

    // ========================================================================
    // Core methods (must implement)
    // ========================================================================

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
    /// - `statement_location`: where the merge occurs in the CFG
    /// - `infos`: iterator of (source_block_id, info) pairs from each incoming path
    fn merge(
        &mut self,
        statement_location: StatementLocation,
        infos: impl Iterator<Item = (BlockId, Self::Info)>,
    ) -> Self::Info;

    // ========================================================================
    // Transfer functions (choose granularity)
    // ========================================================================

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

    // ========================================================================
    // Variable tracking (optional - override if tracking per-variable state)
    // ========================================================================

    /// Apply variable remapping to the state.
    /// Called by the framework when crossing a goto with remapping.
    ///
    /// - Backward: translates demands on destination vars to demands on source vars
    /// - Forward: translates state of source vars to state of destination vars
    ///
    /// Default is no-op (fine for analyses that don't track per-variable state).
    fn apply_remapping(
        &mut self,
        _info: &mut Self::Info,
        _statement_location: StatementLocation,
        _target_block_id: BlockId,
        _remapping: &'a VarRemapping<'db>,
    ) {
    }

    // ========================================================================
    // Block boundary hooks (optional)
    // ========================================================================

    /// Called when entering a block during traversal (before transfer_block).
    fn visit_block_start(
        &mut self,
        _info: &mut Self::Info,
        _block_id: BlockId,
        _block: &Block<'db>,
    ) {
    }

    // ========================================================================
    // Match handling (optional - have sensible defaults)
    // ========================================================================

    /// Backward: merge states from match arms.
    /// Default implementation calls `merge` with arm block IDs.
    /// Override for special handling (e.g., tracking arm-specific metadata).
    fn merge_match(
        &mut self,
        statement_location: StatementLocation,
        match_info: &'a MatchInfo<'db>,
        arm_infos: impl Iterator<Item = Self::Info>,
    ) -> Self::Info {
        let infos_with_blocks = match_info.arms().iter().map(|arm| arm.block_id).zip(arm_infos);
        self.merge(statement_location, infos_with_blocks)
    }

    /// Forward: split state for match arms.
    /// Default implementation clones state for each arm.
    /// Override to refine state based on match conditions.
    fn split_match(
        &mut self,
        info: &Self::Info,
        _statement_location: StatementLocation,
        match_info: &'a MatchInfo<'db>,
    ) -> Vec<Self::Info> {
        match_info.arms().iter().map(|_| info.clone()).collect()
    }
}
