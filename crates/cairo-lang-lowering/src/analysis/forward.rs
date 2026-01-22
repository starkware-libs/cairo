//! Forward dataflow analysis runner.
//!
//! This module provides `FwdAnalysis`, which traverses the control flow graph in forward
//! (topological) order, computing dataflow information from function entry to exits.
use crate::analysis::core::{DataflowAnalyzer, Direction, Edge};
use crate::{BlockEnd, BlockId, Lowered};

/// Forward analysis runner.
///
/// Traverses the CFG in topological order (from entry towards exits), processing
/// statements in forward order within each block.
///
/// The runner automatically handles:
/// - Block/statement traversal via `transfer_block`
/// - Variable remapping at gotos (via `transfer_edge`)
/// - State distribution to match arms (via `transfer_edge`)
/// - State joining at convergence points (via `merge`)
pub struct ForwardDataflowAnalysis<'db, 'a, TAnalyzer: DataflowAnalyzer<'db, 'a>> {
    lowered: &'a Lowered<'db>,
    pub analyzer: TAnalyzer,
    /// Number of predecessors for each block (pre-computed).
    predecessor_counts: Vec<usize>,
    /// Incoming edges: (source_block_id, info). Cleared when block is processed.
    incoming: Vec<Option<TAnalyzer::Info>>,
}

impl<'db, 'a, TAnalyzer: DataflowAnalyzer<'db, 'a>> ForwardDataflowAnalysis<'db, 'a, TAnalyzer> {
    /// Creates a new FwdAnalysis instance.
    pub fn new(lowered: &'a Lowered<'db>, analyzer: TAnalyzer) -> Self {
        debug_assert!(
            TAnalyzer::DIRECTION == Direction::Forward,
            "FwdAnalysis requires an analyzer with DIRECTION == Forward"
        );
        let predecessor_counts = compute_predecessor_counts(lowered);
        let incoming = vec![None; lowered.blocks.len()];
        Self { lowered, analyzer, predecessor_counts, incoming }
    }

    /// Runs the forward analysis and returns the exit info for each block.
    ///
    /// For acyclic CFGs, this processes blocks in topological order.
    /// Returns the exit info Vec indexed by BlockId.
    pub fn run(&mut self) -> Vec<Option<TAnalyzer::Info>> {
        let n_blocks = self.lowered.blocks.len();
        let mut block_info: Vec<Option<TAnalyzer::Info>> = vec![None; n_blocks];

        // Root block has 0 predecessors, so it's immediately ready.
        let root_id = BlockId::root();
        self.incoming[root_id.0] =
            Some(self.analyzer.initial_info(root_id, &self.lowered.blocks[root_id].end));
        let mut ready: Vec<BlockId> = vec![root_id];

        while let Some(block_id) = ready.pop() {
            let block = &self.lowered.blocks[block_id];

            // Get entry info from incoming edges.
            let mut info = self.incoming[block_id.0].clone().unwrap();

            // Process block.
            self.analyzer.visit_block_start(&mut info, block_id, block);
            self.analyzer.transfer_block(&mut info, block_id, block);

            // Transfer to successors and check readiness.
            self.propagate_to_successors(block_id, &info, &mut ready);

            block_info[block_id.0] = Some(info);
        }

        block_info
    }

    /// Propagate info to all successors and mark them ready if all predecessors are done.
    fn propagate_to_successors(
        &mut self,
        block_id: BlockId,
        info: &TAnalyzer::Info,
        ready: &mut Vec<BlockId>,
    ) {
        let block = &self.lowered.blocks[block_id];
        match &block.end {
            BlockEnd::Goto(target, remapping) => {
                let edge = Edge::Goto { target: *target, remapping };
                let target_info = self.analyzer.transfer_edge(info, &edge);
                self.add_and_maybe_ready(*target, target_info, ready);
            }
            BlockEnd::Match { info: match_info } => {
                for arm in match_info.arms() {
                    let edge = Edge::MatchArm { arm, match_info };
                    let arm_info = self.analyzer.transfer_edge(info, &edge);
                    self.add_and_maybe_ready(arm.block_id, arm_info, ready);
                }
            }
            BlockEnd::Return(..) | BlockEnd::Panic(_) => {
                // Terminal blocks, no successors.
            }
            BlockEnd::NotSet => unreachable!("Block end not set"),
        }
    }

    /// Add incoming info and mark target ready if all predecessors have contributed.
    ///
    /// When multiple predecessors contribute to a block, their info is merged.
    fn add_and_maybe_ready(
        &mut self,
        target: BlockId,
        info: TAnalyzer::Info,
        ready: &mut Vec<BlockId>,
    ) {
        let merged_info = match self.incoming[target.0].take() {
            Some(existing) => self.analyzer.merge(self.lowered, (target, 0), existing, info),
            None => info,
        };
        self.incoming[target.0] = Some(merged_info);
        self.predecessor_counts[target.0] -= 1;
        if self.predecessor_counts[target.0] == 0 {
            ready.push(target);
        }
    }
}

/// Computes the number of predecessors for each block.
fn compute_predecessor_counts(lowered: &Lowered<'_>) -> Vec<usize> {
    let n_blocks = lowered.blocks.len();
    let mut counts = vec![0usize; n_blocks];

    for (_, block) in lowered.blocks.iter() {
        match &block.end {
            BlockEnd::Goto(target, _) => {
                counts[target.0] += 1;
            }
            BlockEnd::Match { info } => {
                for arm in info.arms() {
                    counts[arm.block_id.0] += 1;
                }
            }
            BlockEnd::Return(..) | BlockEnd::Panic(_) | BlockEnd::NotSet => {}
        }
    }

    counts
}
