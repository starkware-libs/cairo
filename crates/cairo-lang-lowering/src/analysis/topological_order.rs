use std::fmt;

use crate::analysis::core::{DataflowAnalyzer, Direction, StatementLocation};
use crate::analysis::forward::ForwardDataflowAnalysis;
use crate::{Block, BlockEnd, BlockId, Lowered};

/// Block visitation order produced by forward dataflow traversal.
///
/// Blocks appear in topological order (from entry towards exits), following
/// actual CFG edges rather than block-index ordering.
pub struct TopologicalOrder {
    blocks: Vec<BlockId>,
}

impl TopologicalOrder {
    /// Runs topological order analysis on a lowered function.
    pub fn analyze(lowered: &Lowered<'_>) -> Self {
        let mut fwd = ForwardDataflowAnalysis::new(lowered, TopologicalOrderAnalyzer::default());
        fwd.run();
        Self { blocks: fwd.analyzer.blocks }
    }

    /// Returns the blocks in topological visitation order.
    pub fn blocks(&self) -> &[BlockId] {
        &self.blocks
    }
}

impl fmt::Debug for TopologicalOrder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.blocks.iter()).finish()
    }
}

/// Forward dataflow pass that collects blocks in topological order.
#[derive(Default, Clone)]
struct TopologicalOrderAnalyzer {
    blocks: Vec<BlockId>,
}

impl<'db, 'a> DataflowAnalyzer<'db, 'a> for TopologicalOrderAnalyzer {
    type Info = ();
    const DIRECTION: Direction = Direction::Forward;

    fn initial_info(&mut self, _block_id: BlockId, _block_end: &'a BlockEnd<'db>) -> Self::Info {}

    fn merge(
        &mut self,
        _lowered: &Lowered<'db>,
        _statement_location: StatementLocation,
        _info1: Self::Info,
        _info2: Self::Info,
    ) -> Self::Info {
    }

    fn transfer_block(
        &mut self,
        _info: &mut Self::Info,
        block_id: BlockId,
        _block: &'a Block<'db>,
    ) {
        self.blocks.push(block_id);
    }
}
