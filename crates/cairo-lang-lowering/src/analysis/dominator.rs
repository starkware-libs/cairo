//! Block dominator analysis for lowered IR.
//!
//! Computes the set of blocks that dominate each block using the forward
//! dataflow framework. A block A dominates block B if every path from the
//! entry block to B passes through A.

use cairo_lang_utils::ordered_hash_set::OrderedHashSet;

use crate::analysis::core::{DataflowAnalyzer, Direction, StatementLocation};
use crate::analysis::forward::ForwardDataflowAnalysis;
use crate::{Block, BlockEnd, BlockId, Lowered};

/// Result of dominator analysis, providing efficient dominator queries.
pub struct Dominators {
    /// Set of blocks that dominate `block_id` indexed by block ID.
    per_block: Vec<OrderedHashSet<BlockId>>,
}

impl Dominators {
    /// Runs dominator analysis on a lowered function.
    pub fn analyze(lowered: &Lowered<'_>) -> Self {
        let mut fwd = ForwardDataflowAnalysis::new(lowered, DominatorAnalyzer);
        let per_block = fwd.run().into_iter().map(|opt| opt.unwrap_or_default()).collect();
        Dominators { per_block }
    }

    /// Returns true if block `a` dominates block `b`.
    pub fn dominates(&self, a: BlockId, b: BlockId) -> bool {
        self.per_block[b.0].contains(&a)
    }

    /// Returns the set of dominators for the given block.
    pub fn dominators_of(&self, block: BlockId) -> &OrderedHashSet<BlockId> {
        &self.per_block[block.0]
    }
}

/// Internal forward analyzer that computes dominator sets.
struct DominatorAnalyzer;

impl<'db, 'a> DataflowAnalyzer<'db, 'a> for DominatorAnalyzer {
    type Info = OrderedHashSet<BlockId>;
    const DIRECTION: Direction = Direction::Forward;

    fn initial_info(&mut self, _block_id: BlockId, _block_end: &'a BlockEnd<'db>) -> Self::Info {
        OrderedHashSet::default()
    }

    fn merge(
        &mut self,
        _lowered: &Lowered<'db>,
        _statement_location: StatementLocation,
        info1: Self::Info,
        info2: Self::Info,
    ) -> Self::Info {
        // Dominator merge is set intersection:
        // a block dominates B only if it dominates ALL predecessors of B.
        let mut info1 = info1;
        info1.retain(|id| info2.contains(id));
        info1
    }

    fn transfer_block(&mut self, info: &mut Self::Info, block_id: BlockId, _block: &'a Block<'db>) {
        // Every block dominates itself.
        info.insert(block_id);
    }
}

#[cfg(test)]
#[path = "dominator_test.rs"]
mod dominator_test;
