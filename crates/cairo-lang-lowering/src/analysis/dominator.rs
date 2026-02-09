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
    /// Unreachable blocks default to `{root}`.
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
mod tests {
    use super::*;

    #[test]
    fn test_dominates() {
        // Diamond CFG dominator sets:
        //   blk0:        {blk0}
        //   blk1, blk2:  {blk0, self}     (arms)
        //   blk3:        {blk0, blk3}     (merge)
        let dominators = Dominators {
            per_block: vec![
                OrderedHashSet::from_iter([BlockId(0)]),
                OrderedHashSet::from_iter([BlockId(0), BlockId(1)]),
                OrderedHashSet::from_iter([BlockId(0), BlockId(2)]),
                OrderedHashSet::from_iter([BlockId(0), BlockId(3)]),
            ],
        };

        // Reflexivity: every block dominates itself.
        for i in 0..4 {
            assert!(dominators.dominates(BlockId(i), BlockId(i)));
        }

        // Root dominates every block.
        for i in 0..4 {
            assert!(dominators.dominates(BlockId(0), BlockId(i)));
        }

        // Diamond arms do not dominate each other.
        assert!(!dominators.dominates(BlockId(1), BlockId(2)));
        assert!(!dominators.dominates(BlockId(2), BlockId(1)));

        // Arms do not dominate the merge block.
        assert!(!dominators.dominates(BlockId(1), BlockId(3)));
        assert!(!dominators.dominates(BlockId(2), BlockId(3)));

        // Merge block does not dominate the arms.
        assert!(!dominators.dominates(BlockId(3), BlockId(1)));
        assert!(!dominators.dominates(BlockId(3), BlockId(2)));
    }
}
