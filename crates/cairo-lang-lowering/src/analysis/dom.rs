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
    /// dom_sets for a block_id = set of blocks that dominate block_id.
    dom_sets: Vec<Option<OrderedHashSet<BlockId>>>,
}

impl Dominators {
    /// Runs dominator analysis on a lowered function.
    pub fn analyze(lowered: &Lowered<'_>) -> Self {
        let analyzer = DominatorAnalyzer;
        let mut fwd = ForwardDataflowAnalysis::new(lowered, analyzer);
        let dom_sets = fwd.run();
        Dominators { dom_sets }
    }

    /// Returns true if block `a` dominates block `b`.
    pub fn dominates(&self, a: BlockId, b: BlockId) -> bool {
        self.dom_sets[b.0].as_ref().is_some_and(|set| set.contains(&a))
    }

    /// Returns the set of dominators for the given block, if reachable.
    pub fn dominators_of(&self, block: BlockId) -> Option<&OrderedHashSet<BlockId>> {
        self.dom_sets[block.0].as_ref()
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
        info1.iter().filter(|&id| info2.contains(id)).cloned().collect()
    }

    fn transfer_block(&mut self, info: &mut Self::Info, block_id: BlockId, _block: &'a Block<'db>) {
        // Every block dominates itself.
        info.insert(block_id);
    }
}
