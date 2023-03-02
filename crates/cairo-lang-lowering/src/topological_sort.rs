use std::collections::HashMap;

use crate::blocks::FlatBlocks;
use crate::borrow_check::analysis::{Analyzer, BackAnalysis};
use crate::utils::{Rebuilder, RebuilderEx};
use crate::{BlockId, FlatLowered};

/// Order the blocks in a lowered function topologically.
pub fn topological_sort(lowered: &mut FlatLowered) {
    if !lowered.blocks.is_empty() {
        let ctx = TopSortContext {
            n_blocks: lowered.blocks.len(),
            block_remapping: Default::default(),
            old_block_rev_order: Default::default(),
        };
        let mut analysis =
            BackAnalysis { lowered: &*lowered, cache: Default::default(), analyzer: ctx };
        analysis.get_root_info();
        let mut ctx = analysis.analyzer;

        // Rebuild the blocks in the correct order.
        let mut new_blocks = FlatBlocks::default();
        let old_block_rev_order = std::mem::take(&mut ctx.old_block_rev_order);
        for block_id in old_block_rev_order.into_iter().rev() {
            new_blocks.alloc(ctx.rebuild_block(&lowered.blocks[block_id]));
        }

        lowered.blocks = new_blocks;
    }
}

pub struct TopSortContext {
    n_blocks: usize,
    block_remapping: HashMap<BlockId, BlockId>,
    old_block_rev_order: Vec<BlockId>,
}

impl Analyzer for TopSortContext {
    type Info = ();

    fn visit_block_start(
        &mut self,
        _info: &mut Self::Info,
        block_id: crate::BlockId,
        _block: &crate::FlatBlock,
    ) {
        self.block_remapping
            .insert(block_id, BlockId(self.n_blocks - self.block_remapping.len() - 1));
        self.old_block_rev_order.push(block_id);
    }

    fn merge_match(
        &mut self,
        _statement_location: crate::borrow_check::analysis::StatementLocation,
        _match_info: &crate::MatchInfo,
        _arms: &[(crate::BlockId, Self::Info)],
    ) -> Self::Info {
    }

    fn info_from_return(
        &mut self,
        _statement_location: crate::borrow_check::analysis::StatementLocation,
        _vars: &[crate::VariableId],
    ) -> Self::Info {
    }
}

impl Rebuilder for TopSortContext {
    fn map_var_id(&mut self, var: crate::VariableId) -> crate::VariableId {
        var
    }

    fn map_block_id(&mut self, block: BlockId) -> BlockId {
        self.block_remapping[&block]
    }
}
