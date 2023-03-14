use std::collections::HashMap;

use crate::blocks::FlatBlocksBuilder;
use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::utils::{Rebuilder, RebuilderEx};
use crate::{BlockId, FlatBlock, FlatLowered, MatchInfo, VariableId};

/// Order the blocks in a lowered function topologically.
pub fn topological_sort(lowered: &mut FlatLowered) {
    if !lowered.blocks.is_empty() {
        let ctx = TopSortContext { old_block_rev_order: Default::default() };
        let mut analysis =
            BackAnalysis { lowered: &*lowered, cache: Default::default(), analyzer: ctx };
        analysis.get_root_info();
        let mut ctx = analysis.analyzer;

        // Rebuild the blocks in the correct order.
        let mut new_blocks = FlatBlocksBuilder::default();
        let old_block_rev_order = std::mem::take(&mut ctx.old_block_rev_order);

        let n_visited_blocks = old_block_rev_order.len();
        let mut rebuilder = RebuildContext {
            block_remapping: HashMap::from_iter(
                old_block_rev_order
                    .iter()
                    .enumerate()
                    .map(|(idx, block_id)| (*block_id, BlockId(n_visited_blocks - idx - 1))),
            ),
        };
        for block_id in old_block_rev_order.into_iter().rev() {
            new_blocks.alloc(rebuilder.rebuild_block(&lowered.blocks[block_id]));
        }

        lowered.blocks = new_blocks.build().unwrap();
    }
}

pub struct TopSortContext {
    old_block_rev_order: Vec<BlockId>,
}

impl Analyzer for TopSortContext {
    type Info = ();

    fn visit_block_start(&mut self, _info: &mut Self::Info, block_id: BlockId, _block: &FlatBlock) {
        self.old_block_rev_order.push(block_id);
    }

    fn merge_match(
        &mut self,
        _statement_location: StatementLocation,
        _match_info: &MatchInfo,
        _infos: &[Self::Info],
    ) -> Self::Info {
    }

    fn info_from_return(
        &mut self,
        _statement_location: StatementLocation,
        _vars: &[VariableId],
    ) -> Self::Info {
    }

    fn info_from_panic(
        &mut self,
        _statement_location: StatementLocation,
        _data: &VariableId,
    ) -> Self::Info {
    }
}

pub struct RebuildContext {
    block_remapping: HashMap<BlockId, BlockId>,
}
impl Rebuilder for RebuildContext {
    fn map_var_id(&mut self, var: VariableId) -> VariableId {
        var
    }

    fn map_block_id(&mut self, block: BlockId) -> BlockId {
        self.block_remapping[&block]
    }
}
