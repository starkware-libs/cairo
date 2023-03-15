use std::collections::HashMap;

use itertools::Itertools;

use crate::blocks::FlatBlocksBuilder;
use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::utils::{Rebuilder, RebuilderEx};
use crate::{BlockId, FlatBlock, FlatBlockEnd, FlatLowered, MatchInfo, VarRemapping, VariableId};

/// Reorganizes the blocks in lowered function.
///
/// Removes unreachable blocks.
/// Blocks that are reachable only through goto are combined with the block that does the goto.
/// The order of the blocks is changed to be a topologically sorted.
pub fn reorganize_blocks(lowered: &mut FlatLowered) {
    if !lowered.blocks.is_empty() {
        let ctx = TopSortContext {
            old_block_rev_order: Default::default(),
            incoming_gotos: vec![0; lowered.blocks.len()],
            incoming_match_arm: vec![false; lowered.blocks.len()],
        };
        let mut analysis =
            BackAnalysis { lowered: &*lowered, cache: Default::default(), analyzer: ctx };
        analysis.get_root_info();
        let ctx = analysis.analyzer;

        // Rebuild the blocks in the correct order.
        let mut new_blocks = FlatBlocksBuilder::default();

        // Keep only blocks that are a target of a match arm or have more than 1 incoming
        // goto.
        // Note that unreachable block were not added to `ctx.old_block_rev_order` during
        // the analysis above.
        let mut old_block_rev_order = ctx
            .old_block_rev_order
            .into_iter()
            .filter(|block_id| {
                ctx.incoming_match_arm[block_id.0] || ctx.incoming_gotos[block_id.0] > 1
            })
            .collect_vec();

        // Add the root block as it was filtered above.
        old_block_rev_order.push(BlockId::root());

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
            let mut statements = vec![];

            let mut block = &lowered.blocks[block_id];
            loop {
                for stmt in &block.statements {
                    statements.push(rebuilder.rebuild_statement(stmt));
                }
                if let FlatBlockEnd::Goto(target_block_id, remappings) = &block.end {
                    if rebuilder.block_remapping.get(target_block_id).is_none() {
                        assert!(remappings.is_empty(), "Remapping should be empty.");
                        block = &lowered.blocks[*target_block_id];
                        continue;
                    }
                }
                break;
            }

            let end = rebuilder.rebuild_end(&block.end);
            new_blocks.alloc(FlatBlock { statements, end });
        }

        lowered.blocks = new_blocks.build().unwrap();
    }
}

pub struct TopSortContext {
    old_block_rev_order: Vec<BlockId>,
    // The number of incoming gotos, indexed by block_id.
    incoming_gotos: Vec<usize>,

    // True if the block has a match arm that goes to it.
    incoming_match_arm: Vec<bool>,
}

impl Analyzer for TopSortContext {
    type Info = ();

    fn visit_block_start(&mut self, _info: &mut Self::Info, block_id: BlockId, _block: &FlatBlock) {
        self.old_block_rev_order.push(block_id);
    }

    fn visit_remapping(
        &mut self,
        _info: &mut Self::Info,
        _block_id: BlockId,
        target_block_id: BlockId,
        _remapping: &VarRemapping,
    ) {
        self.incoming_gotos[target_block_id.0] += 1;
    }

    fn merge_match(
        &mut self,
        _statement_location: StatementLocation,
        match_info: &MatchInfo,
        _infos: &[Self::Info],
    ) -> Self::Info {
        for arm in match_info.arms().iter() {
            self.incoming_match_arm[arm.block_id.0] = true;
        }
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
