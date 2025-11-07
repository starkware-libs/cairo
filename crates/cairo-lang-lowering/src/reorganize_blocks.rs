use std::collections::HashMap;

use itertools::Itertools;

use crate::blocks::BlocksBuilder;
use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::optimizations::remappings::{self, Context};
use crate::utils::{Rebuilder, RebuilderEx};
use crate::{
    Block, BlockEnd, BlockId, Lowered, MatchInfo, Statement, VarRemapping, VarUsage, VariableArena,
    VariableId,
};

/// Reorganizes the blocks in lowered function and removes unnecessary remappings.
///
/// Removes unreachable blocks.
/// Blocks that are reachable only through goto are combined with the block that does the goto.
/// The order of the blocks is changed to be a topologically sorted.
pub fn reorganize_blocks<'db>(lowered: &mut Lowered<'db>) {
    if lowered.blocks.is_empty() {
        return;
    }
    let mut ctx = TopSortContext {
        old_block_rev_order: Vec::with_capacity(lowered.blocks.len()),
        incoming_gotos: vec![0; lowered.blocks.len()],
        can_be_merged: vec![true; lowered.blocks.len()],
        remappings_ctx: Context::new(lowered.variables.len()),
    };

    remappings::visit_remappings(lowered, |remapping| {
        for (dst, src) in remapping.iter() {
            ctx.remappings_ctx.dest_to_srcs[dst.index()].push(src.var_id);
        }
    });

    let mut analysis = BackAnalysis::new(lowered, ctx);
    analysis.get_root_info();
    let ctx = analysis.analyzer;

    // Rebuild the blocks in the correct order.
    let mut new_blocks = BlocksBuilder::default();

    // Keep only blocks that can't be merged or have more than 1 incoming
    // goto.
    // Note that unreachable blocks were not added to `ctx.old_block_rev_order` during
    // the analysis above.
    let mut old_block_rev_order = ctx
        .old_block_rev_order
        .into_iter()
        .filter(|block_id| !ctx.can_be_merged[block_id.0] || ctx.incoming_gotos[block_id.0] > 1)
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
        remappings_ctx: ctx.remappings_ctx,
    };

    let mut var_reassigner = VarReassigner::new(&lowered.variables);
    for param in lowered.parameters.iter_mut() {
        *param = var_reassigner.map_var_id(*param);
    }

    for block_id in old_block_rev_order.into_iter().rev() {
        let mut statements = vec![];

        let mut block = &lowered.blocks[block_id];
        loop {
            statements.extend(
                block.statements.iter().map(|stmt| {
                    var_reassigner.rebuild_statement(&rebuilder.rebuild_statement(stmt))
                }),
            );
            if let BlockEnd::Goto(target_block_id, remappings) = &block.end
                && !rebuilder.block_remapping.contains_key(target_block_id)
            {
                assert!(
                    rebuilder.rebuild_remapping(remappings).is_empty(),
                    "Remapping should be empty."
                );
                block = &lowered.blocks[*target_block_id];
                continue;
            }
            break;
        }

        let end = var_reassigner.rebuild_end(&rebuilder.rebuild_end(&block.end));
        new_blocks.alloc(Block { statements, end });
    }

    lowered.variables = var_reassigner.new_vars;
    lowered.blocks = new_blocks.build().unwrap();
}

pub struct TopSortContext {
    old_block_rev_order: Vec<BlockId>,
    // The number of incoming gotos, indexed by block_id.
    incoming_gotos: Vec<usize>,

    // True if the block can be merged with the block that goes to it.
    can_be_merged: Vec<bool>,

    remappings_ctx: remappings::Context,
}

impl<'db> Analyzer<'db, '_> for TopSortContext {
    type Info = ();

    fn visit_block_start(
        &mut self,
        _info: &mut Self::Info,
        block_id: BlockId,
        _block: &Block<'db>,
    ) {
        self.old_block_rev_order.push(block_id);
    }

    fn visit_stmt(
        &mut self,
        _info: &mut Self::Info,
        _statement_location: StatementLocation,
        stmt: &Statement<'db>,
    ) {
        for var_usage in stmt.inputs() {
            self.remappings_ctx.set_used(var_usage.var_id);
        }
    }

    fn visit_goto(
        &mut self,
        _info: &mut Self::Info,
        _statement_location: StatementLocation,
        target_block_id: BlockId,
        // Note that the remappings of a goto are not considered a usage, Later usages (such as a
        // merge) would catch them if used.
        _remapping: &VarRemapping<'db>,
    ) {
        self.incoming_gotos[target_block_id.0] += 1;
    }

    fn merge_match(
        &mut self,
        _statement_location: StatementLocation,
        match_info: &MatchInfo<'db>,
        _infos: impl Iterator<Item = Self::Info>,
    ) -> Self::Info {
        for var_usage in match_info.inputs() {
            self.remappings_ctx.set_used(var_usage.var_id);
        }

        for arm in match_info.arms().iter() {
            self.can_be_merged[arm.block_id.0] = false;
        }
    }

    fn info_from_return(
        &mut self,
        _statement_location: StatementLocation,
        vars: &[VarUsage<'db>],
    ) -> Self::Info {
        for var_usage in vars {
            self.remappings_ctx.set_used(var_usage.var_id);
        }
    }

    fn info_from_panic(
        &mut self,
        _statement_location: StatementLocation,
        data: &VarUsage<'db>,
    ) -> Self::Info {
        self.remappings_ctx.set_used(data.var_id);
    }
}

pub struct RebuildContext {
    block_remapping: HashMap<BlockId, BlockId>,
    remappings_ctx: remappings::Context,
}
impl<'db> Rebuilder<'db> for RebuildContext {
    fn map_block_id(&mut self, block: BlockId) -> BlockId {
        self.block_remapping[&block]
    }

    fn map_var_id(&mut self, var: VariableId) -> VariableId {
        self.remappings_ctx.map_var_id(var)
    }

    fn transform_remapping(&mut self, remapping: &mut VarRemapping<'db>) {
        self.remappings_ctx.transform_remapping(remapping)
    }
}

/// Helper class to reassign variable ids according to the rebuild order.
///
/// Note that it can't be integrated into the RebuildContext above because rebuild_remapping might
/// call `map_var_id` on variables that are going to be removed.
pub struct VarReassigner<'db, 'a> {
    pub old_vars: &'a VariableArena<'db>,
    pub new_vars: VariableArena<'db>,

    // Maps old var_id to new_var_id
    pub vars: Vec<Option<VariableId>>,
}

impl<'db, 'a> VarReassigner<'db, 'a> {
    pub fn new(old_vars: &'a VariableArena<'db>) -> Self {
        Self { old_vars, new_vars: Default::default(), vars: vec![None; old_vars.len()] }
    }
}

impl<'db> Rebuilder<'db> for VarReassigner<'db, '_> {
    fn map_var_id(&mut self, var: VariableId) -> VariableId {
        *self.vars[var.index()]
            .get_or_insert_with(|| self.new_vars.alloc(self.old_vars[var].clone()))
    }
}
