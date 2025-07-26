//! This module is responsible for the lowering of a flow control graph [FlowControlGraph].

use cairo_lang_diagnostics::Maybe;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

use super::graph::{FlowControlGraph, NodeId};
use crate::ids::LocationId;
use crate::lower::block_builder::{
    BlockBuilder, SealedBlockBuilder, SealedGotoCallsite, merge_block_builders,
    merge_sealed_block_builders,
};
use crate::lower::context::{LoweredExpr, LoweringContext, LoweringFlowError, LoweringResult};
use crate::{BlockEnd, BlockId};

mod lower_node;

/// Lowers a flow control graph.
#[allow(dead_code)]
pub fn lower_graph(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    graph: &FlowControlGraph,
    location: LocationId,
) -> LoweringResult<LoweredExpr> {
    let mut context = LowerGraphContext::new(ctx, builder, graph, location);
    // Go over the nodes in reverse order to make sure parent nodes are handled before their
    // children.
    for id in (0..graph.nodes.len()).rev() {
        lower_node::lower_node(&mut context, NodeId(id)).map_err(LoweringFlowError::Failed)?;
    }

    let sealed_blocks = context.finalize_blocks().map_err(LoweringFlowError::Failed)?;

    let (res, new_builder) = context.finalize_root(sealed_blocks);
    *builder = new_builder;
    res
}

/// Information required for the finalization of a block.
///
/// A block is not finalized immediately, since the root block should not be finalized in some cases
/// (e.g., where all the arms panic).
enum BlockFinalization {
    /// The block should be finalized with the given [BlockEnd].
    End(BlockBuilder, BlockEnd),
    /// The block is an arm's block that was already sealed and should be merged with the other
    /// arms.
    Sealed(SealedBlockBuilder),
}

impl std::fmt::Debug for BlockFinalization {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BlockFinalization::End(_, block_end) => write!(f, "End({block_end:?})"),
            BlockFinalization::Sealed(_) => write!(f, "Sealed"),
        }
    }
}

struct LowerGraphContext<'a, 'b, 'db> {
    /// The lowering context.
    ctx: &'a mut LoweringContext<'b, 'db>,
    /// The flow control graph to lower.
    graph: &'a FlowControlGraph,
    /// The [BlockBuilder] for the root node.
    root_builder: &'a mut BlockBuilder,
    /// A map from a node id to the [BlockBuilder]s created by its parents.
    /// When a node is visited, it creates a [BlockBuilder] for each of its child nodes
    /// (see [Self::assign_child_block_id]).
    /// Later, when the child node is visited, all the [BlockBuilder]s are merged into a
    /// single [BlockBuilder] (see [Self::start_builder]), on which the node is lowered.
    block_builders: UnorderedHashMap<NodeId, Vec<BlockBuilder>>,
    /// A map from [NodeId] to its finalization info.
    block_finalizations: UnorderedHashMap<NodeId, BlockFinalization>,
    /// The location of the expression being lowered.
    /// This is used for the location of the variables created by block merges during the lowering.
    location: LocationId,
}

impl<'a, 'b, 'db> LowerGraphContext<'a, 'b, 'db> {
    /// Constructs a new [LowerGraphContext].
    fn new(
        ctx: &'a mut LoweringContext<'b, 'db>,
        builder: &'a mut BlockBuilder,
        graph: &'a FlowControlGraph,
        location: LocationId,
    ) -> Self {
        Self {
            ctx,
            graph,
            root_builder: builder,
            block_builders: UnorderedHashMap::default(),
            block_finalizations: UnorderedHashMap::default(),
            location,
        }
    }

    /// Assigns a new [BlockBuilder] and [BlockId] to the given child node.
    /// Registers the [BlockBuilder] in the [Self::block_builders] map, under the child node's id.
    fn assign_child_block_id(
        &mut self,
        child_id: NodeId,
        parent_block_builder: &BlockBuilder,
    ) -> BlockId {
        let block_id = self.ctx.blocks.alloc_empty();
        let child_builder = parent_block_builder.child_block_builder(block_id);
        self.block_builders.entry(child_id).or_default().push(child_builder);
        block_id
    }

    /// Creates a [BlockBuilder] for the given node, based on the parent nodes.
    fn start_builder(&mut self, id: NodeId) -> BlockBuilder {
        if id == self.graph.root {
            let dummy_block_builder = BlockBuilder::root(BlockId(0));
            std::mem::replace(self.root_builder, dummy_block_builder)
        } else {
            // Extract the builders of the parent nodes (the nodes leading to the current node).
            // TODO(lior): Replace unwrap with handling a non-reachable node.
            let parent_builders = self.block_builders.remove(&id).unwrap();
            merge_block_builders(self.ctx, parent_builders, self.location)
        }
    }

    // Finalization functions.

    /// Finalizes all the blocks created during the graph lowering (except for the root block).
    /// Returns the sealed blocks of the arms.
    fn finalize_blocks(&mut self) -> Maybe<Vec<SealedGotoCallsite>> {
        let mut sealed_blocks = vec![];
        for i in 0..self.graph.nodes.len() {
            if i == self.graph.root.0 {
                // The root is finalized in `finalize_root`.
                continue;
            }

            match self.block_finalizations.remove(&NodeId(i)) {
                Some(BlockFinalization::End(builder, block_end)) => {
                    builder.finalize(self.ctx, block_end);
                }
                Some(BlockFinalization::Sealed(sealed_block)) => {
                    // If the block ends with SealedBlockBuilder::Ends, ignore it as it doesn't
                    // return to the callsite and therefore should not be merged.
                    if let SealedBlockBuilder::GotoCallsite(sealed_block) = sealed_block {
                        sealed_blocks.push(sealed_block);
                    }
                }
                None => {
                    panic!("Block {i} was not lowered.");
                }
            }
        }

        Ok(sealed_blocks)
    }

    /// Finalizes the root block and returns the result of the lowering.
    fn finalize_root(
        &mut self,
        sealed_blocks: Vec<SealedGotoCallsite>,
    ) -> (LoweringResult<LoweredExpr>, BlockBuilder) {
        match self.block_finalizations.remove(&self.graph.root) {
            Some(BlockFinalization::End(builder, BlockEnd::Match { info })) => {
                if let Some((new_builder, lowered_expr)) =
                    merge_sealed_block_builders(self.ctx, sealed_blocks, self.location)
                {
                    builder.finalize(self.ctx, BlockEnd::Match { info });
                    (Ok(lowered_expr), new_builder)
                } else {
                    (Err(LoweringFlowError::Match(info)), builder)
                }
            }
            block_finalization => {
                panic!("Unexpected BlockFinalization for root block: {block_finalization:?}.");
            }
        }
    }
}
