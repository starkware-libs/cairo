//! This module is responsible for the lowering of a flow control graph [FlowControlGraph].

use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

use super::graph::{FlowControlGraph, NodeId};
use crate::ids::LocationId;
use crate::lower::block_builder::{
    BlockBuilder, SealedBlockBuilder, SealedGotoCallsite, merge_block_builders,
    merge_sealed_block_builders,
};
use crate::lower::context::{LoweredExpr, LoweringContext, LoweringFlowError, LoweringResult};
use crate::{BlockEnd, BlockId, MatchInfo};

mod lower_node;

/// Lowers a flow control graph.
#[allow(dead_code)]
pub fn lower_graph(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    graph: &FlowControlGraph,
    location: LocationId,
) -> LoweringResult<LoweredExpr> {
    // Extract the builder from the mutable reference and replace it with a dummy builder.
    let dummy_block_builder = BlockBuilder::root(BlockId(0));
    let builder_ = std::mem::replace(builder, dummy_block_builder);

    let mut context = LowerGraphContext::new(ctx, builder_, graph, location);
    // Go over the nodes in reverse order to make sure parent nodes are handled before their
    // children.
    for id in (0..graph.nodes.len()).rev() {
        lower_node::lower_node(&mut context, NodeId(id)).map_err(LoweringFlowError::Failed)?;
    }

    let (res, new_builder) = context.finalize();
    // Place the new builder in the mutable reference.
    *builder = new_builder;
    res
}

/// Helper struct for the lowering of a flow control graph.
struct LowerGraphContext<'a, 'b, 'db> {
    /// The lowering context.
    ctx: &'a mut LoweringContext<'b, 'db>,
    /// The flow control graph to lower.
    graph: &'a FlowControlGraph,
    /// The [BlockBuilder] for the result of the lowering, and the [MatchInfo] for its
    /// finalization.
    result: Option<(BlockBuilder, MatchInfo)>,
    /// A map from [NodeId] to all the [BlockBuilder]s that lead to it.
    /// When a node is visited, it creates a [BlockBuilder] for each of its child nodes
    /// (see [Self::assign_child_block_id]) and adds it to the map.
    /// For example, when [super::graph::BooleanIf] is visited, two new [BlockBuilder]s are
    /// created for the `true` and `false` arms of the `if`.
    /// Later, when the child node is visited, all the [BlockBuilder]s are merged into a
    /// single [BlockBuilder] (see [Self::start_builder]), on which the node is lowered.
    parent_builders: UnorderedHashMap<NodeId, Vec<BlockBuilder>>,
    /// A list of sealed blocks for the arms (excluding the arms that do not continue to the
    /// callsite).
    sealed_blocks: Vec<SealedGotoCallsite>,
    /// The location of the expression being lowered.
    /// This is used for the location of the variables created by block merges during the lowering.
    location: LocationId,
}

impl<'a, 'b, 'db> LowerGraphContext<'a, 'b, 'db> {
    /// Constructs a new [LowerGraphContext].
    fn new(
        ctx: &'a mut LoweringContext<'b, 'db>,
        root_builder: BlockBuilder,
        graph: &'a FlowControlGraph,
        location: LocationId,
    ) -> Self {
        Self {
            ctx,
            graph,
            result: None,
            parent_builders: [(graph.root(), vec![root_builder])].into_iter().collect(),
            sealed_blocks: vec![],
            location,
        }
    }

    /// Assigns a new [BlockBuilder] and [BlockId] to the given child node.
    /// Registers the [BlockBuilder] in the [Self::parent_builders] map, under the child node's id.
    fn assign_child_block_id(
        &mut self,
        child_id: NodeId,
        parent_block_builder: &BlockBuilder,
    ) -> BlockId {
        let block_id = self.ctx.blocks.alloc_empty();
        let child_builder = parent_block_builder.child_block_builder(block_id);
        self.parent_builders.entry(child_id).or_default().push(child_builder);
        block_id
    }

    /// Creates a [BlockBuilder] for the given node, based on the parent nodes.
    fn start_builder(&mut self, id: NodeId) -> BlockBuilder {
        // Extract the builders of the parent nodes (the nodes leading to the current node).
        // TODO(lior): Replace unwrap with handling a non-reachable node.
        let parent_builders = self.parent_builders.remove(&id).unwrap();
        merge_block_builders(self.ctx, parent_builders, self.location)
    }

    // Finalization functions.

    /// Finalizes a block with a match.
    ///
    /// The root block is not finalized immediately, since the API of lowering functions
    /// requires that the block will not be finalized if it does not continue to the
    /// callsite (for example, if all the arms panic or return from the function).
    fn finalize_with_match(&mut self, id: NodeId, builder: BlockBuilder, info: MatchInfo) {
        if id == self.graph.root() {
            self.result = Some((builder, info));
        } else {
            builder.finalize(self.ctx, BlockEnd::Match { info });
        }
    }

    /// Adds a sealed block to the context.
    ///
    /// The block is an arm's block that was already sealed and should be merged with the other
    /// arms.
    fn add_sealed_block(&mut self, sealed_block: SealedBlockBuilder) {
        match sealed_block {
            SealedBlockBuilder::GotoCallsite(sealed_block) => {
                self.sealed_blocks.push(sealed_block);
            }
            SealedBlockBuilder::Ends(_) => {
                // If the block ends with `SealedBlockBuilder::Ends`, ignore it as it doesn't
                // return to the callsite and therefore should not be merged.
            }
        }
    }

    /// Finalizes the lowering of the graph.
    ///
    /// Returns the [BlockBuilder] with the state at the end of the graph, and the
    /// [LoweredExpr] or [LoweringFlowError] for the resulting expression.
    fn finalize(mut self) -> (LoweringResult<LoweredExpr>, BlockBuilder) {
        let (builder, match_info) = self.result.take().unwrap();
        if let Some((new_builder, lowered_expr)) =
            merge_sealed_block_builders(self.ctx, self.sealed_blocks, self.location)
        {
            builder.finalize(self.ctx, BlockEnd::Match { info: match_info });
            (Ok(lowered_expr), new_builder)
        } else {
            (Err(LoweringFlowError::Match(match_info)), builder)
        }
    }
}
