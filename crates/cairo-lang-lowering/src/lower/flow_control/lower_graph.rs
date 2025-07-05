//! This module is responsible for the lowering of a flow control graph [FlowControlGraph].

use cairo_lang_semantic::MatchArmSelector;
use cairo_lang_semantic::corelib;
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

use super::graph::{ArmExpr, BooleanIf, FlowControlGraph, FlowControlNode, NodeId};
use crate::ids::LocationId;
use crate::lower::block_builder::{BlockBuilder, SealedBlockBuilder};
use crate::lower::context::{
    LoweredExpr, LoweringContext, LoweringFlowError, LoweringResult, VarRequest,
};
use crate::lower::{
    lower_expr, lower_expr_to_var_usage, lowered_expr_to_block_scope_end,
};
use crate::{BlockEnd, BlockId, MatchArm, MatchEnumInfo, MatchInfo};

/// Lowers a flow control graph.
#[allow(dead_code)]
pub fn lower_graph(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    graph: &FlowControlGraph,
) -> LoweringResult<LoweredExpr> {
    let mut context = LowerGraphContext::new(ctx, builder, graph);
    // Go over the nodes in reverse order to make sure parent nodes are handled before their
    // children.
    for id in (0..graph.nodes.len()).rev() {
        context.lower_node(NodeId(id))?;
    }

    let (res, new_builder) = context.finalize_blocks();
    *builder = new_builder;
    res
}

enum BlockFinalization {
    End(BlockBuilder, BlockEnd, LocationId),
    Sealed(SealedBlockBuilder),
}

struct LowerGraphContext<'a, 'b, 'db> {
    /// The lowering context.
    ctx: &'a mut LoweringContext<'b, 'db>,
    /// The flow control graph to lower.
    graph: &'a FlowControlGraph,
    /// The [BlockBuilder] for the root node.
    root_builder: &'a mut BlockBuilder,
    /// A map from a node id to its predecessors (the list of nodes leading to it).
    predecessors: Vec<Vec<NodeId>>,
    /// A map from a node id to its lowering block id.
    block_ids: UnorderedHashMap<NodeId, BlockId>,
    /// A map from a node id to its finalization info.
    block_finalizations: UnorderedHashMap<NodeId, BlockFinalization>,
}

impl<'a, 'b, 'db> LowerGraphContext<'a, 'b, 'db> {
    fn new(
        ctx: &'a mut LoweringContext<'b, 'db>,
        builder: &'a mut BlockBuilder,
        graph: &'a FlowControlGraph,
    ) -> Self {
        Self {
            ctx,
            graph,
            root_builder: builder,
            predecessors: find_predecessors(graph),
            block_ids: UnorderedHashMap::default(),
            block_finalizations: UnorderedHashMap::default(),
        }
    }

    fn get_block_id(&mut self, id: NodeId) -> BlockId {
        *self.block_ids.entry(id).or_insert_with(|| self.ctx.blocks.alloc_empty())
    }

    /// Creates a [BlockBuilder] for the given node, based on the parent nodes.
    fn start_builder(&mut self, id: NodeId) -> BlockBuilder {
        if id == self.graph.root {
            self.root_builder.clone() // TODO: Don't clone.
        } else {
            let block_id = self.get_block_id(id);
            let [parent_node_id] = &self.predecessors[id.0][..] else {
                panic!(
                    "Node {:?} has {} predecessors. Only one predecessor is supported",
                    id,
                    self.predecessors[id.0].len()
                );
            };
            let Some(BlockFinalization::End(parent_builder, _, _)) =
                self.block_finalizations.get(parent_node_id)
            else {
                panic!(); // TODO
            };

            parent_builder.child_block_builder(block_id)
        }
    }

    // TODO: Should we use `LoweringResult`?
    /// Lowers the given node.
    fn lower_node(&mut self, id: NodeId) -> LoweringResult<()> {
        let builder = self.start_builder(id);
        let block_end = match &self.graph.nodes[id.0] {
            FlowControlNode::BooleanIf(node) => self.lower_boolean_if(builder, node),
            FlowControlNode::ArmExpr(node) => self.lower_arm_expr(builder, node),
            _ => todo!(),
        }?;

        assert!(self.block_finalizations.insert(id, block_end).is_none());
        Ok(())
    }

    fn lower_boolean_if(
        &mut self,
        mut builder: BlockBuilder,
        node: &BooleanIf,
    ) -> LoweringResult<(BlockFinalization)> {
        // The condition cannot be unit.
        let condition_var = lower_expr_to_var_usage(self.ctx, &mut builder, node.condition)?;
        let db = self.ctx.db;
        let unit_ty = corelib::unit_ty(db);

        let condition_expr = &self.ctx.function_body.arenas.exprs[node.condition];
        let stable_ptr = condition_expr.stable_ptr().untyped();
        let condition_location = self.ctx.get_location(stable_ptr);

        // Allocate/retrieve block ids for the two branches.
        let true_branch_block_id = self.get_block_id(node.true_branch);
        let false_branch_block_id = self.get_block_id(node.false_branch);

        // Create dummy variables for the unit type inside the bool's variants.
        let true_branch_var_id =
            self.ctx.new_var(VarRequest { ty: unit_ty, location: condition_location });
        let false_branch_var_id =
            self.ctx.new_var(VarRequest { ty: unit_ty, location: condition_location });

        // Finalize the block.
        let match_info = MatchInfo::Enum(MatchEnumInfo {
            concrete_enum_id: corelib::core_bool_enum(db),
            input: condition_var,
            arms: vec![
                MatchArm {
                    arm_selector: MatchArmSelector::VariantId(corelib::false_variant(db)),
                    block_id: false_branch_block_id,
                    var_ids: vec![false_branch_var_id],
                },
                MatchArm {
                    arm_selector: MatchArmSelector::VariantId(corelib::true_variant(db)),
                    block_id: true_branch_block_id,
                    var_ids: vec![true_branch_var_id],
                },
            ],
            location: condition_location,
        });

        Ok((BlockFinalization::End(
            builder,
            BlockEnd::Match { info: match_info },
            condition_location,
        )))
    }

    fn lower_arm_expr(
        &mut self,
        mut builder: BlockBuilder,
        node: &ArmExpr,
    ) -> LoweringResult<BlockFinalization> {
        let lowered_expr = lower_expr(self.ctx, &mut builder, node.expr);
        let sealed_block = lowered_expr_to_block_scope_end(self.ctx, builder, lowered_expr)
            .map_err(LoweringFlowError::Failed)?;
        Ok(BlockFinalization::Sealed(sealed_block))
    }

    fn finalize_blocks(&mut self) -> (LoweringResult<LoweredExpr>, BlockBuilder) {
        let mut sealed_blocks = vec![];
        for i in 0..self.graph.nodes.len() {
            if i == self.graph.root.0 {
                continue;
            }

            match self.block_finalizations.remove(&NodeId(i)) {
                Some(BlockFinalization::End(builder, block_end, _)) => {
                    builder.finalize(self.ctx, block_end);
                }
                Some(BlockFinalization::Sealed(sealed_block)) => {
                    sealed_blocks.push(sealed_block);
                }
                None => {
                    panic!("Block {} was not finalized.", i);
                }
            }
        }

        let Some(BlockFinalization::End(mut builder, BlockEnd::Match { info }, condition_location)) =
            self.block_finalizations.remove(&self.graph.root)
        else {
            panic!()
        };

        (
            builder.merge_and_end_with_match(self.ctx, info, sealed_blocks, condition_location),
            builder,
        )
    }
}

/// Finds the predecessors of every node in the graph.
/// Returns a vector where each index corresponds to a NodeId,
/// and the value is a vector of NodeIds that lead to that node.
pub fn find_predecessors(graph: &FlowControlGraph) -> Vec<Vec<NodeId>> {
    let mut predecessors = vec![Vec::new(); graph.nodes.len()];

    // Iterate through all nodes and their next nodes
    for (node_id, node) in graph.nodes.iter().enumerate() {
        let current_node_id = NodeId(node_id);

        // For each next node, add the current node as a predecessor
        for next_node_id in node.next_nodes() {
            predecessors[next_node_id.0].push(current_node_id);
        }
    }

    predecessors
}
