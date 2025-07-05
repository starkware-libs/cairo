//! This module is responsible for the lowering of a flow control graph [FlowControlGraph].

use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::{MatchArmSelector, corelib};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

use super::graph::{ArmExpr, BooleanIf, FlowControlGraph, FlowControlNode, NodeId};
use crate::ids::LocationId;
use crate::lower::block_builder::{BlockBuilder, SealedBlockBuilder, merge_block_builders};
use crate::lower::context::{
    lowering_flow_error_to_sealed_block, LoweredExpr, LoweringContext, LoweringFlowError, LoweringResult, VarRequest
};
use crate::lower::{lower_expr, lower_expr_to_var_usage, lowered_expr_to_block_scope_end};
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
        // TODO: is it ok to return in `?` without updaing builder?
        context.lower_node(NodeId(id)).map_err(LoweringFlowError::Failed)?;
    }

    let sealed_blocks = context.finalize_blocks().map_err(LoweringFlowError::Failed)?;

    let (res, new_builder) = context.finalize_root(sealed_blocks);
    *builder = new_builder;
    res
}

enum BlockFinalization {
    // TODO: doc.
    End(BlockBuilder, BlockEnd, LocationId),
    // TODO: doc.
    Sealed(SealedBlockBuilder),
    // TODO: doc.
    Missing,
    // TODO: doc.
    JumpsOutside(BlockBuilder, LoweringFlowError),
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
    /// A map from a node id to the BlockBuilders created by its parents.
    block_builders: UnorderedHashMap<NodeId, Vec<BlockBuilder>>,
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
            block_builders: UnorderedHashMap::default(),
            block_finalizations: UnorderedHashMap::default(),
        }
    }

    fn assign_child_block_id(
        &mut self,
        parent_id: NodeId,
        parent_block_builder: &BlockBuilder,
    ) -> BlockId {
        let block_id = self.ctx.blocks.alloc_empty();
        let child_builder = parent_block_builder.child_block_builder(block_id);
        self.block_builders.entry(parent_id).or_insert_with(|| vec![]).push(child_builder);
        block_id
    }

    /// Creates a [BlockBuilder] for the given node, based on the parent nodes.
    fn start_builder(&mut self, id: NodeId, location: LocationId) -> Option<BlockBuilder> {
        if id == self.graph.root {
            let dummy_block_builder = BlockBuilder::root(BlockId(0));
            Some(std::mem::replace(self.root_builder, dummy_block_builder))
        } else {
            // Extract the builders of the parent nodes (the nodes leading to the current node).
            if let Some(parent_builders) = self.block_builders.remove(&id) {
                Some(merge_block_builders(self.ctx, parent_builders, location))
            } else {
                None
            }
        }
    }

    // TODO: Should we use `LoweringResult`?
    /// Lowers the given node.
    fn lower_node(&mut self, id: NodeId) -> Maybe<()> {
        let block_end = match &self.graph.nodes[id.0] {
            FlowControlNode::BooleanIf(node) => self.lower_boolean_if(id, node),
            FlowControlNode::ArmExpr(node) => self.lower_arm_expr(id, node),
            _ => todo!(),
        }?;

        assert!(self.block_finalizations.insert(id, block_end).is_none());
        Ok(())
    }

    fn lower_boolean_if(
        &mut self,
        id: NodeId,
        node: &BooleanIf,
    ) -> Maybe<BlockFinalization> {
        // The condition cannot be unit.
        let db = self.ctx.db;
        let unit_ty = corelib::unit_ty(db);

        let condition_location = get_expr_location(self.ctx, &node.condition);
        let Some(mut builder) = self.start_builder(id, condition_location) else {
            return Ok(BlockFinalization::Missing);
        };

        let lowered_condition = lower_expr_to_var_usage(self.ctx, &mut builder, node.condition);
        let condition_var = match lowered_condition {
            Ok(condition_var) => condition_var,
            Err(err) => {
                return Ok(BlockFinalization::JumpsOutside(builder, err));
            }
        };

        // Allocate/retrieve block ids for the two branches.
        let true_branch_block_id = self.assign_child_block_id(node.true_branch, &builder);
        let false_branch_block_id = self.assign_child_block_id(node.false_branch, &builder);

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

    fn lower_arm_expr(&mut self, id: NodeId, node: &ArmExpr) -> Maybe<BlockFinalization> {
        let Some(mut builder) = self.start_builder(id, get_expr_location(self.ctx, &node.expr)) else {
            return Ok(BlockFinalization::Missing);
        };

        let lowered_expr = lower_expr(self.ctx, &mut builder, node.expr);
        let sealed_block = lowered_expr_to_block_scope_end(self.ctx, builder, lowered_expr)?;
        Ok(BlockFinalization::Sealed(sealed_block))
    }

    fn finalize_blocks(&mut self) -> Maybe<Vec<SealedBlockBuilder>> {
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
                Some(BlockFinalization::Missing) => {
                    println!("Node {} was skipped.", i); // TODO
                    continue;
                }
                Some(BlockFinalization::JumpsOutside(builder, err)) => {
                    lowering_flow_error_to_sealed_block(self.ctx, builder, err)?;
                }
                None => {
                    panic!("Block {} was not finalized.", i);
                }
            }
        }

        Ok(sealed_blocks)
    }

    fn finalize_root(&mut self, sealed_blocks: Vec<SealedBlockBuilder>) -> (LoweringResult<LoweredExpr>, BlockBuilder) {
        match self.block_finalizations.remove(&self.graph.root) {
            Some(BlockFinalization::End(mut builder, BlockEnd::Match { info }, condition_location)) => {
                (
                    // TODO: Replace with `merge_block_builders`?
                    builder.merge_and_end_with_match(self.ctx, info, sealed_blocks, condition_location),
                    builder,
                )
            }
            Some(BlockFinalization::JumpsOutside(builder, err)) => {
                (Err(err), builder)
            }
            _ => {
                panic!("Root block was not finalized.");
            }
        }


    }
}

fn get_expr_location(ctx: &LoweringContext<'_, '_>, expr: &semantic::ExprId) -> LocationId {
    let condition_expr = &ctx.function_body.arenas.exprs[*expr];
    let stable_ptr = condition_expr.stable_ptr().untyped();
    ctx.get_location(stable_ptr)
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
