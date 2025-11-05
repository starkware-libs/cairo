//! This module is responsible for the lowering of a flow control graph [FlowControlGraph].

use cairo_lang_diagnostics::Maybe;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

use super::graph::{FlowControlGraph, FlowControlVar, NodeId};
use crate::ids::LocationId;
use crate::lower::block_builder::{
    BlockBuilder, SealedGotoCallsite, merge_block_builders, merge_sealed_block_builders,
};
use crate::lower::context::{
    LoweredExpr, LoweringContext, LoweringFlowError, LoweringResult, handle_lowering_flow_error,
};
use crate::lower::lowered_expr_to_block_scope_end;
use crate::{BlockEnd, BlockId, MatchInfo, VarUsage, VariableId};

mod lower_node;

/// Lowers a flow control graph.
pub fn lower_graph<'db, 'mt>(
    ctx: &mut LoweringContext<'db, 'mt>,
    builder: &mut BlockBuilder<'db>,
    graph: &FlowControlGraph<'db>,
    location: LocationId<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    // Extract the builder from the mutable reference and replace it with a dummy builder.
    let dummy_block_builder = BlockBuilder::root(BlockId(0));
    let builder_ = std::mem::replace(builder, dummy_block_builder);

    let nodes_len = graph.size();
    let mut context = LowerGraphContext::new(ctx, builder_, graph, location);
    // Go over the nodes in reverse order to make sure parent nodes are handled before their
    // children.
    for id in (0..nodes_len).rev() {
        lower_node::lower_node(&mut context, NodeId(id)).map_err(LoweringFlowError::Failed)?;
    }

    let (res, new_builder) = context.finalize();
    // Place the new builder in the mutable reference.
    *builder = new_builder;
    res
}

/// Information about the final result of the lowering of the graph.
enum LowerGraphResult<'db> {
    /// The result is the given expression.
    Expr(LoweredExpr<'db>),
    /// The graph is finalized using the given [MatchInfo] on the sealed blocks from all the arms.
    /// In this case, the final [LoweringResult] may be `Ok` or [LoweringFlowError::Match] (if
    /// all arms panic or return).
    Match(MatchInfo<'db>),
    /// The flow does not continue to the callsite (for example, if all the arms panic or return).
    Error(LoweringFlowError<'db>),
}

/// Helper struct for the lowering of a flow control graph.
struct LowerGraphContext<'db, 'mt, 'a> {
    /// The lowering context.
    ctx: &'a mut LoweringContext<'db, 'mt>,
    /// The flow control graph to lower.
    graph: &'a FlowControlGraph<'db>,
    /// The [BlockBuilder] for the result of the lowering, and the [MatchInfo] for its
    /// finalization.
    result: Option<(BlockBuilder<'db>, LowerGraphResult<'db>)>,
    /// A map from [NodeId] to all the [BlockBuilder]s that lead to it.
    /// When a node is visited, it creates a [BlockBuilder] for each of its child nodes
    /// (see [Self::assign_child_block_id]) and adds it to the map.
    /// For example, when [super::graph::BooleanIf] is visited, two new [BlockBuilder]s are
    /// created for the `true` and `false` arms of the `if`.
    /// Later, when the child node is visited, all the [BlockBuilder]s are merged into a
    /// single [BlockBuilder] (see [Self::get_builder_if_reachable]), on which the node is lowered.
    parent_builders: UnorderedHashMap<NodeId, Vec<BlockBuilder<'db>>>,
    /// A list of sealed blocks for the arms (excluding the arms that do not continue to the
    /// callsite).
    sealed_blocks: Vec<SealedGotoCallsite<'db>>,
    /// A map from [FlowControlVar] to [VariableId].
    vars: UnorderedHashMap<FlowControlVar, VariableId>,
    /// The first node (starting from the root) that does not pass its block builder directly to
    /// the child node (see [Self::pass_builder_to_child]).
    /// The block builder of this node is the original block builder.
    effective_root: NodeId,
    /// The location of the expression being lowered.
    /// This is used for the location of the variables created by block merges during the lowering.
    location: LocationId<'db>,
}

impl<'mt, 'db, 'a> LowerGraphContext<'db, 'mt, 'a> {
    /// Constructs a new [LowerGraphContext].
    fn new(
        ctx: &'a mut LoweringContext<'db, 'mt>,
        root_builder: BlockBuilder<'db>,
        graph: &'a FlowControlGraph<'db>,
        location: LocationId<'db>,
    ) -> Self {
        let root = graph.root();
        Self {
            ctx,
            graph,
            result: None,
            parent_builders: [(root, vec![root_builder])].into_iter().collect(),
            sealed_blocks: vec![],
            vars: UnorderedHashMap::default(),
            effective_root: root,
            location,
        }
    }

    /// Creates a new [BlockBuilder] for a child node.
    ///
    /// In most cases, [Self::assign_child_block_id] should be used instead.
    /// Use [Self::create_child_builder] if statements need to be added to the block before
    /// registering it in the [Self::parent_builders] map (see [Self::register_child_builder]).
    fn create_child_builder(
        &mut self,
        parent_block_builder: &BlockBuilder<'db>,
    ) -> BlockBuilder<'db> {
        parent_block_builder.child_block_builder(self.ctx.blocks.alloc_empty())
    }

    /// Registers a [BlockBuilder] created by [Self::create_child_builder] to the given [NodeId].
    fn register_child_builder(
        &mut self,
        child_id: NodeId,
        child_builder: BlockBuilder<'db>,
    ) -> BlockId {
        let block_id = child_builder.block_id;
        self.parent_builders.entry(child_id).or_default().push(child_builder);
        block_id
    }

    /// Assigns a new [BlockBuilder] and [BlockId] to the given child node.
    /// Registers the [BlockBuilder] in the [Self::parent_builders] map, under the child node's id.
    fn assign_child_block_id(
        &mut self,
        child_id: NodeId,
        parent_block_builder: &BlockBuilder<'db>,
    ) -> BlockId {
        let child_builder = self.create_child_builder(parent_block_builder);
        self.register_child_builder(child_id, child_builder)
    }

    /// Passes the parent block builder to the child node.
    /// This function is used by nodes that have a single child node.
    fn pass_builder_to_child(
        &mut self,
        parent_id: NodeId,
        child_id: NodeId,
        builder: BlockBuilder<'db>,
    ) {
        if parent_id == self.effective_root {
            self.effective_root = child_id;
        }
        self.parent_builders.entry(child_id).or_default().push(builder);
    }

    /// Creates a [BlockBuilder] for the given node, based on the parent nodes.
    ///
    /// Returns `None` if the node is unreachable (for example, if the node that was supposed to
    /// lead to it always panics or returns from the function).
    fn get_builder_if_reachable(&mut self, id: NodeId) -> Option<BlockBuilder<'db>> {
        // Extract the builders of the parent nodes (the nodes leading to the current node).
        if let Some(parent_builders) = self.parent_builders.remove(&id) {
            Some(merge_block_builders(self.ctx, parent_builders, self.location))
        } else {
            None
        }
    }

    /// Registers that a [FlowControlVar] is stored in the lowering context as [VarUsage].
    ///
    /// This function is called when lowering the node creating the [FlowControlVar].
    /// Later, when the [FlowControlVar] is used in another node, [Self::vars] is used to get the
    /// [VarUsage].
    fn register_var(&mut self, var_id: FlowControlVar, lowered_var: VariableId) {
        assert!(
            self.vars.insert(var_id, lowered_var).is_none(),
            "Variable {var_id:?} was already registered.",
        );
    }

    /// Returns the [VarUsage] of the given [FlowControlVar].
    ///
    /// Note that the usage location of [FlowControlVar]s is identical to the location of their
    /// definition.
    fn var_usage(&self, var: FlowControlVar) -> VarUsage<'db> {
        VarUsage { var_id: self.vars[&var], location: var.location(self.graph) }
    }

    // Finalization functions.

    /// Finalizes a block with a match.
    ///
    /// The root block is not finalized immediately, since the API of lowering functions
    /// requires that the block will not be finalized if it does not continue to the
    /// callsite (for example, if all the arms panic or return from the function).
    fn finalize_with_match(
        &mut self,
        id: NodeId,
        builder: BlockBuilder<'db>,
        info: MatchInfo<'db>,
    ) {
        if id == self.effective_root {
            self.result = Some((builder, LowerGraphResult::Match(info)));
        } else {
            builder.finalize(self.ctx, BlockEnd::Match { info });
        }
    }

    /// Finalizes a block that does not continue to the callsite with the given [LoweringFlowError].
    /// For example, a block that always panics or returns from the function.
    fn block_doesnt_continue(
        &mut self,
        id: NodeId,
        builder: BlockBuilder<'db>,
        err: LoweringFlowError<'db>,
    ) -> Maybe<()> {
        if id == self.effective_root {
            self.result = Some((builder, LowerGraphResult::Error(err)));
        } else {
            handle_lowering_flow_error(self.ctx, builder, err)?;
        }
        Ok(())
    }

    /// Finalizes an arm block that ends with the given `lowering_result`.
    fn finalize_with_arm(
        &mut self,
        id: NodeId,
        builder: BlockBuilder<'db>,
        lowering_result: LoweringResult<'db, LoweredExpr<'db>>,
    ) -> Maybe<()> {
        if id == self.effective_root {
            let lower_graph_result = match lowering_result {
                Ok(lowered_expr) => LowerGraphResult::Expr(lowered_expr),
                Err(err) => LowerGraphResult::Error(err),
            };
            self.result = Some((builder, lower_graph_result));
        } else {
            let sealed_block = lowered_expr_to_block_scope_end(self.ctx, builder, lowering_result)?;
            if let Some(sealed_block) = sealed_block {
                self.sealed_blocks.push(sealed_block);
            }
        }

        Ok(())
    }

    /// Finalizes the lowering of the graph.
    ///
    /// Returns the [LoweredExpr] or [LoweringFlowError] for the resulting expression,
    /// and the [BlockBuilder] with the state at the end of the graph.
    fn finalize(mut self) -> (LoweringResult<'db, LoweredExpr<'db>>, BlockBuilder<'db>) {
        let (builder, match_info) = self.result.take().unwrap();

        match match_info {
            LowerGraphResult::Match(match_info) => {
                if let Some((new_builder, lowered_expr)) =
                    merge_sealed_block_builders(self.ctx, self.sealed_blocks, self.location)
                {
                    builder.finalize(self.ctx, BlockEnd::Match { info: match_info });
                    (Ok(lowered_expr), new_builder)
                } else {
                    (Err(LoweringFlowError::Match(match_info)), builder)
                }
            }
            LowerGraphResult::Error(err) => (Err(err), builder),
            LowerGraphResult::Expr(lowered_expr) => (Ok(lowered_expr), builder),
        }
    }
}
