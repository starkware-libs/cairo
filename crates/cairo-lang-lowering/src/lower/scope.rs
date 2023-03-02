use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_diagnostics::{Maybe, ToMaybe};
use cairo_lang_semantic as semantic;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use itertools::chain;

use super::context::{LoweredExpr, LoweringContext, LoweringFlowError, LoweringResult, VarRequest};
use crate::{
    BlockId, MatchInfo, Statement, StructuredBlock, StructuredBlockEnd, StructuredStatement,
    VarRemapping, VariableId,
};

/// StructuredBlock builder, describing its current state.
#[derive(Clone)]
pub struct BlockBuilder {
    /// Variables given as inputs to the block, including implicits. Relevant for function blocks /
    /// match arm blocks, etc...
    inputs: Vec<VariableId>,
    /// A store for semantic variables, owning their OwnedVariable instances.
    semantics: OrderedHashMap<semantic::VarId, VariableId>,
    /// The semantic variables that are added/changed in this block.
    changed_semantics: OrderedHashSet<semantic::VarId>,
    /// Current sequence of lowered statements emitted.
    statements: Vec<StructuredStatement>,
    /// Statement pending finalize_statement().
    pending_statement: Option<Statement>,
    /// The block id to use for this block when it's finalized.
    pub block_id: BlockId,
}
impl BlockBuilder {
    /// Creates a new [BlockBuilder] for the root block of a function body.
    pub fn root(_ctx: &LoweringContext<'_>, block_id: BlockId) -> Self {
        BlockBuilder {
            inputs: vec![],
            semantics: Default::default(),
            changed_semantics: Default::default(),
            statements: Default::default(),
            pending_statement: None,
            block_id,
        }
    }

    /// Creates a [BlockBuilder] for a subscope.
    pub fn subscope(&self, block_id: BlockId) -> BlockBuilder {
        BlockBuilder {
            inputs: vec![],
            semantics: self.semantics.clone(),
            changed_semantics: Default::default(),
            statements: Default::default(),
            pending_statement: None,
            block_id,
        }
    }

    /// Creates a [BlockBuilder] for a subscope with unchanged refs.
    pub fn subscope_with_bound_refs(&self, block_id: BlockId) -> BlockBuilder {
        self.subscope(block_id)
    }

    /// Creates a [BlockBuilder] for a sibling scope. This is used when an original block is split
    /// (e.g. after a match statement) to add the ability to 'goto' to after-the-block.
    pub fn sibling_scope(&self, block_id: BlockId) -> BlockBuilder {
        BlockBuilder {
            inputs: vec![],
            semantics: self.semantics.clone(),
            changed_semantics: self.changed_semantics.clone(),
            statements: Default::default(),
            pending_statement: None,
            block_id,
        }
    }

    /// Adds an input to the block.
    pub fn add_input(&mut self, ctx: &mut LoweringContext<'_>, req: VarRequest) -> VariableId {
        let var_id = ctx.new_var(req);
        self.inputs.push(var_id);
        var_id
    }

    /// Binds a semantic variable to a lowered variable.
    pub fn put_semantic(
        &mut self,
        _ctx: &mut LoweringContext<'_>,
        semantic_var_id: semantic::VarId,
        var: VariableId,
    ) {
        if self.semantics.insert(semantic_var_id, var) == Some(var) {
            return;
        }
        self.changed_semantics.insert(semantic_var_id);
    }

    /// Gets the current lowered variable bound to a semantic variable.
    pub fn get_semantic(&self, semantic_var_id: semantic::VarId) -> VariableId {
        self.semantics
            .get(&semantic_var_id)
            .copied()
            .expect("Use of undefined variable cannot happen after semantic phase.")
    }

    /// Adds a statement to the block. finalize_statement() should be called after binding the refs
    /// that are relevant right after the statement, and always before the next push_statement().
    pub fn push_statement(&mut self, stmt: Statement) {
        assert!(self.pending_statement.replace(stmt).is_none(), "finalize_statement() not called.");
    }

    /// Finalizes a statement after binding its refs.
    pub fn finalize_statement(&mut self) {
        let statement = self.pending_statement.take().expect("push_statement() not called.");
        self.statements.push(StructuredStatement { statement });
    }

    /// Adds a statement to the block without rebinding refs - the refs will remain like they were
    /// before this statement.
    pub fn push_finalized_statement(&mut self, stmt: Statement) {
        self.push_statement(stmt);
        self.finalize_statement();
    }

    /// Ends a block with Unreachable.
    pub fn unreachable(self, ctx: &mut LoweringContext<'_>) {
        self.finalize(ctx, StructuredBlockEnd::Unreachable);
    }

    /// Ends a block with an unreachable match.
    pub fn unreachable_match(self, ctx: &mut LoweringContext<'_>, match_info: MatchInfo) {
        self.finalize(ctx, StructuredBlockEnd::Match { info: match_info });
    }

    /// Ends a block with Panic.
    pub fn panic(self, ctx: &mut LoweringContext<'_>, data: VariableId) -> Maybe<()> {
        self.finalize(ctx, StructuredBlockEnd::Panic { data });
        Ok(())
    }

    /// Ends a block with Callsite.
    pub fn goto_callsite(self, expr: Option<VariableId>) -> SealedBlockBuilder {
        SealedBlockBuilder::GotoCallsite { scope: self, expr }
    }

    /// Ends a block with Return.
    pub fn ret(self, ctx: &mut LoweringContext<'_>, expr: VariableId) -> Maybe<()> {
        let ref_vars = ctx
            .ref_params
            .iter()
            .map(|semantic_var_id| self.semantics.get(semantic_var_id).copied())
            .collect::<Option<Vec<_>>>()
            .to_maybe()?;

        self.finalize(
            ctx,
            StructuredBlockEnd::Return { returns: chain!(ref_vars, [expr]).collect() },
        );
        Ok(())
    }

    /// Ends a block with known ending information. Used by [SealedBlockBuilder].
    fn finalize(self, ctx: &mut LoweringContext<'_>, end: StructuredBlockEnd) {
        let block = StructuredBlock { inputs: self.inputs, statements: self.statements, end };
        ctx.blocks.set_block(self.block_id, block);
    }

    /// Ends a block with a match-end. Replaces `self` with a sibling scope.
    pub fn end_with_match(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        merged: MergedBlocks,
        match_info: MatchInfo,
    ) -> LoweringResult<LoweredExpr> {
        let Some(following_block) = merged.following_block else {
            return Err(LoweringFlowError::Match(match_info));
        };
        let new_scope = self.sibling_scope(following_block);
        let prev_scope = std::mem::replace(self, new_scope);
        prev_scope.finalize(ctx, StructuredBlockEnd::Match { info: match_info });
        merged.expr
    }
}

/// Remapping of lowered variables with more semantic information regarding what is the semantic
/// role of the lowered variables.
#[derive(Debug, Default)]
pub struct SemanticRemapping {
    expr: Option<VariableId>,
    semantics: OrderedHashMap<semantic::VarId, VariableId>,
}

/// A sealed BlockBuilder, ready to be merged with sibling blocks to end the block.
#[allow(clippy::large_enum_variant)]
pub enum SealedBlockBuilder {
    /// Block should end by goto callsite. `expr` may be None for blocks that return the unit type.
    GotoCallsite { scope: BlockBuilder, expr: Option<VariableId> },
    /// Block end is already known.
    Ends(BlockId),
}
impl SealedBlockBuilder {
    /// Given the extra information needed, returns the ID of the final block.
    /// This information includes the semantic remapping of variables and the target block to jump
    /// to.
    fn finalize(
        self,
        ctx: &mut LoweringContext<'_>,
        target: Option<BlockId>,
        semantic_remapping: &SemanticRemapping,
    ) -> BlockId {
        match self {
            SealedBlockBuilder::GotoCallsite { mut scope, expr } => {
                let mut remapping = VarRemapping::default();
                // Since SemanticRemapping should have unique variable ids, these asserts will pass.
                for (semantic, remapped_var) in semantic_remapping.semantics.iter() {
                    assert!(remapping.insert(*remapped_var, scope.semantics[*semantic]).is_none());
                }
                if let Some(remapped_var) = semantic_remapping.expr {
                    let expr = expr.unwrap_or_else(|| {
                        LoweredExpr::Tuple {
                            exprs: vec![],
                            location: ctx.variables[remapped_var].location,
                        }
                        .var(ctx, &mut scope)
                        .unwrap()
                    });
                    assert!(remapping.insert(remapped_var, expr).is_none());
                }

                let block_id = scope.block_id;
                scope
                    .finalize(ctx, StructuredBlockEnd::Goto { target: target.unwrap(), remapping });
                block_id
            }
            SealedBlockBuilder::Ends(id) => id,
        }
    }
}

/// Information regarding the merge of sibling blocks.
pub struct MergedBlocks {
    /// The converged expression of the blocks, usable at the calling scope.
    pub expr: LoweringResult<LoweredExpr>,
    pub following_block: Option<BlockId>,
}

/// Merges sibling sealed blocks.
pub fn merge_sealed(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockBuilder,
    sealed_blocks: Vec<SealedBlockBuilder>,
    location: StableLocation,
) -> MergedBlocks {
    // TODO(spapini): When adding Gotos, include the callsite target in the required information to
    // merge.
    // TODO(spapini): Don't remap if we have a single reachable branch.

    let mut semantic_remapping = SemanticRemapping::default();
    let mut n_reachable_blocks = 0;

    // Remap Variables from all blocks.
    for sealed_block in &sealed_blocks {
        let SealedBlockBuilder::GotoCallsite { scope: subscope, expr } = sealed_block else {
            continue;
        };
        n_reachable_blocks += 1;
        if let Some(var) = expr {
            semantic_remapping.expr.get_or_insert_with(|| {
                let var = ctx.variables[*var].clone();
                ctx.variables.alloc(var)
            });
        }
        for semantic in subscope.changed_semantics.iter() {
            if !scope.semantics.contains_key(semantic) {
                // This variable is local to the subscope.
                continue;
            }
            // This variable belongs to an outer scope, and it is changed in at least one branch.
            // It should be remapped.
            semantic_remapping.semantics.entry(*semantic).or_insert_with(|| {
                let var = ctx.variables[subscope.semantics[*semantic]].clone();
                ctx.variables.alloc(var)
            });
        }
    }

    // If there are reachable blocks, create a new empty block for the code after this match.
    let following_block =
        if n_reachable_blocks == 0 { None } else { Some(ctx.blocks.alloc_empty()) };

    for sealed_block in sealed_blocks {
        sealed_block.finalize(ctx, following_block, &semantic_remapping);
    }

    // Apply remapping on scope.
    for (semantic, var) in semantic_remapping.semantics {
        scope.put_semantic(ctx, semantic, var);
    }

    let expr = match semantic_remapping.expr {
        _ if n_reachable_blocks == 0 => Err(LoweringFlowError::Unreachable),
        Some(var) => Ok(LoweredExpr::AtVariable(var)),
        None => Ok(LoweredExpr::Tuple { exprs: vec![], location }),
    };
    MergedBlocks { expr, following_block }
}
