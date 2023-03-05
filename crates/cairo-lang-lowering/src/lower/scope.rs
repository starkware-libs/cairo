use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::MemberId;
use cairo_lang_diagnostics::{Maybe, ToMaybe};
use cairo_lang_semantic as semantic;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use itertools::{chain, zip_eq, Itertools};
use semantic::items::structure::SemanticStructEx;
use semantic::{ConcreteTypeId, TypeLongId, VarMemberPath};

use super::context::{LoweredExpr, LoweringContext, LoweringFlowError, LoweringResult, VarRequest};
use super::generators;
use super::generators::StatementsBuilder;
use super::refs::{SemanticLoweringMapping, StructRecomposer};
use crate::{BlockId, FlatBlock, FlatBlockEnd, MatchInfo, Statement, VarRemapping, VariableId};

/// FlatBlock builder, describing its current state.
#[derive(Clone)]
pub struct BlockBuilder {
    /// Variables given as inputs to the block, including implicits. Relevant for function blocks /
    /// match arm blocks, etc...
    inputs: Vec<VariableId>,
    /// A store for semantic variables, owning their OwnedVariable instances.
    semantics: SemanticLoweringMapping,
    /// The semantic variables that are added/changed in this block.
    changed_semantics: OrderedHashSet<semantic::VarId>,
    /// Current sequence of lowered statements emitted.
    pub statements: StatementsBuilder,
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
    pub fn put_semantic(&mut self, semantic_var_id: semantic::VarId, var: VariableId) {
        self.semantics.insert_semantic_var(semantic_var_id, var);
        self.changed_semantics.insert(semantic_var_id);
    }

    pub fn update_ref(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        member_path: &VarMemberPath,
        var: VariableId,
    ) {
        let location = ctx.get_location(member_path.stable_ptr().untyped());
        self.semantics.update_member_path(
            BlockStructRecomposer { statements: &mut self.statements, ctx, location },
            member_path,
            var,
        );
        self.changed_semantics.insert(member_path.base_var());
    }

    pub fn get_ref(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        member_path: &VarMemberPath,
    ) -> Option<VariableId> {
        let location = ctx.get_location(member_path.stable_ptr().untyped());
        self.semantics.get_member_path(
            BlockStructRecomposer { statements: &mut self.statements, ctx, location },
            member_path,
        )
    }

    /// Gets the current lowered variable bound to a semantic variable.
    pub fn get_semantic(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        semantic_var_id: semantic::VarId,
        location: StableLocation,
    ) -> VariableId {
        self.semantics
            .get_semantic_var(
                BlockStructRecomposer { statements: &mut self.statements, ctx, location },
                &semantic_var_id,
            )
            .expect("Use of undefined variable cannot happen after semantic phase.")
    }

    /// Adds a statement to the block.
    pub fn push_statement(&mut self, statement: Statement) {
        self.statements.push_statement(statement);
    }

    /// Ends a block with an unreachable match.
    pub fn unreachable_match(self, ctx: &mut LoweringContext<'_>, match_info: MatchInfo) {
        self.finalize(ctx, FlatBlockEnd::Match { info: match_info });
    }

    /// Ends a block with Panic.
    pub fn panic(self, ctx: &mut LoweringContext<'_>, data: VariableId) -> Maybe<()> {
        self.finalize(ctx, FlatBlockEnd::Panic(data));
        Ok(())
    }

    /// Ends a block with Callsite.
    pub fn goto_callsite(self, expr: Option<VariableId>) -> SealedBlockBuilder {
        SealedBlockBuilder::GotoCallsite { scope: self, expr }
    }

    /// Ends a block with Return.
    pub fn ret(
        mut self,
        ctx: &mut LoweringContext<'_>,
        expr: VariableId,
        location: StableLocation,
    ) -> Maybe<()> {
        let ref_vars = ctx
            .ref_params
            .iter()
            .map(|semantic_var_id| {
                self.semantics.get_semantic_var(
                    BlockStructRecomposer { statements: &mut self.statements, ctx, location },
                    semantic_var_id,
                )
            })
            .collect::<Option<Vec<_>>>()
            .to_maybe()?;

        self.finalize(ctx, FlatBlockEnd::Return(chain!(ref_vars, [expr]).collect()));
        Ok(())
    }

    /// Ends a block with known ending information. Used by [SealedBlockBuilder].
    fn finalize(self, ctx: &mut LoweringContext<'_>, end: FlatBlockEnd) {
        let block = FlatBlock { inputs: self.inputs, statements: self.statements.statements, end };
        ctx.blocks.set_block(self.block_id, block);
    }

    /// Merges the sealed blocks and ends the block with a match-end.
    /// Replaces `self` with a sibling scope.
    pub fn merge_and_end_with_match(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        match_info: MatchInfo,
        sealed_blocks: Vec<SealedBlockBuilder>,
        location: StableLocation,
    ) -> LoweringResult<LoweredExpr> {
        let Some((merged_expr, following_block)) = self.merge_sealed(ctx, sealed_blocks, location) else {
            return Err(LoweringFlowError::Match(match_info));
        };

        let new_scope = self.sibling_scope(following_block);
        let prev_scope = std::mem::replace(self, new_scope);
        prev_scope.finalize(ctx, FlatBlockEnd::Match { info: match_info });
        Ok(merged_expr)
    }

    /// Merges sibling sealed blocks.
    /// If there are reachable blocks, returns the converged expression of the blocks, usable at the
    /// calling scope, and the following block ID.
    /// Otherwise, returns None.
    fn merge_sealed(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        sealed_blocks: Vec<SealedBlockBuilder>,
        location: StableLocation,
    ) -> Option<(LoweredExpr, BlockId)> {
        // TODO(spapini): When adding Gotos, include the callsite target in the required information
        // to merge.
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
                if !self.semantics.contains_semantic_var(semantic) {
                    // This variable is local to the subscope.
                    continue;
                }
                // This variable belongs to an outer scope, and it is changed in at least one
                // branch. It should be remapped.
                semantic_remapping.semantics.entry(*semantic).or_insert_with(|| {
                    let var = self.get_semantic(ctx, *semantic, location);
                    let var = ctx.variables[var].clone();
                    ctx.variables.alloc(var)
                });
            }
        }

        if n_reachable_blocks == 0 {
            return None;
        }

        // If there are reachable blocks, create a new empty block for the code after this match.
        let following_block = ctx.blocks.alloc_empty();

        for sealed_block in sealed_blocks {
            sealed_block.finalize(ctx, following_block, &semantic_remapping, location);
        }

        // Apply remapping on scope.
        for (semantic, var) in semantic_remapping.semantics {
            self.put_semantic(semantic, var);
        }

        let expr = match semantic_remapping.expr {
            Some(var) => LoweredExpr::AtVariable(var),
            None => LoweredExpr::Tuple { exprs: vec![], location },
        };
        Some((expr, following_block))
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
    /// Finalizes a non-finalized block, given the semantic remapping of variables and the target
    /// block to jump to.
    fn finalize(
        self,
        ctx: &mut LoweringContext<'_>,
        target: BlockId,
        semantic_remapping: &SemanticRemapping,
        location: StableLocation,
    ) {
        if let SealedBlockBuilder::GotoCallsite { mut scope, expr } = self {
            let mut remapping = VarRemapping::default();
            // Since SemanticRemapping should have unique variable ids, these asserts will pass.
            for (semantic, remapped_var) in semantic_remapping.semantics.iter() {
                assert!(
                    remapping
                        .insert(*remapped_var, scope.get_semantic(ctx, *semantic, location))
                        .is_none()
                );
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

            scope.finalize(ctx, FlatBlockEnd::Goto(target, remapping));
        }
    }
}

struct BlockStructRecomposer<'a, 'b> {
    statements: &'a mut StatementsBuilder,
    ctx: &'a mut LoweringContext<'b>,
    location: StableLocation,
}
impl<'a, 'b> StructRecomposer for BlockStructRecomposer<'a, 'b> {
    fn deconstruct(
        &mut self,
        concrete_struct_id: semantic::ConcreteStructId,
        value: VariableId,
    ) -> OrderedHashMap<MemberId, VariableId> {
        let members = self.ctx.db.concrete_struct_members(concrete_struct_id).unwrap();
        let members = members.values().collect_vec();
        let member_ids = members.iter().map(|m| m.id);
        let var_reqs = members
            .iter()
            .map(|member| VarRequest { ty: member.ty, location: self.location })
            .collect();
        let member_values =
            generators::StructDestructure { input: value, var_reqs }.add(self.ctx, self.statements);
        OrderedHashMap::from_iter(zip_eq(member_ids, member_values))
    }

    fn reconstruct(
        &mut self,
        concrete_struct_id: semantic::ConcreteStructId,
        members: Vec<VariableId>,
    ) -> VariableId {
        let ty = self
            .ctx
            .db
            .intern_type(TypeLongId::Concrete(ConcreteTypeId::Struct(concrete_struct_id)));
        generators::StructConstruct { inputs: members, ty, location: self.location }
            .add(self.ctx, self.statements)
    }

    fn var_ty(&self, var: VariableId) -> semantic::TypeId {
        self.ctx.variables[var].ty
    }
}
