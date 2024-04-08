use cairo_lang_defs::ids::{MemberId, NamedLanguageElementId};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic as semantic;
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use itertools::{chain, zip_eq, Itertools};
use semantic::items::structure::SemanticStructEx;
use semantic::{ConcreteTypeId, ExprVarMemberPath, TypeLongId};

use super::context::{LoweredExpr, LoweringContext, LoweringFlowError, LoweringResult, VarRequest};
use super::generators;
use super::generators::StatementsBuilder;
use super::refs::{SemanticLoweringMapping, StructRecomposer};
use super::usage::MemberPath;
use crate::db::LoweringGroup;
use crate::diagnostic::LoweringDiagnosticKind;
use crate::ids::LocationId;
use crate::{
    BlockId, FlatBlock, FlatBlockEnd, MatchInfo, Statement, VarRemapping, VarUsage, VariableId,
};

/// FlatBlock builder, describing its current state.
#[derive(Clone)]
pub struct BlockBuilder {
    /// A store for semantic variables, owning their OwnedVariable instances.
    pub semantics: SemanticLoweringMapping,
    /// The semantic variables that are added/changed in this block.
    changed_member_paths: OrderedHashSet<MemberPath>,
    /// Current sequence of lowered statements emitted.
    pub statements: StatementsBuilder,
    /// The block id to use for this block when it's finalized.
    pub block_id: BlockId,
}
impl BlockBuilder {
    /// Creates a new [BlockBuilder] for the root block of a function body.
    pub fn root(_ctx: &mut LoweringContext<'_, '_>, block_id: BlockId) -> Self {
        BlockBuilder {
            semantics: Default::default(),
            changed_member_paths: Default::default(),
            statements: Default::default(),
            block_id,
        }
    }

    /// Creates a [BlockBuilder] for a subscope.
    pub fn child_block_builder(&self, block_id: BlockId) -> BlockBuilder {
        BlockBuilder {
            semantics: self.semantics.clone(),
            changed_member_paths: Default::default(),
            statements: Default::default(),
            block_id,
        }
    }

    /// Creates a [BlockBuilder] for a sibling builder. This is used when an original block is split
    /// (e.g. after a match statement) to add the ability to 'goto' to after-the-block.
    pub fn sibling_block_builder(&self, block_id: BlockId) -> BlockBuilder {
        BlockBuilder {
            semantics: self.semantics.clone(),
            changed_member_paths: self.changed_member_paths.clone(),
            statements: Default::default(),
            block_id,
        }
    }

    /// Binds a semantic variable to a lowered variable.
    pub fn put_semantic(&mut self, semantic_var_id: semantic::VarId, var: VariableId) {
        self.semantics.introduce(MemberPath::Var(semantic_var_id), var);
        self.changed_member_paths.insert(MemberPath::Var(semantic_var_id));
    }

    pub fn update_ref(
        &mut self,
        ctx: &mut LoweringContext<'_, '_>,
        member_path: &ExprVarMemberPath,
        var: VariableId,
    ) {
        let location = ctx.get_location(member_path.stable_ptr().untyped());
        self.update_ref_raw(ctx, member_path.into(), var, location);
    }

    pub fn update_ref_raw(
        &mut self,
        ctx: &mut LoweringContext<'_, '_>,
        member_path: MemberPath,
        var: VariableId,
        location: LocationId,
    ) {
        self.semantics.update(
            BlockStructRecomposer { statements: &mut self.statements, ctx, location },
            &member_path,
            var,
        );
        self.changed_member_paths.insert(member_path);
    }

    pub fn get_ref(
        &mut self,
        ctx: &mut LoweringContext<'_, '_>,
        member_path: &ExprVarMemberPath,
    ) -> Option<VarUsage> {
        let location = ctx.get_location(member_path.stable_ptr().untyped());
        self.get_ref_raw(ctx, &member_path.into(), location)
    }

    pub fn get_ref_raw(
        &mut self,
        ctx: &mut LoweringContext<'_, '_>,
        member_path: &MemberPath,
        location: LocationId,
    ) -> Option<VarUsage> {
        self.semantics
            .get(
                BlockStructRecomposer { statements: &mut self.statements, ctx, location },
                member_path,
            )
            .map(|var_id| VarUsage { var_id, location })
    }

    /// Gets the type of a semantic variable.
    pub fn get_ty(
        &mut self,
        ctx: &mut LoweringContext<'_, '_>,
        member_path: &MemberPath,
    ) -> semantic::TypeId {
        match member_path {
            MemberPath::Var(var) => ctx.semantic_defs[var].ty(),
            MemberPath::Member { member_id, concrete_struct_id, .. } => {
                ctx.db.concrete_struct_members(*concrete_struct_id).unwrap()
                    [&member_id.name(ctx.db.upcast())]
                    .ty
            }
        }
    }

    /// Adds a statement to the block.
    pub fn push_statement(&mut self, statement: Statement) {
        self.statements.push_statement(statement);
    }

    /// Ends a block with an unreachable match.
    pub fn unreachable_match(self, ctx: &mut LoweringContext<'_, '_>, match_info: MatchInfo) {
        self.finalize(ctx, FlatBlockEnd::Match { info: match_info });
    }

    /// Ends a block with Panic.
    pub fn panic(self, ctx: &mut LoweringContext<'_, '_>, data: VarUsage) -> Maybe<()> {
        self.finalize(ctx, FlatBlockEnd::Panic(data));
        Ok(())
    }

    /// Ends a block with Callsite.
    pub fn goto_callsite(self, expr: Option<VarUsage>) -> SealedBlockBuilder {
        SealedBlockBuilder::GotoCallsite { builder: self, expr }
    }

    /// Ends a block with Return.
    pub fn ret(
        mut self,
        ctx: &mut LoweringContext<'_, '_>,
        expr: VarUsage,
        location: LocationId,
    ) -> Maybe<()> {
        let refs = ctx
            .signature
            .extra_rets
            .clone()
            .iter()
            .map(|member_path| self.get_ref(ctx, member_path))
            .collect::<Option<Vec<_>>>()
            .ok_or_else(|| {
                ctx.diagnostics.report_by_location(
                    location.get(ctx.db),
                    LoweringDiagnosticKind::UnexpectedError,
                )
            })?;

        self.finalize(ctx, FlatBlockEnd::Return(chain!(refs, [expr]).collect(), location));
        Ok(())
    }

    /// Ends a block with known ending information. Used by [SealedBlockBuilder].
    pub fn finalize(self, ctx: &mut LoweringContext<'_, '_>, end: FlatBlockEnd) {
        let block = FlatBlock { statements: self.statements.statements, end };
        ctx.blocks.set_block(self.block_id, block);
    }

    /// Merges the sealed blocks and ends the block with a match-end.
    /// Replaces `self` with a sibling builder.
    pub fn merge_and_end_with_match(
        &mut self,
        ctx: &mut LoweringContext<'_, '_>,
        match_info: MatchInfo,
        sealed_blocks: Vec<SealedBlockBuilder>,
        location: LocationId,
    ) -> LoweringResult<LoweredExpr> {
        let Some((merged_expr, following_block)) = self.merge_sealed(ctx, sealed_blocks, location)
        else {
            return Err(LoweringFlowError::Match(match_info));
        };

        let new_scope = self.sibling_block_builder(following_block);
        let prev_scope = std::mem::replace(self, new_scope);
        prev_scope.finalize(ctx, FlatBlockEnd::Match { info: match_info });
        Ok(merged_expr)
    }

    /// Merges sibling sealed blocks.
    /// If there are reachable blocks, returns the converged expression of the blocks, usable at the
    /// calling builder, and the following block ID.
    /// Otherwise, returns None.
    fn merge_sealed(
        &mut self,
        ctx: &mut LoweringContext<'_, '_>,
        sealed_blocks: Vec<SealedBlockBuilder>,
        location: LocationId,
    ) -> Option<(LoweredExpr, BlockId)> {
        // TODO(spapini): When adding Gotos, include the callsite target in the required information
        // to merge.
        // TODO(spapini): Don't remap if we have a single reachable branch.

        let mut semantic_remapping = SemanticRemapping::default();
        let mut n_reachable_blocks = 0;

        // Remap Variables from all blocks.
        for sealed_block in &sealed_blocks {
            let SealedBlockBuilder::GotoCallsite { builder: subscope, expr } = sealed_block else {
                continue;
            };
            n_reachable_blocks += 1;
            if let Some(var_usage) = expr {
                semantic_remapping.expr.get_or_insert_with(|| {
                    let var = ctx.variables[var_usage.var_id].clone();
                    ctx.variables.variables.alloc(var)
                });
            }
            for member_path in subscope.changed_member_paths.iter() {
                let Some(containing_member_path) =
                    self.semantics.topmost_mapped_containing_member_path(member_path.clone())
                else {
                    // This variable is local to the subscope.
                    continue;
                };
                // Consider the topmost used(==mapped) member path that contains the changed member,
                // as it might be needed in the merge site.
                let mut queue = vec![containing_member_path];
                while let Some(v) = queue.pop() {
                    // If we reached the original member path, it needs to be restructured - so no
                    // need to continue recursively.
                    if v != *member_path {
                        // If it is scattered - add its members instead of itself, to avoid a
                        // possibly unnecessary restructuring.
                        if let Some(members) = self.semantics.get_scattered_members(&v) {
                            queue.extend(members);
                            continue;
                        }
                    }
                    // Actually adding to the remapping.
                    semantic_remapping.member_path_value.entry(v.clone()).or_insert_with(|| {
                        let ty = self.get_ty(ctx, &v);
                        ctx.new_var(VarRequest { ty, location })
                    });
                }
            }
        }

        // Prune parents from semantic_remapping.
        for member_path in semantic_remapping.member_path_value.keys().cloned().collect_vec() {
            let mut current = member_path.clone();
            while let MemberPath::Member { parent, .. } = current {
                if semantic_remapping.member_path_value.contains_key(&*parent) {
                    semantic_remapping.member_path_value.swap_remove(&member_path);
                }
                current = *parent;
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

        // Apply remapping on builder.
        for (semantic, var) in semantic_remapping.member_path_value {
            self.update_ref_raw(ctx, semantic, var, location);
        }

        let expr = match semantic_remapping.expr {
            Some(var_id) => LoweredExpr::AtVariable(VarUsage { var_id, location }),
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
    member_path_value: OrderedHashMap<MemberPath, VariableId>,
}

/// A sealed BlockBuilder, ready to be merged with sibling blocks to end the block.
#[allow(clippy::large_enum_variant)]
pub enum SealedBlockBuilder {
    /// Block should end by goto callsite. `expr` may be None for blocks that return the unit type.
    GotoCallsite { builder: BlockBuilder, expr: Option<VarUsage> },
    /// Block end is already known.
    Ends(BlockId),
}
impl SealedBlockBuilder {
    /// Finalizes a non-finalized block, given the semantic remapping of variables and the target
    /// block to jump to.
    fn finalize(
        self,
        ctx: &mut LoweringContext<'_, '_>,
        target: BlockId,
        semantic_remapping: &SemanticRemapping,
        location: LocationId,
    ) {
        if let SealedBlockBuilder::GotoCallsite { mut builder, expr } = self {
            let mut remapping = VarRemapping::default();
            // Since SemanticRemapping should have unique variable ids, these asserts will pass.
            for (semantic, remapped_var) in semantic_remapping.member_path_value.iter() {
                assert!(
                    remapping
                        .insert(
                            *remapped_var,
                            builder.get_ref_raw(ctx, semantic, location).unwrap()
                        )
                        .is_none()
                );
            }
            if let Some(remapped_var) = semantic_remapping.expr {
                let var_usage = expr.unwrap_or_else(|| {
                    LoweredExpr::Tuple {
                        exprs: vec![],
                        location: ctx.variables[remapped_var].location,
                    }
                    .as_var_usage(ctx, &mut builder)
                    .unwrap()
                });
                assert!(remapping.insert(remapped_var, var_usage).is_none());
            }

            builder.finalize(ctx, FlatBlockEnd::Goto(target, remapping));
        }
    }
}

struct BlockStructRecomposer<'a, 'b, 'c> {
    statements: &'a mut StatementsBuilder,
    ctx: &'a mut LoweringContext<'b, 'c>,
    location: LocationId,
}
impl<'a, 'b, 'c> StructRecomposer for BlockStructRecomposer<'a, 'b, 'c> {
    fn deconstruct(
        &mut self,
        concrete_struct_id: semantic::ConcreteStructId,
        value: VariableId,
    ) -> OrderedHashMap<MemberId, VariableId> {
        let members = self.ctx.db.concrete_struct_members(concrete_struct_id).unwrap();
        let members = members.values().collect_vec();
        let member_ids = members.iter().map(|m| m.id);

        let location = self.ctx.variables[value].location;
        let var_reqs =
            members.iter().map(|member| VarRequest { ty: member.ty, location }).collect();
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
        // TODO(ilya): Is using the `self.location` correct here?
        generators::StructConstruct {
            inputs: members
                .into_iter()
                .map(|var_id| VarUsage { var_id, location: self.location })
                .collect_vec(),
            ty,
            location: self.location,
        }
        .add(self.ctx, self.statements)
        .var_id
    }

    fn var_ty(&self, var: VariableId) -> semantic::TypeId {
        self.ctx.variables[var].ty
    }

    fn db(&self) -> &dyn LoweringGroup {
        self.ctx.db
    }
}
