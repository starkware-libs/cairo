use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{MemberId, NamedLanguageElementId};
use cairo_lang_diagnostics::{DiagnosticNote, Maybe};
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::expr::fmt::ExprFormatter;
use cairo_lang_semantic::items::structure::StructSemantic;
use cairo_lang_semantic::types::{peel_snapshots, wrap_in_snapshots};
use cairo_lang_semantic::usage::{MemberPath, Usage};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{Intern, require};
use itertools::{Itertools, chain, zip_eq};
use semantic::{ConcreteTypeId, ExprVarMemberPath, TypeLongId};

use super::context::{LoweredExpr, LoweringContext, LoweringFlowError, LoweringResult, VarRequest};
use super::generators;
use super::generators::StatementsBuilder;
use super::refs::{AssembleValueError, MovedVar, SemanticLoweringMapping, merge_semantics};
use crate::diagnostic::{LoweringDiagnosticKind, LoweringDiagnosticsBuilder};
use crate::ids::LocationId;
use crate::lower::refs::ClosureInfo;
use crate::{Block, BlockEnd, BlockId, MatchInfo, Statement, VarRemapping, VarUsage, VariableId};

/// Block builder, describing its current state.
#[derive(Clone)]
pub struct BlockBuilder<'db> {
    /// A store for semantic variables, owning their OwnedVariable instances.
    semantics: SemanticLoweringMapping<'db>,
    /// Current sequence of lowered statements emitted.
    pub statements: StatementsBuilder<'db>,
    /// The block id to use for this block when it's finalized.
    pub block_id: BlockId,
}
impl<'db> BlockBuilder<'db> {
    /// Creates a new [BlockBuilder] for the root block of a function body.
    pub fn root(block_id: BlockId) -> Self {
        BlockBuilder { semantics: Default::default(), statements: Default::default(), block_id }
    }

    /// Creates a [BlockBuilder] for a subscope.
    pub fn child_block_builder(&self, block_id: BlockId) -> BlockBuilder<'db> {
        BlockBuilder { semantics: self.semantics.clone(), statements: Default::default(), block_id }
    }

    /// Creates a [BlockBuilder] for a sibling builder. This is used when an original block is split
    /// (e.g. after a match statement) to add the ability to 'goto' to after-the-block.
    pub fn sibling_block_builder(&self, block_id: BlockId) -> BlockBuilder<'db> {
        BlockBuilder { semantics: self.semantics.clone(), statements: Default::default(), block_id }
    }

    /// Binds a semantic variable to a lowered variable.
    pub fn put_semantic(&mut self, semantic_var_id: semantic::VarId<'db>, var: VariableId) {
        self.semantics.introduce(MemberPath::Var(semantic_var_id), var);
    }

    pub fn update_ref(
        &mut self,
        ctx: &mut LoweringContext<'db, '_>,
        member_path: &ExprVarMemberPath<'db>,
        var: VariableId,
    ) {
        let location = ctx.get_location(member_path.stable_ptr().untyped());
        self.update_ref_raw(ctx, member_path.into(), var, location);
    }

    pub fn update_ref_raw(
        &mut self,
        ctx: &mut LoweringContext<'db, '_>,
        member_path: MemberPath<'db>,
        var: VariableId,
        location: LocationId<'db>,
    ) {
        self.semantics.update(
            &mut BlockStructRecomposer { statements: &mut self.statements, ctx, location },
            &member_path,
            var,
        );
    }

    /// Returns the [VarUsage] for a given `member_path`.
    ///
    /// If the variable is not copyable, it is marked as [MovedVar].
    ///
    /// If the `member_path` is not in the semantics, returns `None`.
    ///
    /// If the variable was marked as [MovedVar], an error will be reported and a dummy variable
    /// will be returned.
    pub fn get_ref(
        &mut self,
        ctx: &mut LoweringContext<'db, '_>,
        member_path: &ExprVarMemberPath<'db>,
    ) -> Option<VarUsage<'db>> {
        let location = ctx.get_location(member_path.stable_ptr().untyped());
        self.get_ref_raw(ctx, &member_path.into(), location, None)
    }

    /// Returns the [VarUsage] for a given `member_path`.
    ///
    /// If `member_path` is not in the semantics, or the resulting [VarUsage] does not have the
    /// expected type, returns `None`.
    ///
    /// If the variable is not copyable, it is marked as [MovedVar].
    ///
    /// If the variable was marked as [MovedVar], an error will be reported and a dummy variable
    /// will be returned.
    pub fn get_ref_of_type(
        &mut self,
        ctx: &mut LoweringContext<'db, '_>,
        member_path: &ExprVarMemberPath<'db>,
        expected_ty: semantic::TypeId<'db>,
    ) -> Option<VarUsage<'db>> {
        let location = ctx.get_location(member_path.stable_ptr().untyped());
        self.get_ref_raw(ctx, &member_path.into(), location, Some(expected_ty))
    }

    /// Returns the [VarUsage] for a given `member_path`.
    ///
    /// If `member_path` is not in the semantics, or the resulting [VarUsage] does not have the
    /// expected type (if specified), returns `None`.
    ///
    /// If the variable is not copyable, it is marked as [MovedVar].
    ///
    /// If the variable was marked as [MovedVar], an error will be reported and a dummy variable
    /// will be returned.
    pub fn get_ref_raw(
        &mut self,
        ctx: &mut LoweringContext<'db, '_>,
        member_path: &MemberPath<'db>,
        location: LocationId<'db>,
        expected_ty: Option<semantic::TypeId<'db>>,
    ) -> Option<VarUsage<'db>> {
        // Fetch the variable from the semantics.
        let res = self.semantics.get(
            BlockStructRecomposer { statements: &mut self.statements, ctx, location },
            member_path,
        );

        match res {
            Ok(var_id) => {
                // If `expected_ty` is given, check that the returned variable has that type.
                if let Some(expected_ty) = expected_ty
                    && ctx.variables[var_id].ty != expected_ty
                {
                    return None;
                }

                // If the variable is not copyable, mark it as [MovedVar].
                let var = &ctx.variables[var_id];
                let copyable = var.info.copyable.clone();
                let ty = var.ty;

                if let Err(inference_error) = copyable {
                    self.semantics.mark_as_used(
                        BlockStructRecomposer { statements: &mut self.statements, ctx, location },
                        member_path,
                        MovedVar { ty, inference_error, last_use_location: location },
                    );
                }

                Some(VarUsage { var_id, location })
            }
            Err(AssembleValueError::Moved(MovedVar { ty, inference_error, last_use_location })) => {
                // If the variable was already moved, report an error.
                let diag_location = location
                    .long(ctx.db)
                    .clone()
                    .add_note_with_location(
                        ctx.db,
                        "variable was previously used here",
                        last_use_location,
                    )
                    .with_note(DiagnosticNote::text_only(inference_error.format(ctx.db)));
                ctx.diagnostics.report_by_location(
                    diag_location,
                    LoweringDiagnosticKind::VariableMoved { inference_error },
                );

                // Create and return a dummy variable.
                let var_id = ctx.new_var(VarRequest { ty, location });
                Some(VarUsage { var_id, location })
            }
            Err(AssembleValueError::Missing) => None,
        }
    }

    /// Returns the [VarUsage] for a given `member_path` for the purpose of remapping.
    ///
    /// Returns `None` if the variable was marked as [MovedVar].
    pub fn get_ref_for_remapping(
        &mut self,
        ctx: &mut LoweringContext<'db, '_>,
        member_path: &MemberPath<'db>,
        location: LocationId<'db>,
    ) -> Option<VarUsage<'db>> {
        let res = self.semantics.get(
            BlockStructRecomposer { statements: &mut self.statements, ctx, location },
            member_path,
        );

        match res {
            Ok(var_id) => Some(VarUsage { var_id, location }),
            Err(AssembleValueError::Moved { .. }) => None,
            Err(AssembleValueError::Missing) => {
                unreachable!();
            }
        }
    }

    /// Introduces a semantic variable as the representation of the given member path.
    pub fn introduce(&mut self, member_path: MemberPath<'db>, var: VariableId) {
        self.semantics.introduce(member_path, var);
    }

    /// Gets the reference of a snapshot of semantic variable, possibly by deconstructing its
    /// parents.
    pub fn get_snap_ref(
        &mut self,
        ctx: &mut LoweringContext<'db, '_>,
        member_path: &ExprVarMemberPath<'db>,
    ) -> Option<VarUsage<'db>> {
        let location = ctx.get_location(member_path.stable_ptr().untyped());
        if let Some(var_id) = ctx.snapped_semantics.get::<MemberPath<'_>>(&member_path.into()) {
            return Some(VarUsage { var_id: *var_id, location });
        }
        let ExprVarMemberPath::Member { parent, member_id, concrete_struct_id, .. } = member_path
        else {
            return None;
        };
        let parent_var = self.get_snap_ref(ctx, parent)?;
        let members = ctx.db.concrete_struct_members(*concrete_struct_id).ok()?;
        let (parent_number_of_snapshots, _) =
            peel_snapshots(ctx.db, ctx.variables[parent_var.var_id].ty);
        let member_idx = members.iter().position(|(_, member)| member.id == *member_id)?;
        Some(
            generators::StructMemberAccess {
                input: parent_var,
                member_tys: members
                    .iter()
                    .map(|(_, member)| {
                        wrap_in_snapshots(ctx.db, member.ty, parent_number_of_snapshots)
                    })
                    .collect(),
                member_idx,
                location,
            }
            .add(ctx, &mut self.statements),
        )
    }

    /// Adds a statement to the block.
    pub fn push_statement(&mut self, statement: Statement<'db>) {
        self.statements.push_statement(statement);
    }

    /// Ends a block with an unreachable match.
    pub fn unreachable_match(self, ctx: &mut LoweringContext<'db, '_>, match_info: MatchInfo<'db>) {
        self.finalize(ctx, BlockEnd::Match { info: match_info });
    }

    /// Ends a block with Panic.
    pub fn panic(self, ctx: &mut LoweringContext<'db, '_>, data: VarUsage<'db>) -> Maybe<()> {
        self.finalize(ctx, BlockEnd::Panic(data));
        Ok(())
    }

    /// Ends a block with Callsite.
    pub fn goto_callsite(self, expr: Option<VarUsage<'db>>) -> SealedBlockBuilder<'db> {
        Some(SealedGotoCallsite { builder: self, expr })
    }

    /// Ends a block with Return.
    pub fn ret(
        mut self,
        ctx: &mut LoweringContext<'db, '_>,
        expr: VarUsage<'db>,
        location: LocationId<'db>,
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
                    location.long(ctx.db).clone(),
                    LoweringDiagnosticKind::UnexpectedError,
                )
            })?;

        self.finalize(ctx, BlockEnd::Return(chain!(refs, [expr]).collect(), location));
        Ok(())
    }

    /// Ends a block with known ending information. Used by [SealedBlockBuilder].
    pub fn finalize(self, ctx: &mut LoweringContext<'db, '_>, end: BlockEnd<'db>) {
        let block = Block { statements: self.statements.statements, end };
        ctx.blocks.set_block(self.block_id, block);
    }

    /// Merges the sealed blocks and ends the block with a match-end.
    /// Replaces `self` with a sibling builder.
    pub fn merge_and_end_with_match(
        &mut self,
        ctx: &mut LoweringContext<'db, '_>,
        match_info: MatchInfo<'db>,
        sealed_blocks: Vec<SealedBlockBuilder<'db>>,
        location: LocationId<'db>,
    ) -> LoweringResult<'db, LoweredExpr<'db>> {
        let sealed_blocks = sealed_blocks.into_iter().flatten().collect_vec();

        let Some((new_scope, merged_expr)) =
            merge_sealed_block_builders(ctx, sealed_blocks, location)
        else {
            return Err(LoweringFlowError::Match(match_info));
        };
        let prev_scope = std::mem::replace(self, new_scope);
        prev_scope.finalize(ctx, BlockEnd::Match { info: match_info });
        Ok(merged_expr)
    }

    /// Captures the variable in a usage associated with a closure.
    ///
    /// Returns the variable usage of the closure.
    pub fn capture(
        &mut self,
        ctx: &mut LoweringContext<'db, '_>,
        usage: Usage<'db>,
        expr: &semantic::ExprClosure<'db>,
    ) -> (VarUsage<'db>, ClosureInfo<'db>) {
        let location = ctx.get_location(expr.stable_ptr.untyped());

        let inputs = chain!(
            usage.usage.values().map(|expr| { LoweredExpr::MemberPath(expr.clone(), location) }),
            usage.snap_usage.values().map(|expr| {
                LoweredExpr::Snapshot {
                    expr: Box::new(LoweredExpr::MemberPath(expr.clone(), location)),
                    location,
                }
            })
        )
        .map(|expr| expr.as_var_usage(ctx, self).unwrap())
        .collect_vec();

        let members: OrderedHashMap<MemberPath<'_>, semantic::TypeId<'_>> =
            chain!(usage.usage.values(), usage.changes.values())
                .map(|expr| (expr.into(), expr.ty()))
                .collect();

        let snapshots = usage
            .snap_usage
            .keys()
            .cloned()
            .zip(
                inputs
                    .iter()
                    .skip(members.len())
                    .map(|var_usage| ctx.variables.variables[var_usage.var_id].ty),
            )
            .collect();

        let TypeLongId::Closure(closure_id) = expr.ty.long(ctx.db) else {
            unreachable!("Closure Expr should have a Closure type.");
        };

        // Assert that the closure type matches the input we pass to it.
        assert!(
            closure_id
                .captured_types
                .iter()
                .eq(inputs.iter().map(|var_usage| &ctx.variables[var_usage.var_id].ty))
        );

        let var_usage =
            generators::StructConstruct { inputs: inputs.clone(), ty: expr.ty, location }
                .add(ctx, &mut self.statements);

        (var_usage, ClosureInfo { members, snapshots })
    }

    /// Destructures a closure.
    /// Invalidates the closure variable and returns the captured variables.
    pub fn destructure_closure(
        &mut self,
        ctx: &mut LoweringContext<'db, '_>,
        location: LocationId<'db>,
        closure_var: VariableId,
        closure_info: &ClosureInfo<'db>,
    ) -> Vec<VariableId> {
        self.semantics.destructure_closure(
            &mut BlockStructRecomposer { statements: &mut self.statements, ctx, location },
            closure_var,
            closure_info,
        )
    }
}

impl<'db> DebugWithDb<'db> for BlockBuilder<'db> {
    type Db = ExprFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &ExprFormatter<'db>) -> std::fmt::Result {
        writeln!(f, "block_id: {:?}", self.block_id)?;
        if !self.statements.statements.is_empty() {
            writeln!(f, "statements:")?;
            for statement in &self.statements.statements {
                writeln!(f, "  {statement:?}")?;
            }
        }
        writeln!(f, "semantics:")?;
        write!(f, "{}", indent::indent_all_with("  ", format!("{:?}", self.semantics.debug(db))))?;

        Ok(())
    }
}

/// Gets the type of a semantic variable.
fn get_ty<'db>(
    ctx: &LoweringContext<'db, '_>,
    member_path: &MemberPath<'db>,
) -> semantic::TypeId<'db> {
    match member_path {
        MemberPath::Var(var) => ctx.semantic_defs[var].ty(),
        MemberPath::Member { member_id, concrete_struct_id, .. } => {
            ctx.db.concrete_struct_members(*concrete_struct_id).unwrap()[&member_id.name(ctx.db)].ty
        }
    }
}

/// Remapping of lowered variables with more semantic information regarding what is the semantic
/// role of the lowered variables.
#[derive(Debug, Default)]
pub struct SemanticRemapping<'db> {
    expr: Option<VariableId>,
    member_path_value: OrderedHashMap<MemberPath<'db>, VariableId>,
}

/// Represents a sealed [BlockBuilder] that ends with returning (goto) to the callsite.
pub struct SealedGotoCallsite<'db> {
    /// The sealed builder.
    pub builder: BlockBuilder<'db>,
    /// The expression that is returned by the block to the callsite. Can be `None` if the block
    /// returns the unit type.
    pub expr: Option<VarUsage<'db>>,
}
impl<'db> SealedGotoCallsite<'db> {
    /// Finalizes a non-finalized block, given the semantic remapping of variables and the target
    /// block to jump to.
    fn finalize(
        self,
        ctx: &mut LoweringContext<'db, '_>,
        target: BlockId,
        semantic_remapping: &SemanticRemapping<'db>,
        location: LocationId<'db>,
    ) {
        let SealedGotoCallsite { mut builder, expr } = self;
        let mut remapping = VarRemapping::default();
        // Since SemanticRemapping should have unique variable ids, these asserts will pass.
        for (semantic, remapped_var) in semantic_remapping.member_path_value.iter() {
            // If the variable is not copyable and was moved before, do not remap it (it can no
            // longer be used).
            if let Some(var_usage) = builder.get_ref_for_remapping(ctx, semantic, location) {
                assert!(remapping.insert(*remapped_var, var_usage).is_none());
            }
        }
        if let Some(remapped_var) = semantic_remapping.expr {
            let var_usage = expr.unwrap_or_else(|| {
                LoweredExpr::Tuple { exprs: vec![], location: ctx.variables[remapped_var].location }
                    .as_var_usage(ctx, &mut builder)
                    .unwrap()
            });
            assert!(remapping.insert(remapped_var, var_usage).is_none());
        }

        builder.finalize(ctx, BlockEnd::Goto(target, remapping));
    }
}

/// A sealed [BlockBuilder], ready to be merged with sibling blocks to end the block.
pub type SealedBlockBuilder<'db> = Option<SealedGotoCallsite<'db>>;

pub struct BlockStructRecomposer<'a, 'b, 'db> {
    statements: &'a mut StatementsBuilder<'db>,
    pub ctx: &'a mut LoweringContext<'db, 'b>,
    location: LocationId<'db>,
}
impl<'db> BlockStructRecomposer<'_, '_, 'db> {
    pub fn deconstruct(
        &mut self,
        concrete_struct_id: semantic::ConcreteStructId<'db>,
        value: VariableId,
    ) -> OrderedHashMap<MemberId<'db>, VariableId> {
        let members = self.ctx.db.concrete_struct_members(concrete_struct_id).unwrap();
        let members = members.values().collect_vec();
        let member_ids = members.iter().map(|m| m.id);

        let member_values =
            self.deconstruct_by_types(value, members.iter().map(|member| member.ty));
        OrderedHashMap::from_iter(zip_eq(member_ids, member_values))
    }

    pub fn deconstruct_by_types(
        &mut self,
        value: VariableId,
        types: impl Iterator<Item = semantic::TypeId<'db>>,
    ) -> Vec<VariableId> {
        // We use the location of the variable being deconstructed for the members
        // to get a better location for variable not dropped errors.
        let location = self.ctx.variables[value].location;
        let var_reqs = types.map(|ty| VarRequest { ty, location }).collect_vec();

        generators::StructDestructure {
            input: VarUsage { var_id: value, location: self.location },
            var_reqs,
        }
        .add(self.ctx, self.statements)
    }

    pub fn reconstruct(
        &mut self,
        concrete_struct_id: semantic::ConcreteStructId<'db>,
        members: Vec<VariableId>,
    ) -> VariableId {
        let ty =
            TypeLongId::Concrete(ConcreteTypeId::Struct(concrete_struct_id)).intern(self.ctx.db);
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
}

/// Given a list of sealed block builders ([SealedGotoCallsite]), creates a new single block builder
/// and finalizes all the block builders with a [BlockEnd::Goto] to the new block.
///
/// The mapping from semantic variables to lowered variables in the new block follows these rules:
///
/// * Variables mapped to the same lowered variable across all input blocks are kept as-is.
/// * Local variables that appear in only a subset of the blocks are removed.
/// * Variables with different mappings across blocks are remapped to a new lowered variable.
pub fn merge_sealed_block_builders<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    sealed_blocks: Vec<SealedGotoCallsite<'db>>,
    location: LocationId<'db>,
) -> Option<(BlockBuilder<'db>, LoweredExpr<'db>)> {
    require(!sealed_blocks.is_empty())?;

    // Handle the expression returned by the block (if exists).
    let mut res_var: Option<VariableId> = None;

    for sealed_block in sealed_blocks.iter() {
        if let Some(var_usage) = sealed_block.expr {
            let var = ctx.variables[var_usage.var_id].clone();
            res_var = Some(ctx.variables.variables.alloc(var));
            break;
        }
    }

    let lowered_expr = if let Some(res_var) = res_var {
        LoweredExpr::AtVariable(VarUsage { var_id: res_var, location })
    } else {
        LoweredExpr::Tuple { exprs: vec![], location }
    };

    let merged_builder = merge_block_builders_inner(ctx, sealed_blocks, res_var, location);

    Some((merged_builder, lowered_expr))
}

/// Seals and merges the given [BlockBuilder]s into a single [BlockBuilder].
/// If only one parent builder is given, returns it as-is without sealing and creating a new block.
///
/// See [merge_sealed_block_builders] for more details.
pub fn merge_block_builders<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    parent_builders: Vec<BlockBuilder<'db>>,
    location: LocationId<'db>,
) -> BlockBuilder<'db> {
    // If there is only one parent builder, return it.
    if parent_builders.len() == 1 {
        return parent_builders.into_iter().next().unwrap();
    }

    let parent_builders = parent_builders
        .into_iter()
        .map(|builder| SealedGotoCallsite { builder, expr: None })
        .collect_vec();
    merge_block_builders_inner(ctx, parent_builders, None, location)
}

/// Helper function for [merge_sealed_block_builders] and [merge_block_builders].
fn merge_block_builders_inner<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    parent_sealed_blocks: Vec<SealedGotoCallsite<'db>>,
    res_expr: Option<VariableId>,
    location: LocationId<'db>,
) -> BlockBuilder<'db> {
    // Compute the merged semantics.

    // A map from [MemberPath] that requires a new lowered variable (due to remapping) to the
    // corresponding lowered variable.
    let mut member_path_value = OrderedHashMap::<MemberPath<'_>, VariableId>::default();

    let semantics = merge_semantics(
        parent_sealed_blocks.iter().map(|sealed_block| &sealed_block.builder.semantics),
        &mut |path| {
            let ty = get_ty(ctx, path);
            let var = ctx.new_var(VarRequest { ty, location });
            member_path_value.insert(path.clone(), var);
            var
        },
    );

    let semantic_remapping = SemanticRemapping { expr: res_expr, member_path_value };

    // Assign a new block id for the current node.
    let block_id = ctx.blocks.alloc_empty();

    // Finalize the intermediate blocks.
    for sealed_block in parent_sealed_blocks.into_iter() {
        sealed_block.finalize(ctx, block_id, &semantic_remapping, location);
    }

    // Create a new [BlockBuilder] with the merged `semantics`.
    BlockBuilder { semantics, statements: Default::default(), block_id }
}
