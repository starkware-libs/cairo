use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::items::function_with_body::FunctionWithBodySemantic;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use itertools::{Itertools, chain, zip_eq};
use salsa::Database;
use semantic::TypeId;

use crate::blocks::Blocks;
use crate::db::{ConcreteSCCRepresentative, LoweringGroup};
use crate::ids::{ConcreteFunctionWithBodyId, FunctionId, FunctionLongId, LocationId};
use crate::{
    BlockEnd, BlockId, DependencyType, Lowered, LoweringStage, MatchArm, MatchInfo, Statement,
    VarUsage, Variable, VariableArena,
};

struct Context<'db, 'a> {
    db: &'db dyn Database,
    lowered: &'a mut Lowered<'db>,
    implicit_index: UnorderedHashMap<TypeId<'db>, usize>,
    implicits_tys: Vec<TypeId<'db>>,
    implicit_vars_for_block: UnorderedHashMap<BlockId, Vec<VarUsage<'db>>>,
    visited: UnorderedHashSet<BlockId>,
    location: LocationId<'db>,
}

/// Lowering phase that adds implicits.
pub fn lower_implicits<'db>(
    db: &'db dyn Database,
    function_id: ConcreteFunctionWithBodyId<'db>,
    lowered: &mut Lowered<'db>,
) {
    if let Err(diag_added) = inner_lower_implicits(db, function_id, lowered) {
        lowered.blocks = Blocks::new_errored(diag_added);
    }
}

/// Similar to lower_implicits, but uses Maybe<> for convenience.
pub fn inner_lower_implicits<'db>(
    db: &'db dyn Database,
    function_id: ConcreteFunctionWithBodyId<'db>,
    lowered: &mut Lowered<'db>,
) -> Maybe<()> {
    let semantic_function = function_id.base_semantic_function(db).function_with_body_id(db);
    let location = LocationId::from_stable_location(
        db,
        StableLocation::new(semantic_function.untyped_stable_ptr(db)),
    );
    lowered.blocks.has_root()?;
    let root_block_id = BlockId::root();

    let implicits_tys = db.function_with_body_implicits(function_id)?;

    let implicit_index = implicits_tys.iter().enumerate().map(|(i, ty)| (*ty, i)).collect();
    let mut ctx = Context {
        db,
        lowered,
        implicit_index,
        implicits_tys,
        implicit_vars_for_block: Default::default(),
        visited: Default::default(),
        location,
    };

    // Start from root block.
    lower_function_blocks_implicits(&mut ctx, root_block_id)?;

    // Introduce new input variables in the root block.
    let implicit_vars = &ctx.implicit_vars_for_block[&root_block_id];
    ctx.lowered.parameters.splice(0..0, implicit_vars.iter().map(|var_usage| var_usage.var_id));

    Ok(())
}

/// Allocates and returns new variables with usage location for each of the current function's
/// implicits.
fn alloc_implicits<'db>(
    db: &'db dyn Database,
    variables: &mut VariableArena<'db>,
    implicits_tys: &[TypeId<'db>],
    location: LocationId<'db>,
) -> Vec<VarUsage<'db>> {
    implicits_tys
        .iter()
        .copied()
        .map(|ty| VarUsage {
            var_id: variables.alloc(Variable::with_default_context(db, ty, location)),
            location,
        })
        .collect_vec()
}

/// Returns the implicits that are used in the statements of a block.
fn block_body_implicits<'db>(
    ctx: &mut Context<'db, '_>,
    block_id: BlockId,
) -> Result<Vec<VarUsage<'db>>, cairo_lang_diagnostics::DiagnosticAdded> {
    let mut implicits = ctx
        .implicit_vars_for_block
        .entry(block_id)
        .or_insert_with(|| {
            alloc_implicits(
                ctx.db,
                &mut ctx.lowered.variables,
                &ctx.implicits_tys,
                ctx.location.with_auto_generation_note(ctx.db, "implicits"),
            )
        })
        .clone();
    let require_implicits_libfunc_id = semantic::corelib::internal_require_implicit(ctx.db);
    let mut remove = vec![];
    for (i, statement) in ctx.lowered.blocks[block_id].statements.iter_mut().enumerate() {
        if let Statement::Call(stmt) = statement {
            if matches!(
                stmt.function.long(ctx.db),
                FunctionLongId::Semantic(func_id)
                    if func_id.get_concrete(ctx.db).generic_function == require_implicits_libfunc_id
            ) {
                remove.push(i);
                continue;
            }
            let callee_implicits = ctx.db.function_implicits(stmt.function)?;
            let location = stmt.location.with_auto_generation_note(ctx.db, "implicits");

            let indices = callee_implicits.iter().map(|ty| ctx.implicit_index[ty]).collect_vec();

            let implicit_input_vars = indices.iter().map(|i| implicits[*i]);
            stmt.inputs.splice(0..0, implicit_input_vars);
            let implicit_output_vars = callee_implicits
                .iter()
                .copied()
                .map(|ty| {
                    ctx.lowered
                        .variables
                        .alloc(Variable::with_default_context(ctx.db, ty, location))
                })
                .collect_vec();
            for (i, var) in zip_eq(indices, implicit_output_vars.iter()) {
                implicits[i] =
                    VarUsage { var_id: *var, location: ctx.lowered.variables[*var].location };
            }
            stmt.outputs.splice(0..0, implicit_output_vars);
        }
    }
    for i in remove.into_iter().rev() {
        ctx.lowered.blocks[block_id].statements.remove(i);
    }
    Ok(implicits)
}

/// Finds the implicits for a function's blocks starting from the root.
fn lower_function_blocks_implicits<'db>(
    ctx: &mut Context<'db, '_>,
    root_block_id: BlockId,
) -> Maybe<()> {
    let mut blocks_to_visit = vec![root_block_id];
    while let Some(block_id) = blocks_to_visit.pop() {
        if !ctx.visited.insert(block_id) {
            continue;
        }
        let implicits = block_body_implicits(ctx, block_id)?;
        // End.
        match &mut ctx.lowered.blocks[block_id].end {
            BlockEnd::Return(rets, _location) => {
                rets.splice(0..0, implicits.iter().cloned());
            }
            BlockEnd::Panic(_) => {
                unreachable!("Panics should have been stripped in a previous phase.")
            }
            BlockEnd::Goto(block_id, remapping) => {
                let target_implicits = ctx
                    .implicit_vars_for_block
                    .entry(*block_id)
                    .or_insert_with(|| {
                        alloc_implicits(
                            ctx.db,
                            &mut ctx.lowered.variables,
                            &ctx.implicits_tys,
                            ctx.location,
                        )
                    })
                    .clone();
                let old_remapping = std::mem::take(&mut remapping.remapping);
                remapping.remapping = chain!(
                    zip_eq(
                        target_implicits.into_iter().map(|var_usage| var_usage.var_id),
                        implicits
                    ),
                    old_remapping
                )
                .collect();
                blocks_to_visit.push(*block_id);
            }
            BlockEnd::Match { info } => {
                blocks_to_visit.extend(info.arms().iter().rev().map(|a| a.block_id));
                match info {
                    MatchInfo::Enum(_) | MatchInfo::Value(_) => {
                        for MatchArm { arm_selector: _, block_id, var_ids: _ } in info.arms() {
                            assert!(
                                ctx.implicit_vars_for_block
                                    .insert(*block_id, implicits.clone())
                                    .is_none(),
                                "Multiple jumps to arm blocks are not allowed."
                            );
                        }
                    }
                    MatchInfo::Extern(stmt) => {
                        let callee_implicits = ctx.db.function_implicits(stmt.function)?;

                        let indices =
                            callee_implicits.iter().map(|ty| ctx.implicit_index[ty]).collect_vec();

                        let implicit_input_vars = indices.iter().map(|i| implicits[*i]);
                        stmt.inputs.splice(0..0, implicit_input_vars);
                        let location = stmt.location.with_auto_generation_note(ctx.db, "implicits");

                        for MatchArm { arm_selector: _, block_id, var_ids } in stmt.arms.iter_mut()
                        {
                            let mut arm_implicits = implicits.clone();
                            let mut implicit_input_vars = vec![];
                            for ty in callee_implicits.iter().copied() {
                                let var = ctx
                                    .lowered
                                    .variables
                                    .alloc(Variable::with_default_context(ctx.db, ty, location));
                                implicit_input_vars.push(var);
                                let implicit_index = ctx.implicit_index[&ty];
                                arm_implicits[implicit_index] = VarUsage { var_id: var, location };
                            }
                            assert!(
                                ctx.implicit_vars_for_block
                                    .insert(*block_id, arm_implicits)
                                    .is_none(),
                                "Multiple jumps to arm blocks are not allowed."
                            );

                            var_ids.splice(0..0, implicit_input_vars);
                        }
                    }
                }
            }
            BlockEnd::NotSet => unreachable!(),
        }
    }
    Ok(())
}

// =========== Query implementations ===========

/// Query implementation of [crate::db::LoweringGroup::function_implicits].
#[salsa::tracked]
pub fn function_implicits<'db>(
    db: &'db dyn Database,
    function: FunctionId<'db>,
) -> Maybe<Vec<TypeId<'db>>> {
    if let Some(body) = function.body(db)? {
        return db.function_with_body_implicits(body);
    }
    Ok(function.signature(db)?.implicits)
}

/// A trait to add helper methods in [LoweringGroup].
pub trait FunctionImplicitsTrait<'db>: Database {
    /// Returns all the implicits used by a [ConcreteFunctionWithBodyId].
    fn function_with_body_implicits(
        &'db self,
        function: ConcreteFunctionWithBodyId<'db>,
    ) -> Maybe<Vec<TypeId<'db>>> {
        let db: &'db dyn Database = self.as_dyn_database();
        let scc_representative = db.lowered_scc_representative(
            function,
            DependencyType::Call,
            LoweringStage::PostBaseline,
        );
        let mut implicits = scc_implicits(db, scc_representative)?;

        let precedence = db.function_declaration_implicit_precedence(
            function.base_semantic_function(db).function_with_body_id(db),
        )?;
        precedence.apply(&mut implicits, db);

        Ok(implicits)
    }
}
impl<'db, T: Database + ?Sized> FunctionImplicitsTrait<'db> for T {}

/// Returns all the implicits used by a strongly connected component of functions.
fn scc_implicits<'db>(
    db: &'db dyn Database,
    scc: ConcreteSCCRepresentative<'db>,
) -> Maybe<Vec<TypeId<'db>>> {
    scc_implicits_tracked(db, scc.0)
}

/// Tracked implementation of [scc_implicits].
#[salsa::tracked]
fn scc_implicits_tracked<'db>(
    db: &'db dyn Database,
    rep: ConcreteFunctionWithBodyId<'db>,
) -> Maybe<Vec<TypeId<'db>>> {
    let scc_functions = db.lowered_scc(rep, DependencyType::Call, LoweringStage::PostBaseline);
    let mut all_implicits = OrderedHashSet::<_>::default();
    for function in scc_functions {
        // Add the function's explicit implicits.
        all_implicits.extend(function.function_id(db)?.signature(db)?.implicits);
        // For each direct callee, add its implicits.
        let direct_callees =
            db.lowered_direct_callees(function, DependencyType::Call, LoweringStage::PostBaseline)?;
        for direct_callee in direct_callees {
            if let Some(callee_body) = direct_callee.body(db)? {
                let callee_scc = db.lowered_scc_representative(
                    callee_body,
                    DependencyType::Call,
                    LoweringStage::PostBaseline,
                );
                if callee_scc.0 != rep {
                    all_implicits.extend(scc_implicits(db, callee_scc)?);
                }
            } else {
                all_implicits.extend(direct_callee.signature(db)?.implicits);
            }
        }
    }
    Ok(all_implicits.into_iter().collect())
}
