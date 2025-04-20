use std::collections::{HashMap, HashSet};

use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_utils::{LookupIntern, Upcast};
use itertools::{Itertools, chain, zip_eq};
use semantic::TypeId;

use crate::blocks::Blocks;
use crate::db::{ConcreteSCCRepresentative, LoweringGroup};
use crate::ids::{ConcreteFunctionWithBodyId, FunctionId, FunctionLongId, LocationId};
use crate::lower::context::{VarRequest, VariableAllocator};
use crate::{
    BlockId, DependencyType, FlatBlockEnd, FlatLowered, MatchArm, MatchInfo, Statement, VarUsage,
};

struct Context<'a> {
    db: &'a dyn LoweringGroup,
    variables: &'a mut VariableAllocator<'a>,
    lowered: &'a mut FlatLowered,
    implicit_index: HashMap<TypeId, usize>,
    implicits_tys: Vec<TypeId>,
    implicit_vars_for_block: HashMap<BlockId, Vec<VarUsage>>,
    visited: HashSet<BlockId>,
    location: LocationId,
}

/// Lowering phase that adds implicits.
pub fn lower_implicits(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
    lowered: &mut FlatLowered,
) {
    if let Err(diag_added) = inner_lower_implicits(db, function_id, lowered) {
        lowered.blocks = Blocks::new_errored(diag_added);
    }
}

/// Similar to lower_implicits, but uses Maybe<> for convenience.
pub fn inner_lower_implicits(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
    lowered: &mut FlatLowered,
) -> Maybe<()> {
    let semantic_function = function_id.function_with_body_id(db).base_semantic_function(db);
    let location = LocationId::from_stable_location(
        db,
        StableLocation::new(semantic_function.untyped_stable_ptr(db.upcast())),
    );
    lowered.blocks.has_root()?;
    let root_block_id = BlockId::root();

    let mut variables = VariableAllocator::new(
        db,
        function_id.function_with_body_id(db).base_semantic_function(db),
        lowered.variables.clone(),
    )?;

    let implicits_tys = db.function_with_body_implicits(function_id)?;

    let implicit_index =
        HashMap::from_iter(implicits_tys.iter().enumerate().map(|(i, ty)| (*ty, i)));
    let mut ctx = Context {
        db,
        variables: &mut variables,
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

    lowered.variables = std::mem::take(&mut ctx.variables.variables);

    Ok(())
}

/// Allocates and returns new variables with usage location for each of the current function's
/// implicits.
fn alloc_implicits(
    ctx: &mut VariableAllocator<'_>,
    implicits_tys: &[TypeId],
    location: LocationId,
) -> Vec<VarUsage> {
    implicits_tys
        .iter()
        .copied()
        .map(|ty| VarUsage { var_id: ctx.new_var(VarRequest { ty, location }), location })
        .collect_vec()
}

/// Returns the implicits that are used in the statements of a block.
fn block_body_implicits(
    ctx: &mut Context<'_>,
    block_id: BlockId,
) -> Result<Vec<VarUsage>, cairo_lang_diagnostics::DiagnosticAdded> {
    let mut implicits = ctx
        .implicit_vars_for_block
        .entry(block_id)
        .or_insert_with(|| {
            alloc_implicits(
                ctx.variables,
                &ctx.implicits_tys,
                ctx.location.with_auto_generation_note(ctx.db, "implicits"),
            )
        })
        .clone();
    let require_implicits_libfunc_id =
        semantic::corelib::internal_require_implicit(ctx.db.upcast());
    let mut remove = vec![];
    for (i, statement) in ctx.lowered.blocks[block_id].statements.iter_mut().enumerate() {
        if let Statement::Call(stmt) = statement {
            if matches!(
                stmt.function.lookup_intern(ctx.db),
                FunctionLongId::Semantic(func_id)
                    if func_id.get_concrete(ctx.db.upcast()).generic_function == require_implicits_libfunc_id
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
                .map(|ty| ctx.variables.new_var(VarRequest { ty, location }))
                .collect_vec();
            for (i, var) in zip_eq(indices, implicit_output_vars.iter()) {
                implicits[i] = VarUsage { var_id: *var, location: ctx.variables[*var].location };
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
fn lower_function_blocks_implicits(ctx: &mut Context<'_>, root_block_id: BlockId) -> Maybe<()> {
    let mut blocks_to_visit = vec![root_block_id];
    while let Some(block_id) = blocks_to_visit.pop() {
        if !ctx.visited.insert(block_id) {
            continue;
        }
        let implicits = block_body_implicits(ctx, block_id)?;
        // End.
        match &mut ctx.lowered.blocks[block_id].end {
            FlatBlockEnd::Return(rets, _location) => {
                rets.splice(0..0, implicits.iter().cloned());
            }
            FlatBlockEnd::Panic(_) => {
                unreachable!("Panics should have been stripped in a previous phase.")
            }
            FlatBlockEnd::Goto(block_id, remapping) => {
                let target_implicits = ctx
                    .implicit_vars_for_block
                    .entry(*block_id)
                    .or_insert_with(|| {
                        alloc_implicits(ctx.variables, &ctx.implicits_tys, ctx.location)
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
            FlatBlockEnd::Match { info } => {
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
                                let var = ctx.variables.new_var(VarRequest { ty, location });
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
            FlatBlockEnd::NotSet => unreachable!(),
        }
    }
    Ok(())
}

// =========== Query implementations ===========

/// Query implementation of [crate::db::LoweringGroup::function_implicits].
pub fn function_implicits(db: &dyn LoweringGroup, function: FunctionId) -> Maybe<Vec<TypeId>> {
    if let Some(body) = function.body(db.upcast())? {
        return db.function_with_body_implicits(body);
    }
    Ok(function.signature(db)?.implicits)
}

/// A trait to add helper methods in [LoweringGroup].
pub trait FunctionImplicitsTrait<'a>: Upcast<dyn LoweringGroup + 'a> {
    /// Returns all the implicits used by a [ConcreteFunctionWithBodyId].
    fn function_with_body_implicits(
        &self,
        function: ConcreteFunctionWithBodyId,
    ) -> Maybe<Vec<TypeId>> {
        let db: &dyn LoweringGroup = self.upcast();
        let semantic_db: &dyn SemanticGroup = db.upcast();
        let scc_representative = db
            .concrete_function_with_body_scc_inlined_representative(function, DependencyType::Call);
        let mut implicits = db.scc_implicits(scc_representative)?;

        let precedence = db.function_declaration_implicit_precedence(
            function.function_with_body_id(db).base_semantic_function(db),
        )?;
        precedence.apply(&mut implicits, semantic_db);

        Ok(implicits)
    }
}
impl<'a, T: Upcast<dyn LoweringGroup + 'a> + ?Sized> FunctionImplicitsTrait<'a> for T {}

/// Query implementation of [LoweringGroup::scc_implicits].
pub fn scc_implicits(db: &dyn LoweringGroup, scc: ConcreteSCCRepresentative) -> Maybe<Vec<TypeId>> {
    let scc_functions = db.concrete_function_with_body_inlined_scc(scc.0, DependencyType::Call);
    let mut all_implicits = HashSet::new();
    for function in scc_functions {
        // Add the function's explicit implicits.
        all_implicits.extend(function.function_id(db)?.signature(db)?.implicits);
        // For each direct callee, add its implicits.
        let direct_callees =
            db.concrete_function_with_body_inlined_direct_callees(function, DependencyType::Call)?;
        for direct_callee in direct_callees {
            if let Some(callee_body) = direct_callee.body(db.upcast())? {
                let callee_scc = db.concrete_function_with_body_scc_inlined_representative(
                    callee_body,
                    DependencyType::Call,
                );
                if callee_scc != scc {
                    all_implicits.extend(db.scc_implicits(callee_scc)?);
                }
            } else {
                all_implicits.extend(direct_callee.signature(db)?.implicits);
            }
        }
    }
    Ok(all_implicits.into_iter().collect())
}
