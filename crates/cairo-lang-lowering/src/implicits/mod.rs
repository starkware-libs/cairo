use std::collections::{HashMap, HashSet};

use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic as semantic;
use itertools::{chain, zip_eq, Itertools};
use semantic::items::functions::{
    ConcreteFunctionWithBody, GenericFunctionId, GenericFunctionWithBodyId,
};
use semantic::{ConcreteFunctionWithBodyId, TypeId};

use crate::db::{ConcreteSCCRepresentative, LoweringGroup};
use crate::graph_algorithms::strongly_connected_components::concrete_function_with_body_scc;
use crate::lower::context::{LoweringContext, LoweringContextBuilder, VarRequest};
use crate::{BlockId, FlatBlockEnd, FlatLowered, MatchArm, MatchInfo, Statement, VariableId};

struct Context<'a> {
    db: &'a dyn LoweringGroup,
    ctx: &'a mut LoweringContext<'a>,
    lowered: &'a mut FlatLowered,
    implicit_index: HashMap<TypeId, usize>,
    implicits_tys: Vec<TypeId>,
    implicit_vars_for_block: HashMap<BlockId, Vec<VariableId>>,
    visited: HashSet<BlockId>,
    location: StableLocation,
}

/// Lowering phase that adds implicits.
pub fn lower_implicits(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
    lowered: &mut FlatLowered,
) {
    if inner_lower_implicits(db, function_id, lowered).is_err() {
        lowered.blocks.0.clear();
    }
}

/// Similar to lower_implicits, but uses Maybe<> for convenience.
pub fn inner_lower_implicits(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
    lowered: &mut FlatLowered,
) -> Maybe<()> {
    let generic_function_id = function_id.function_with_body_id(db.upcast());
    let function_signature = db.function_with_body_signature(generic_function_id)?;
    let location = StableLocation::new(
        generic_function_id.module_file_id(db.upcast()),
        function_signature.stable_ptr.untyped(),
    );
    lowered.blocks.has_root()?;
    let root_block_id = BlockId::root();

    let lowering_info = LoweringContextBuilder::new(db, generic_function_id)?;
    let mut lowering_ctx = lowering_info.ctx()?;
    lowering_ctx.variables = lowered.variables.clone();

    let implicits_tys = db.concrete_function_with_body_all_implicits_vec(function_id)?;

    let implicit_index =
        HashMap::from_iter(implicits_tys.iter().enumerate().map(|(i, ty)| (*ty, i)));
    let mut ctx = Context {
        db,
        ctx: &mut lowering_ctx,
        lowered,
        implicit_index,
        implicits_tys,
        implicit_vars_for_block: Default::default(),
        visited: Default::default(),
        location,
    };

    // Start form root block.
    lower_block_implicits(&mut ctx, root_block_id)?;

    // Introduce new input variables in the root block.
    let implicit_vars = &ctx.implicit_vars_for_block[&root_block_id];
    ctx.lowered.blocks[root_block_id].inputs.splice(0..0, implicit_vars.iter().cloned());

    lowered.variables = std::mem::take(&mut ctx.ctx.variables);

    Ok(())
}

/// Allocates and returns new variables for each of the current function's implicits.
fn alloc_implicits(
    ctx: &mut LoweringContext<'_>,
    implicits_tys: &[TypeId],
    location: StableLocation,
) -> Vec<VariableId> {
    implicits_tys.iter().copied().map(|ty| ctx.new_var(VarRequest { ty, location })).collect_vec()
}

/// Adds implicits in a block.
fn lower_block_implicits(ctx: &mut Context<'_>, block_id: BlockId) -> Maybe<()> {
    if !ctx.visited.insert(block_id) {
        return Ok(());
    }
    let mut implicits = ctx
        .implicit_vars_for_block
        .entry(block_id)
        .or_insert_with(|| alloc_implicits(ctx.ctx, &ctx.implicits_tys, ctx.location))
        .clone();
    for statement in &mut ctx.lowered.blocks[block_id].statements {
        if let Statement::Call(stmt) = statement {
            let callee_implicits = ctx.db.function_all_implicits(stmt.function)?;
            let indices = callee_implicits.iter().map(|ty| ctx.implicit_index[ty]).collect_vec();
            let implicit_input_vars = indices.iter().map(|i| implicits[*i]);
            stmt.inputs.splice(0..0, implicit_input_vars);
            let implicit_output_vars = callee_implicits
                .iter()
                .copied()
                .map(|ty| ctx.ctx.new_var(VarRequest { ty, location: stmt.location }))
                .collect_vec();
            for (i, var) in zip_eq(indices, implicit_output_vars.iter()) {
                implicits[i] = *var;
            }
            stmt.outputs.splice(0..0, implicit_output_vars);
        }
    }
    // End.
    let mut blocks_to_visit = vec![];
    match &mut ctx.lowered.blocks[block_id].end {
        FlatBlockEnd::Return(rets) => {
            rets.splice(0..0, implicits);
        }
        FlatBlockEnd::Panic(_) => {
            unreachable!("Panics should have been stripped in a previous phase.")
        }
        FlatBlockEnd::Goto(block_id, remapping) => {
            let target_implicits = ctx
                .implicit_vars_for_block
                .entry(*block_id)
                .or_insert_with(|| alloc_implicits(ctx.ctx, &ctx.implicits_tys, ctx.location))
                .clone();
            let old_remapping = std::mem::take(&mut remapping.remapping);
            remapping.remapping =
                chain!(zip_eq(target_implicits, implicits), old_remapping).collect();
            blocks_to_visit.push(*block_id);
        }
        FlatBlockEnd::Match { info } => match info {
            MatchInfo::Enum(stmt) => {
                for MatchArm { variant_id: _, block_id } in &stmt.arms {
                    assert!(
                        ctx.implicit_vars_for_block.insert(*block_id, implicits.clone()).is_none(),
                        "Multiple jumps to arm blocks are not allowed."
                    );
                    blocks_to_visit.push(*block_id);
                }
            }
            MatchInfo::Extern(stmt) => {
                let callee_implicits = ctx.db.function_all_implicits(stmt.function)?;
                let indices =
                    callee_implicits.iter().map(|ty| ctx.implicit_index[ty]).collect_vec();
                let implicit_input_vars = indices.iter().map(|i| implicits[*i]);
                stmt.inputs.splice(0..0, implicit_input_vars);
                let location = stmt.location;

                for MatchArm { variant_id: _, block_id } in stmt.arms.clone() {
                    let mut arm_implicits = implicits.clone();
                    let mut implicit_input_vars = vec![];
                    for ty in callee_implicits.iter().copied() {
                        let var = ctx.ctx.new_var(VarRequest { ty, location });
                        implicit_input_vars.push(var);
                        let implicit_index = ctx.implicit_index[&ty];
                        arm_implicits[implicit_index] = var;
                    }
                    assert!(
                        ctx.implicit_vars_for_block.insert(block_id, arm_implicits).is_none(),
                        "Multiple jumps to arm blocks are not allowed."
                    );
                    ctx.lowered.blocks[block_id].inputs.splice(0..0, implicit_input_vars);
                    blocks_to_visit.push(block_id);
                }
            }
        },
        FlatBlockEnd::NotSet => unreachable!(),
    }
    for block_id in blocks_to_visit {
        lower_block_implicits(ctx, block_id)?;
    }
    Ok(())
}

// =========== Query implementations ===========

/// Query implementation of [crate::db::LoweringGroup::function_scc_explicit_implicits].
pub fn function_scc_explicit_implicits(
    db: &dyn LoweringGroup,
    function: ConcreteSCCRepresentative,
) -> Maybe<HashSet<TypeId>> {
    let scc = concrete_function_with_body_scc(db, function.0);
    let mut explicit_implicits = HashSet::new();
    for func in scc {
        let current_implicits: HashSet<TypeId> = match func.generic_function(db.upcast()) {
            GenericFunctionWithBodyId::Free(free_function) => {
                db.free_function_declaration_implicits(free_function)?.into_iter().collect()
            }
            GenericFunctionWithBodyId::Impl(concrete_impl_generic_function) => db
                .impl_function_declaration_implicits(concrete_impl_generic_function.function)?
                .into_iter()
                .collect(),
        };
        explicit_implicits.extend(current_implicits);
    }
    Ok(explicit_implicits)
}

/// Query implementation of [crate::db::LoweringGroup::function_all_implicits].
pub fn function_all_implicits(
    db: &dyn LoweringGroup,
    function: semantic::FunctionId,
) -> Maybe<Vec<TypeId>> {
    let concrete_function = function.get_concrete(db.upcast());
    match concrete_function.generic_function {
        GenericFunctionId::Free(free_function) => {
            let concrete_with_body =
                db.intern_concrete_function_with_body(ConcreteFunctionWithBody {
                    generic_function: GenericFunctionWithBodyId::Free(free_function),
                    generic_args: concrete_function.generic_args,
                });
            db.concrete_function_with_body_all_implicits_vec(concrete_with_body)
        }
        GenericFunctionId::Impl(impl_generic_function) => {
            let Some(generic_with_body) =
                impl_generic_function.to_generic_with_body(db.upcast())?
                else {
                    unreachable!();
                };
            let concrete_with_body =
                db.intern_concrete_function_with_body(ConcreteFunctionWithBody {
                    generic_function: generic_with_body,
                    generic_args: concrete_function.generic_args,
                });
            db.concrete_function_with_body_all_implicits_vec(concrete_with_body)
        }
        GenericFunctionId::Extern(extern_function) => {
            db.extern_function_declaration_implicits(extern_function)
        }
    }
}

/// Query implementation of [crate::db::LoweringGroup::concrete_function_with_body_all_implicits].
pub fn concrete_function_with_body_all_implicits(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
) -> Maybe<HashSet<TypeId>> {
    // Find the SCC representative.
    let scc_representative = db.concrete_function_with_body_scc_representative(function);

    // Start with the explicit implicits of the SCC.
    let mut all_implicits = db.function_scc_explicit_implicits(scc_representative.clone())?;

    let direct_callees = db.concrete_function_with_body_direct_callees(function)?;
    // For each direct callee, add its implicits.
    for direct_callee in direct_callees {
        let generic_function = direct_callee.generic_function;
        let current_implicits = match generic_function {
            GenericFunctionId::Free(free_function) => {
                // For a free function, call this method recursively. To avoid cycles, first
                // check that the callee is not in this function's SCC.
                let concrete_with_body =
                    db.intern_concrete_function_with_body(ConcreteFunctionWithBody {
                        generic_function: GenericFunctionWithBodyId::Free(free_function),
                        generic_args: direct_callee.generic_args,
                    });
                let direct_callee_representative =
                    db.concrete_function_with_body_scc_representative(concrete_with_body);
                if direct_callee_representative == scc_representative {
                    // We already have the implicits of this SCC - do nothing.
                    continue;
                }
                db.concrete_function_with_body_all_implicits(direct_callee_representative.0)?
            }
            GenericFunctionId::Impl(impl_generic_function) => {
                let Some(generic_with_body) =
                    impl_generic_function.to_generic_with_body(db.upcast())?
                    else {
                        unreachable!();
                    };
                // For an impl function, call this method recursively. To avoid cycles, first
                // check that the callee is not in this function's SCC.
                let concrete_with_body =
                    db.intern_concrete_function_with_body(ConcreteFunctionWithBody {
                        generic_function: generic_with_body,
                        generic_args: direct_callee.generic_args,
                    });
                let direct_callee_representative =
                    db.concrete_function_with_body_scc_representative(concrete_with_body);
                if direct_callee_representative == scc_representative {
                    // We already have the implicits of this SCC - do nothing.
                    continue;
                }
                db.concrete_function_with_body_all_implicits(direct_callee_representative.0)?
            }
            GenericFunctionId::Extern(extern_function) => {
                // All implicits of a libfunc are explicit implicits.
                db.extern_function_declaration_implicits(extern_function)?.into_iter().collect()
            }
        };
        all_implicits.extend(&current_implicits);
    }
    Ok(all_implicits)
}

/// Query implementation of
/// [crate::db::LoweringGroup::concrete_function_with_body_all_implicits_vec].
pub fn concrete_function_with_body_all_implicits_vec(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
) -> Maybe<Vec<TypeId>> {
    let implicits_set = db.concrete_function_with_body_all_implicits(function)?;
    let mut implicits_vec = implicits_set.into_iter().collect_vec();

    let semantic_db = db.upcast();
    let precedence = db.implicit_precedence();
    implicits_vec.sort_by_cached_key(|type_id| {
        if let Some(idx) = precedence.iter().position(|item| item == type_id) {
            return (idx, "".to_string());
        }

        (precedence.len(), type_id.format(semantic_db))
    });

    Ok(implicits_vec)
}
