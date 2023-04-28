use std::collections::{HashMap, HashSet};

use cairo_lang_defs::diagnostic_utils::StableLocationOption;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::corelib::get_core_ty_by_name;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_utils::Upcast;
use itertools::{chain, zip_eq, Itertools};
use semantic::TypeId;

use crate::blocks::Blocks;
use crate::db::{ConcreteSCCRepresentative, LoweringGroup};
use crate::graph_algorithms::strongly_connected_components::concrete_function_with_body_postpanic_scc;
use crate::ids::{ConcreteFunctionWithBodyId, FunctionId};
use crate::lower::context::{VarRequest, VariableAllocator};
use crate::{BlockId, FlatBlockEnd, FlatLowered, MatchArm, MatchInfo, Statement, VariableId};

struct Context<'a> {
    db: &'a dyn LoweringGroup,
    variables: &'a mut VariableAllocator<'a>,
    lowered: &'a mut FlatLowered,
    implicit_index: HashMap<TypeId, usize>,
    implicits_tys: Vec<TypeId>,
    implicit_vars_for_block: HashMap<BlockId, Vec<VariableId>>,
    visited: HashSet<BlockId>,
    location: StableLocationOption,
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
    let module_file_id = semantic_function.module_file_id(db.upcast());
    let location = StableLocationOption::new(
        module_file_id,
        semantic_function.untyped_stable_ptr(db.upcast()),
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
    lower_block_implicits(&mut ctx, root_block_id)?;

    // Introduce new input variables in the root block.
    let implicit_vars = &ctx.implicit_vars_for_block[&root_block_id];
    ctx.lowered.parameters.splice(0..0, implicit_vars.iter().cloned());

    lowered.variables = std::mem::take(&mut ctx.variables.variables);

    Ok(())
}

/// Allocates and returns new variables for each of the current function's implicits.
fn alloc_implicits(
    ctx: &mut VariableAllocator<'_>,
    implicits_tys: &[TypeId],
    location: StableLocationOption,
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
        .or_insert_with(|| alloc_implicits(ctx.variables, &ctx.implicits_tys, ctx.location))
        .clone();
    for statement in &mut ctx.lowered.blocks[block_id].statements {
        if let Statement::Call(stmt) = statement {
            let callee_implicits = ctx.db.function_implicits(stmt.function)?;
            let indices = callee_implicits.iter().map(|ty| ctx.implicit_index[ty]).collect_vec();
            let implicit_input_vars = indices.iter().map(|i| implicits[*i]);
            stmt.inputs.splice(0..0, implicit_input_vars);
            let implicit_output_vars = callee_implicits
                .iter()
                .copied()
                .map(|ty| ctx.variables.new_var(VarRequest { ty, location: stmt.location }))
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
                .or_insert_with(|| alloc_implicits(ctx.variables, &ctx.implicits_tys, ctx.location))
                .clone();
            let old_remapping = std::mem::take(&mut remapping.remapping);
            remapping.remapping =
                chain!(zip_eq(target_implicits, implicits), old_remapping).collect();
            blocks_to_visit.push(*block_id);
        }
        FlatBlockEnd::Match { info } => match info {
            MatchInfo::Enum(stmt) => {
                for MatchArm { variant_id: _, block_id, var_ids: _ } in &stmt.arms {
                    assert!(
                        ctx.implicit_vars_for_block.insert(*block_id, implicits.clone()).is_none(),
                        "Multiple jumps to arm blocks are not allowed."
                    );
                    blocks_to_visit.push(*block_id);
                }
            }
            MatchInfo::Extern(stmt) => {
                let callee_implicits = ctx.db.function_implicits(stmt.function)?;
                let indices =
                    callee_implicits.iter().map(|ty| ctx.implicit_index[ty]).collect_vec();
                let implicit_input_vars = indices.iter().map(|i| implicits[*i]);
                stmt.inputs.splice(0..0, implicit_input_vars);
                let location = stmt.location;

                for MatchArm { variant_id: _, block_id, var_ids } in stmt.arms.iter_mut() {
                    let mut arm_implicits = implicits.clone();
                    let mut implicit_input_vars = vec![];
                    for ty in callee_implicits.iter().copied() {
                        let var = ctx.variables.new_var(VarRequest { ty, location });
                        implicit_input_vars.push(var);
                        let implicit_index = ctx.implicit_index[&ty];
                        arm_implicits[implicit_index] = var;
                    }
                    assert!(
                        ctx.implicit_vars_for_block.insert(*block_id, arm_implicits).is_none(),
                        "Multiple jumps to arm blocks are not allowed."
                    );

                    var_ids.splice(0..0, implicit_input_vars);
                    blocks_to_visit.push(*block_id);
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

/// Query implementation of [crate::db::LoweringGroup::function_implicits].
pub fn function_implicits(db: &dyn LoweringGroup, function: FunctionId) -> Maybe<Vec<TypeId>> {
    if let Some(body) = function.body(db.upcast())? {
        return db.function_with_body_implicits(body);
    }
    Ok(function.signature(db)?.implicits)
}

/// A trait to add helper methods in [LoweringGroup].
pub trait FunctionImplicitsTrait<'a>: Upcast<dyn LoweringGroup + 'a> {
    /// Returns all the implicitis used by a [ConcreteFunctionWithBodyId].
    fn function_with_body_implicits(
        &self,
        function: ConcreteFunctionWithBodyId,
    ) -> Maybe<Vec<TypeId>> {
        let scc_representative =
            self.upcast().concrete_function_with_body_scc_postpanic_representative(function);
        self.upcast().scc_implicits(scc_representative)
    }
}
impl<'a, T: Upcast<dyn LoweringGroup + 'a> + ?Sized> FunctionImplicitsTrait<'a> for T {}

/// Query implementation of [crate::db::LoweringGroup::scc_implicits].
pub fn scc_implicits(db: &dyn LoweringGroup, scc: ConcreteSCCRepresentative) -> Maybe<Vec<TypeId>> {
    let scc_functions = concrete_function_with_body_postpanic_scc(db, scc.0);
    let mut all_implicits = HashSet::new();
    for function in scc_functions {
        // Add the function's explicit implicits.
        all_implicits.extend(function.function_id(db)?.signature(db)?.implicits);
        // For each direct callee, add its implicits.
        let direct_callees = db.concrete_function_with_body_postpanic_direct_callees(function)?;
        for direct_callee in direct_callees {
            if let Some(callee_body) = direct_callee.body(db.upcast())? {
                let callee_scc =
                    db.concrete_function_with_body_scc_postpanic_representative(callee_body);
                if callee_scc != scc {
                    all_implicits.extend(db.scc_implicits(callee_scc)?);
                }
            } else {
                all_implicits.extend(direct_callee.signature(db)?.implicits);
            }
        }
    }
    Ok(sort_implicits(db, all_implicits))
}

/// Sorts the given implicits: first the ones with precedence (according to it), then the others by
/// their name.
fn sort_implicits(db: &dyn LoweringGroup, implicits: HashSet<TypeId>) -> Vec<TypeId> {
    let semantic_db: &dyn SemanticGroup = db.upcast();
    let mut implicits_vec = implicits.into_iter().collect_vec();
    let precedence = db
        .implicit_precedence()
        .iter()
        .map(|name| get_core_ty_by_name(semantic_db, name.clone(), vec![]))
        .collect::<Vec<_>>();
    implicits_vec.sort_by_cached_key(|type_id| {
        if let Some(idx) = precedence.iter().position(|item| item == type_id) {
            return (idx, "".to_string());
        }

        (precedence.len(), type_id.format(semantic_db))
    });

    implicits_vec
}
