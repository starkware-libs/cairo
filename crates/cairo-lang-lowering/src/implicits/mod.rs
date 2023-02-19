use std::collections::{HashMap, HashSet};

use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic as semantic;
use itertools::{chain, zip_eq, Itertools};
use semantic::{ConcreteFunctionWithBodyId, TypeId};

use crate::db::LoweringGroup;
use crate::lower::context::{LoweringContext, LoweringContextBuilder, VarRequest};
use crate::{BlockId, FlatBlockEnd, FlatLowered, MatchInfo, Statement, VariableId};

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

    let implicits_tys = db.function_all_implicits(function_id.function_id(db.upcast()))?;

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
        FlatBlockEnd::Unreachable => {}
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
                for (_, block_id) in &stmt.arms.clone() {
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

                for (_, block_id) in stmt.arms.clone() {
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
