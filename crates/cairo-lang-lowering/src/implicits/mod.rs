use std::collections::HashMap;

use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic as semantic;
use itertools::{zip_eq, Itertools};
use semantic::{ConcreteFunctionWithBodyId, TypeId};

use crate::db::LoweringGroup;
use crate::lower::context::{LoweringContext, LoweringContextBuilder, VarRequest};
use crate::{BlockId, FlatLowered, Statement, VariableId};

struct Context<'a> {
    db: &'a dyn LoweringGroup,
    ctx: &'a mut LoweringContext<'a>,
    lowered: &'a mut FlatLowered,
    implicit_index: HashMap<TypeId, usize>,
    implicits_tys: Vec<TypeId>,
}

/// Lowering phase that adds implicits.
pub fn lower_implicits(
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
    let root_block_id = lowered.root?;

    let lowering_info = LoweringContextBuilder::new(db, generic_function_id)?;
    let mut ctx = lowering_info.ctx()?;
    ctx.variables = lowered.variables.clone();

    // Skip this phase for non panicable functions.
    let implicits_tys = db.function_all_implicits(function_id.function_id(db.upcast()))?;

    // Introduce new input variables.
    let implicit_vars = implicits_tys
        .iter()
        .copied()
        .map(|ty| ctx.new_var(VarRequest { ty, location }))
        .collect_vec();
    lowered.blocks[root_block_id].inputs.splice(0..0, implicit_vars.iter().cloned());

    let implicit_index = HashMap::new();
    for (i, ty) in implicits_tys.iter().enumerate() {
        implicit_index.insert(*ty, i);
    }
    let ctx = Context { db, ctx: &mut ctx, lowered, implicit_index,implicits_tys };

    // Iterate block queue (old and new blocks).
    lower_block(&mut ctx, root_block_id, implicit_vars)?;
    Ok(())
}

pub fn lower_block(
    ctx: &mut Context<'_>,
    block_id: BlockId,
    mut implicits: Vec<VariableId>,
) -> Maybe<Vec<VariableId>> {
    for i in 0..ctx.lowered.blocks[block_id].statements.len() {
        match &mut ctx.lowered.blocks[block_id].statements[i] {
            Statement::Call(stmt) => {
                let callee_implicits = ctx.db.function_all_implicits(stmt.function)?;
                let indices =
                    callee_implicits.iter().map(|ty| ctx.implicit_index[ty]).collect_vec();
                let implicit_input_vars = indices.iter().map(|i| implicits[*i]);
                stmt.inputs.splice(0..0, implicit_input_vars);
                let implicit_output_vars = callee_implicits
                    .iter()
                    .copied()
                    .map(|ty| ctx.ctx.new_var(VarRequest { ty, location: stmt.location }))
                    .collect_vec();
                for (i, var) in zip_eq(indices, implicit_output_vars) {
                    implicits[i] = var;
                }
                stmt.outputs.splice(0..0, implicit_output_vars);
            }
            Statement::MatchExtern(stmt) => {
                let callee_implicits = ctx.db.function_all_implicits(stmt.function)?;
                let indices =
                    callee_implicits.iter().map(|ty| ctx.implicit_index[ty]).collect_vec();
                let implicit_input_vars = indices.iter().map(|i| implicits[*i]);
                stmt.inputs.splice(0..0, implicit_input_vars);

                let implicit_output_vars = ctx.implicits_tys
                    .iter()
                    .copied()
                    .map(|ty| ctx.ctx.new_var(VarRequest { ty, location: stmt.location }))
                    .collect_vec();

                for (_, block_id) in stmt.arms {
                    let arm_implicits =  implicits.clone();
                    let implicit_output_vars = callee_implicits
                    .iter()
                    .copied()
                    .map(|ty| ctx.ctx.new_var(VarRequest { ty, location: stmt.location }))
                    .collect_vec();

                    lower_block(ctx, block_id, arm_implicits
                }

                for (i, var) in zip_eq(indices, implicit_output_vars) {
                    implicits[i] = var;
                }
                stmt.outputs.splice(0..0, implicit_output_vars);
            }
            Statement::MatchEnum(_) => todo!(),
            Statement::Literal(_)
            | Statement::StructConstruct(_)
            | Statement::StructDestructure(_)
            | Statement::EnumConstruct(_) => {}
        }
    }
    // End.
    todo!()
}
