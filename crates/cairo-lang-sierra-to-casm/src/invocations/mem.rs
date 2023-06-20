use cairo_lang_casm::ap_change::ApplyApChange;
use cairo_lang_casm::cell_expression::{CellExpression, CellOperator};
use cairo_lang_casm::instructions::Instruction;
use cairo_lang_casm::operand::{CellRef, Register};
use cairo_lang_casm::{casm, casm_extend};
use cairo_lang_sierra::extensions::lib_func::SignatureAndTypeConcreteLibfunc;
use cairo_lang_sierra::extensions::mem::MemConcreteLibfunc;
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::extract_matches;
use itertools::{repeat_n, zip_eq};

use super::{misc, CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::environment::frame_state;
use crate::references::ReferenceExpression;

/// Builds instructions for Sierra memory operations.
pub fn build(
    libfunc: &MemConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        MemConcreteLibfunc::StoreTemp(SignatureAndTypeConcreteLibfunc { ty, .. }) => {
            build_store_temp(builder, ty)
        }
        MemConcreteLibfunc::Rename(_) => misc::build_identity(builder),
        MemConcreteLibfunc::FinalizeLocals(_) => build_finalize_locals(builder),
        MemConcreteLibfunc::AllocLocal(SignatureAndTypeConcreteLibfunc { ty, .. }) => {
            build_alloc_local(builder, ty)
        }
        MemConcreteLibfunc::StoreLocal(SignatureAndTypeConcreteLibfunc { ty, .. }) => {
            build_store_local(builder, ty)
        }
    }
}

/// Adds a single instruction to a casm context.
macro_rules! add_instruction {
    ($ctx:ident, $($tok:tt)*) => {{
        casm_extend! {$ctx, $($tok)* ;}
    }}
}

/// Returns a store instruction. Helper function for store_temp and store_local.
///
/// Note that for `dst_cells`, `ap` increases by 1 after every store, so in order to write
/// two consecutive cells, `dst_cells` should be `[ap, ap]` and not `[ap, ap + 1]`.
/// On the other hand, the function adjusts the ap-change in the cells of `src_expr`.
fn get_store_instructions<DstCells: Iterator<Item = CellRef>>(
    builder: &CompiledInvocationBuilder<'_>,
    src_type: &ConcreteTypeId,
    dst_cells: DstCells,
    src_expr: &ReferenceExpression,
) -> Result<Vec<Instruction>, InvocationError> {
    if builder.program_info.type_sizes.get(src_type).is_none() {
        return Err(InvocationError::NotSized(builder.invocation.clone()));
    }
    let mut ctx = casm!();
    let mut ap_change = 0;
    for (dst, cell_expr_orig) in zip_eq(dst_cells, &src_expr.cells) {
        let cell_expr = cell_expr_orig.clone().apply_known_ap_change(ap_change as usize).unwrap();
        match cell_expr {
            CellExpression::Deref(operand) => add_instruction!(ctx, dst = operand),
            CellExpression::DoubleDeref(operand, offset) => {
                add_instruction!(ctx, dst = [[&operand] + offset])
            }
            CellExpression::Immediate(operand) => add_instruction!(ctx, dst = operand),
            CellExpression::BinOp { op, a, b } => match op {
                CellOperator::Add => add_instruction!(ctx, dst = a + b),
                CellOperator::Mul => add_instruction!(ctx, dst = a * b),
                // dst = a - b => a = dst + b
                CellOperator::Sub => add_instruction!(ctx, a = dst + b),
                // dst = a / b => a = dst * b
                CellOperator::Div => add_instruction!(ctx, a = dst * b),
            },
        }
        if matches!(dst.register, Register::AP) {
            ap_change += 1;
            ctx.instructions.last_mut().unwrap().inc_ap = true;
        }
    }
    Ok(ctx.instructions)
}

/// Handles store_temp for the given type.
fn build_store_temp(
    builder: CompiledInvocationBuilder<'_>,
    ty: &ConcreteTypeId,
) -> Result<CompiledInvocation, InvocationError> {
    let expression = builder.try_get_refs::<1>()?[0];

    let type_size = builder.program_info.type_sizes[ty];
    let instructions = get_store_instructions(
        &builder,
        ty,
        repeat_n(CellRef { register: Register::AP, offset: 0 }, type_size as usize),
        expression,
    )?;
    Ok(builder.build(
        instructions,
        vec![],
        [[ReferenceExpression {
            cells: (-type_size..0)
                .map(|i| CellExpression::Deref(CellRef { register: Register::AP, offset: i }))
                .collect(),
        }]
        .into_iter()]
        .into_iter(),
    ))
}

/// Handles store_local for the given type.
fn build_store_local(
    builder: CompiledInvocationBuilder<'_>,
    ty: &ConcreteTypeId,
) -> Result<CompiledInvocation, InvocationError> {
    let [dst_expr, src_expr] = builder.try_get_refs()?;
    let dst_cells = dst_expr.cells.iter().map(|c| *extract_matches!(c, CellExpression::Deref));
    let instructions = get_store_instructions(&builder, ty, dst_cells, src_expr)?;
    let dst_expr = dst_expr.clone();
    Ok(builder.build(instructions, vec![], [[dst_expr].into_iter()].into_iter()))
}

/// Handles a locals allocation finalization instruction.
fn build_finalize_locals(
    mut builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (n_slots, frame_state) = frame_state::handle_finalize_locals(
        builder.environment.frame_state,
        builder.environment.ap_tracking,
    )?;
    builder.environment.frame_state = frame_state;
    Ok(builder.build(
        casm! { ap += (n_slots as i128); }.instructions,
        vec![],
        [[].into_iter()].into_iter(),
    ))
}

/// Handles the local variable allocation instruction.
fn build_alloc_local(
    mut builder: CompiledInvocationBuilder<'_>,
    ty: &ConcreteTypeId,
) -> Result<CompiledInvocation, InvocationError> {
    let allocation_size = *builder
        .program_info
        .type_sizes
        .get(ty)
        .ok_or_else(|| InvocationError::NotSized(builder.invocation.clone()))?;

    let (slot, frame_state) = frame_state::handle_alloc_local(
        builder.environment.frame_state,
        builder.environment.ap_tracking,
        allocation_size as usize,
    )?;
    builder.environment.frame_state = frame_state;
    let slot: i16 = slot.into_or_panic();
    Ok(builder.build_only_reference_changes(
        [ReferenceExpression {
            cells: (0..allocation_size)
                .map(|i| {
                    CellExpression::Deref(CellRef { register: Register::FP, offset: slot + i })
                })
                .collect(),
        }]
        .into_iter(),
    ))
}
