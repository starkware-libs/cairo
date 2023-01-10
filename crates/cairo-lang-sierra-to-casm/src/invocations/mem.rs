use cairo_lang_casm::ap_change::{ApChange, ApplyApChange};
use cairo_lang_casm::cell_expression::{CellExpression, CellOperator};
use cairo_lang_casm::instructions::Instruction;
use cairo_lang_casm::operand::{CellRef, Register};
use cairo_lang_casm::{casm, casm_extend};
use cairo_lang_sierra::extensions::lib_func::SignatureAndTypeConcreteLibfunc;
use cairo_lang_sierra::extensions::mem::MemConcreteLibfunc;
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_utils::casts::usize_as_i16;

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
        MemConcreteLibfunc::AlignTemps(_) => {
            Err(InvocationError::NotImplemented(builder.invocation.clone()))
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
fn get_store_instructions(
    builder: &CompiledInvocationBuilder<'_>,
    src_type: &ConcreteTypeId,
    mut dst: CellRef,
    src_expr: &ReferenceExpression,
) -> Result<Vec<Instruction>, InvocationError> {
    if builder.program_info.type_sizes.get(src_type).is_none() {
        return Err(InvocationError::NotSized(builder.invocation.clone()));
    }
    let mut ctx = casm!();
    let mut ap_change = 0;
    let inc_ap = match dst.register {
        Register::AP => true,
        Register::FP => false,
    };
    for cell_expr_orig in &src_expr.cells {
        let cell_expr =
            cell_expr_orig.clone().apply_ap_change(ApChange::Known(ap_change as usize)).unwrap();
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
        if inc_ap {
            ap_change += 1;
            ctx.instructions.last_mut().unwrap().inc_ap = true;
        } else {
            dst.offset += 1;
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

    let instructions = get_store_instructions(
        &builder,
        ty,
        CellRef { register: Register::AP, offset: 0 },
        expression,
    )?;
    let type_size = builder.program_info.type_sizes[ty];
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
    let dst = dst_expr
        .try_unpack_single()?
        .to_deref()
        .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;
    let instructions = get_store_instructions(&builder, ty, dst, src_expr)?;
    let type_size = builder.program_info.type_sizes[ty];
    Ok(builder.build(
        instructions,
        vec![],
        [[ReferenceExpression {
            cells: (0..type_size)
                .map(|i| {
                    CellExpression::Deref(CellRef {
                        register: Register::FP,
                        offset: dst.offset + i,
                    })
                })
                .collect(),
        }]
        .into_iter()]
        .into_iter(),
    ))
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

    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::Deref(CellRef {
            register: Register::FP,
            offset: usize_as_i16(slot),
        }))]
        .into_iter(),
    ))
}
