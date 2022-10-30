use casm::ap_change::{ApChange, ApplyApChange};
use casm::casm;
use casm::hints::Hint;
use casm::instructions::{AddApInstruction, AssertEqInstruction, Instruction, InstructionBody};
use casm::operand::{BinOpOperand, CellRef, Operation, Register, ResOperand};
use sierra::extensions::felt::FeltOperator;
use sierra::extensions::mem::{
    AllocLocalConcreteLibFunc, MemConcreteLibFunc, StoreLocalConcreteLibFunc,
    StoreTempConcreteLibFunc,
};
use sierra::ids::ConcreteTypeId;
use utils::try_extract_matches;

use super::{misc, CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::environment::frame_state;
use crate::references::{BinOpExpression, CellExpression, ReferenceExpression, ReferenceValue};

/// Builds instructions for Sierra memory operations.
pub fn build(
    libfunc: &MemConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        MemConcreteLibFunc::StoreTemp(StoreTempConcreteLibFunc { ty, .. }) => {
            build_store_temp(builder, ty)
        }
        MemConcreteLibFunc::Rename(_) => misc::build_identity(builder),
        MemConcreteLibFunc::FinalizeLocals(_) => build_finalize_locals(builder),
        MemConcreteLibFunc::AllocLocal(AllocLocalConcreteLibFunc { ty, .. }) => {
            build_alloc_local(builder, ty)
        }
        MemConcreteLibFunc::StoreLocal(StoreLocalConcreteLibFunc { ty, .. }) => {
            build_store_local(builder, ty)
        }
        MemConcreteLibFunc::AlignTemps(_) => {
            Err(InvocationError::NotImplemented(builder.invocation.clone()))
        }
    }
}

/// Returns a store instruction. Helper function for store_temp and store_local.
fn get_store_instructions(
    builder: &CompiledInvocationBuilder<'_>,
    src_type: &ConcreteTypeId,
    mut dst: CellRef,
    src_expr: &ReferenceExpression,
    inc_ap: bool,
) -> Result<Vec<Instruction>, InvocationError> {
    match builder.program_info.type_sizes.get(src_type) {
        Some(0) => return Err(InvocationError::NotSized(builder.invocation.clone())),
        None => return Err(InvocationError::NotImplemented(builder.invocation.clone())),
        Some(_) => {}
    };

    let mut instructions = vec![];
    let mut ap_change = 0;
    // TODO(Gil): Consider using the casm! macros and add an if inc_ap.
    for cell_expr_orig in src_expr.cells.iter() {
        let cell_expr = cell_expr_orig.clone().apply_ap_change(ApChange::Known(ap_change)).unwrap();
        instructions.push(match cell_expr {
            CellExpression::Deref(operand) => Instruction {
                body: InstructionBody::AssertEq(AssertEqInstruction {
                    a: dst,
                    b: ResOperand::Deref(operand),
                }),
                inc_ap,
                hints: vec![],
            },
            CellExpression::DoubleDeref(operand) => Instruction {
                body: InstructionBody::AssertEq(AssertEqInstruction {
                    a: dst,
                    b: ResOperand::DoubleDeref(operand, 0),
                }),
                inc_ap,
                hints: vec![],
            },
            CellExpression::IntoSingleCellRef(operand) => Instruction {
                body: InstructionBody::AssertEq(AssertEqInstruction {
                    a: operand,
                    b: ResOperand::DoubleDeref(dst, 0),
                }),
                inc_ap,
                hints: vec![Hint::AllocSegment { dst }],
            },
            CellExpression::Immediate(operand) => Instruction {
                body: InstructionBody::AssertEq(AssertEqInstruction {
                    a: dst,
                    b: ResOperand::Immediate(operand),
                }),
                inc_ap,
                hints: vec![],
            },
            CellExpression::BinOp(BinOpExpression { op, a, b }) => match op {
                FeltOperator::Add => Instruction {
                    body: InstructionBody::AssertEq(AssertEqInstruction {
                        a: dst,
                        b: ResOperand::BinOp(BinOpOperand { op: Operation::Add, a, b }),
                    }),
                    inc_ap,
                    hints: vec![],
                },
                FeltOperator::Mul => Instruction {
                    body: InstructionBody::AssertEq(AssertEqInstruction {
                        a: dst,
                        b: ResOperand::BinOp(BinOpOperand { op: Operation::Mul, a, b }),
                    }),
                    inc_ap,
                    hints: vec![],
                },

                // dst = a - b => a = dst + b
                FeltOperator::Sub => Instruction {
                    body: InstructionBody::AssertEq(AssertEqInstruction {
                        a,
                        b: ResOperand::BinOp(BinOpOperand { op: Operation::Add, a: dst, b }),
                    }),
                    inc_ap,
                    hints: vec![],
                },
                // dst = a / b => a = dst * b
                FeltOperator::Div => Instruction {
                    body: InstructionBody::AssertEq(AssertEqInstruction {
                        a,
                        b: ResOperand::BinOp(BinOpOperand { op: Operation::Mul, a: dst, b }),
                    }),
                    inc_ap,
                    hints: vec![],
                },
            },
            CellExpression::AllocateSegment => Instruction {
                body: InstructionBody::AddAp(AddApInstruction {
                    operand: ResOperand::from(if inc_ap { 1 } else { 0 }),
                }),
                inc_ap: false,
                hints: vec![Hint::AllocSegment { dst }],
            },
        });
        if let Register::FP = dst.register {
            dst.offset += 1;
        }
        if inc_ap {
            ap_change += 1;
        }
    }
    Ok(instructions)
}

/// Handles store_temp for the given type.
fn build_store_temp(
    builder: CompiledInvocationBuilder<'_>,
    ty: &ConcreteTypeId,
) -> Result<CompiledInvocation, InvocationError> {
    let expression = match builder.refs {
        [ReferenceValue { expression, .. }] => expression,
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 1,
                actual: refs.len(),
            });
        }
    };

    let dst = CellRef { register: Register::AP, offset: 0 };
    let instructions = get_store_instructions(&builder, ty, dst, expression, true)?;
    let type_size = builder.program_info.type_sizes[ty];
    Ok(builder.build(
        instructions,
        vec![],
        [ApChange::Known(type_size)].into_iter(),
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
    let (dst_expr, src_expr) = match builder.refs {
        [
            ReferenceValue { expression: dst_expr, .. },
            ReferenceValue { expression: src_expr, .. },
        ] => Ok((dst_expr, src_expr)),
        refs => Err(InvocationError::WrongNumberOfArguments { expected: 2, actual: refs.len() }),
    }?;
    let dst = try_extract_matches!(
        dst_expr
            .try_unpack_single()
            .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?,
        CellExpression::Deref
    )
    .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;
    let instructions = get_store_instructions(&builder, ty, dst, src_expr, false)?;
    let type_size = builder.program_info.type_sizes[ty];
    Ok(builder.build(
        instructions,
        vec![],
        [ApChange::Known(0)].into_iter(),
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

/// Handles a locals alloction finalization instruction.
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
        [ApChange::Known(n_slots)].into_iter(),
        [[].into_iter()].into_iter(),
    ))
}

/// Handles the local variable allocation instruction.
fn build_alloc_local(
    mut builder: CompiledInvocationBuilder<'_>,
    ty: &ConcreteTypeId,
) -> Result<CompiledInvocation, InvocationError> {
    let allocation_size = match builder.program_info.type_sizes.get(ty) {
        Some(0) => Err(InvocationError::NotSized(builder.invocation.clone())),
        Some(size) => Ok(*size),
        _ => Err(InvocationError::NotImplemented(builder.invocation.clone())),
    }?;

    let (slot, frame_state) = frame_state::handle_alloc_local(
        builder.environment.frame_state,
        builder.environment.ap_tracking,
        allocation_size,
    )?;
    builder.environment.frame_state = frame_state;

    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::Deref(CellRef {
            register: Register::FP,
            offset: slot,
        }))]
        .into_iter(),
    ))
}
