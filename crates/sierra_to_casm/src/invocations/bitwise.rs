use casm::builder::{CasmBuildResult, CasmBuilder};
use casm::casm_build_extend;
use casm::operand::{BinOpOperand, DerefOrImmediate, Operation, ResOperand};
use num_bigint::BigInt;
use sierra::extensions::bitwise::BitwiseConcreteLibFunc;
use sierra_ap_change::core_libfunc_ap_change;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{
    try_unpack_deref, try_unpack_deref_with_offset, CellExpression, ReferenceExpression,
    ReferenceValue,
};

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &BitwiseConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        BitwiseConcreteLibFunc::Do(_) => build_bitwise(builder),
    }
}

/// Handles instruction for appending an element to an array.
fn build_bitwise(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let ((bitwise_base, bitwise_offset), x, y) = match builder.refs {
        [
            ReferenceValue { expression: expr_bitwise, .. },
            ReferenceValue { expression: expr_x, .. },
            ReferenceValue { expression: expr_y, .. },
        ] => (
            try_unpack_deref_with_offset(expr_bitwise)?,
            try_unpack_deref(expr_x)?,
            try_unpack_deref(expr_y)?,
        ),
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 3,
                actual: refs.len(),
            });
        }
    };

    if bitwise_offset > i16::MAX - 2 {
        return Err(InvocationError::InvalidReferenceExpressionForArgument);
    }

    let mut casm_builder = CasmBuilder::default();
    let x = casm_builder.add_var(ResOperand::Deref(x));
    let y = casm_builder.add_var(ResOperand::Deref(y));
    let x_bitwise_cell = casm_builder.add_var(ResOperand::BinOp(BinOpOperand {
        op: Operation::Add,
        a: bitwise_base,
        b: DerefOrImmediate::Immediate(BigInt::from(bitwise_offset)),
    }));
    let y_bitwise_cell = casm_builder.add_var(ResOperand::BinOp(BinOpOperand {
        op: Operation::Add,
        a: bitwise_base,
        b: DerefOrImmediate::Immediate(BigInt::from(bitwise_offset + 1)),
    }));
    let and_bitwise_cell = casm_builder.add_var(ResOperand::BinOp(BinOpOperand {
        op: Operation::Add,
        a: bitwise_base,
        b: DerefOrImmediate::Immediate(BigInt::from(bitwise_offset + 2)),
    }));
    let or_bitwise_cell = casm_builder.add_var(ResOperand::BinOp(BinOpOperand {
        op: Operation::Add,
        a: bitwise_base,
        b: DerefOrImmediate::Immediate(BigInt::from(bitwise_offset + 3)),
    }));
    let xor_bitwise_cell = casm_builder.add_var(ResOperand::BinOp(BinOpOperand {
        op: Operation::Add,
        a: bitwise_base,
        b: DerefOrImmediate::Immediate(BigInt::from(bitwise_offset + 4)),
    }));
    let next_bitwise_cell = casm_builder.add_var(ResOperand::BinOp(BinOpOperand {
        op: Operation::Add,
        a: bitwise_base,
        b: DerefOrImmediate::Immediate(BigInt::from(bitwise_offset + 5)),
    }));
    casm_build_extend! {casm_builder,
        assert x = x_bitwise_cell;
        assert y = y_bitwise_cell;
    };
    casm_builder.add_bitwise(x_bitwise_cell);

    let CasmBuildResult { instructions, awaiting_relocations, label_state: _, fallthrough_state } =
        casm_builder.build();

    // TODO(orizi): Extract the assertion out of the libfunc implementation.
    assert_eq!(
        core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
        [fallthrough_state.ap_change].map(sierra_ap_change::ApChange::Known)
    );

    if !awaiting_relocations.is_empty() {
        panic!("No relocations in bitwise!");
    }

    let output_expressions = [vec![
        ReferenceExpression::from_cell(CellExpression::from_res_operand(
            fallthrough_state.get_adjusted(next_bitwise_cell),
        )),
        ReferenceExpression::from_cell(CellExpression::Deref(
            fallthrough_state.get_adjusted_as_cell_ref(and_bitwise_cell),
        )),
        ReferenceExpression::from_cell(CellExpression::Deref(
            fallthrough_state.get_adjusted_as_cell_ref(or_bitwise_cell),
        )),
        ReferenceExpression::from_cell(CellExpression::Deref(
            fallthrough_state.get_adjusted_as_cell_ref(xor_bitwise_cell),
        )),
    ]
    .into_iter()]
    .into_iter();
    Ok(builder.build(instructions, vec![], output_expressions))
}
