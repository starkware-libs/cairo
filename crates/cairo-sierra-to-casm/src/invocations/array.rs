use cairo_casm::builder::CasmBuilder;
use cairo_casm::casm_build_extend;
use cairo_casm::operand::{DerefOrImmediate, ResOperand};
use cairo_sierra::extensions::array::ArrayConcreteLibfunc;
use cairo_sierra::extensions::felt::FeltBinaryOperator;
use cairo_sierra::ids::ConcreteTypeId;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::get_non_fallthrough_statement_id;
use crate::references::{BinOpExpression, CellExpression, ReferenceExpression};

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &ArrayConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        ArrayConcreteLibfunc::New(_) => build_array_new(builder),
        ArrayConcreteLibfunc::Append(_) => build_array_append(builder),
        ArrayConcreteLibfunc::PopFront(libfunc) => build_pop_front(&libfunc.ty, builder),
        ArrayConcreteLibfunc::At(libfunc) => build_array_at(&libfunc.ty, builder),
        ArrayConcreteLibfunc::Len(libfunc) => build_array_len(&libfunc.ty, builder),
    }
}

/// Handles a Sierra statement for creating a new array.
fn build_array_new(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    builder.try_get_refs::<0>()?;
    let mut casm_builder = CasmBuilder::default();
    casm_build_extend! {casm_builder,
        tempvar arr_start;
        hint AllocSegment {} into {dst: arr_start};
        ap += 1;
    };
    Ok(builder
        .build_from_casm_builder(casm_builder, [("Fallthrough", &[&[arr_start, arr_start]], None)]))
}

/// Handles a Sierra statement for appending an element to an array.
fn build_array_append(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_arr, elem] = builder.try_get_refs()?;
    let [arr_start, arr_end] = expr_arr.try_unpack()?;
    let arr_start = arr_start.to_buffer(0)?;
    let arr_end = arr_end.to_buffer(elem.cells.len() as i16)?;

    let mut casm_builder = CasmBuilder::default();
    let arr_start = casm_builder.add_var(arr_start);
    let arr_end = casm_builder.add_var(arr_end);
    for cell in &elem.cells {
        let cell = casm_builder.add_var(ResOperand::Deref(cell.to_deref()?));
        casm_build_extend!(casm_builder, assert cell = *(arr_end++););
    }
    Ok(builder
        .build_from_casm_builder(casm_builder, [("Fallthrough", &[&[arr_start, arr_end]], None)]))
}

/// Handles a Sierra statement for popping an element from the begining of an array.
fn build_pop_front(
    elem_ty: &ConcreteTypeId,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [arr_start, arr_end] = builder.try_get_refs::<1>()?[0].try_unpack()?;
    let arr_start = arr_start.to_deref()?;
    let arr_end = arr_end.to_deref()?;
    let element_size = builder.program_info.type_sizes[elem_ty];

    let mut casm_builder = CasmBuilder::default();
    let arr_start = casm_builder.add_var(ResOperand::Deref(arr_start));
    let arr_end = casm_builder.add_var(ResOperand::Deref(arr_end));
    casm_build_extend! {casm_builder,
        tempvar is_non_empty = arr_end - arr_start;
        jump NonEmpty if is_non_empty != 0;
        jump Failure;
        NonEmpty:
        const element_size_imm = element_size;
        let new_start = arr_start + element_size_imm;
    };
    let elem_cells: Vec<_> =
        (0..element_size).map(|i| casm_builder.double_deref(arr_start, i)).collect();
    let failure_handle = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[new_start, arr_end], &elem_cells], None),
            ("Failure", &[&[arr_start, arr_end]], Some(failure_handle)),
        ],
    ))
}

/// Handles a Sierra statement for fetching an array element at a specific index.
fn build_array_at(
    elem_ty: &ConcreteTypeId,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_range_check, expr_arr, expr_index] = builder.try_get_refs()?;
    let range_check = expr_range_check.try_unpack_single()?.to_deref()?;
    let [arr_start, arr_end] = expr_arr.try_unpack()?;
    let arr_start = arr_start.to_deref()?;
    let arr_end = arr_end.to_deref()?;
    let index = expr_index.try_unpack_single()?.to_deref_or_immediate()?;

    let element_size = builder.program_info.type_sizes[elem_ty];

    let mut casm_builder = CasmBuilder::default();
    let index = casm_builder.add_var(match index {
        DerefOrImmediate::Immediate(imm) => ResOperand::Immediate(imm),
        DerefOrImmediate::Deref(cell) => ResOperand::Deref(cell),
    });
    let arr_start = casm_builder.add_var(ResOperand::Deref(arr_start));
    let arr_end = casm_builder.add_var(ResOperand::Deref(arr_end));
    let range_check = casm_builder.add_var(ResOperand::Deref(range_check));
    casm_build_extend! {casm_builder,
        // Compute the length of the array (in felts).
        tempvar array_cell_size = arr_end - arr_start;
    };
    let element_offset = if element_size == 1 {
        index
    } else {
        casm_build_extend! {casm_builder,
            const element_size = element_size;
            // Compute the length of the array (in felts).
            tempvar element_offset = index * element_size;
        };
        element_offset
    };
    casm_build_extend! {casm_builder,
        // Check offset is in range. Note that the offset may be as large as
        // `2^15 * (2^128 - 1)`, but still, `length - offset` is in [0, 2^128) if and only
        // if `offset <= length`.
        tempvar is_in_range;
        hint TestLessThan {lhs: element_offset, rhs: array_cell_size} into {dst: is_in_range};
        jump InRange if is_in_range != 0;
        // Index out of bounds. Compute offset - length.
        tempvar offset_length_diff = element_offset - array_cell_size;
    };
    let array_length = if element_size == 1 {
        array_cell_size
    } else {
        casm_build_extend! {casm_builder,
            // Divide by element size. We assume the length is divisible by element size, and by
            // construction, so is the offset.
            const element_size = element_size;
            tempvar array_length = array_cell_size / element_size;
        };
        array_length
    };
    casm_build_extend! {casm_builder,
        // Assert offset - length >= 0.
        assert array_length = *(range_check++);
        jump FailureHandle;
        InRange:
        // Assert offset < length, or that length-(offset+1) is in [0, 2^128).
        // Compute offset+1.
        const one = 1;
        tempvar element_offset_plus_1 = element_offset + one;
        // Compute length-(offset+1).
        tempvar offset_length_diff = element_offset_plus_1 - array_cell_size;
        // Assert length-(offset+1) is in [0, 2^128).
        assert element_offset_plus_1 = *(range_check++);
        // Compute address of target cell.
        tempvar target_cell = arr_start + element_offset;
    };
    let elem_cells: Vec<_> =
        (0..element_size).map(|i| casm_builder.double_deref(target_cell, i)).collect();
    let failure_handle = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[arr_start, arr_end], &elem_cells], None),
            ("FailureHandle", &[&[range_check], &[arr_start, arr_end]], Some(failure_handle)),
        ],
    ))
}

/// Handles a Sierra statement for getting the length of an array.
fn build_array_len(
    elem_ty: &ConcreteTypeId,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_arr] = builder.try_get_refs()?;
    let [arr_start, arr_end] = expr_arr.try_unpack()?;
    let arr_start = arr_start.to_deref()?;
    let arr_end = arr_end.to_deref()?;

    let element_size = builder.program_info.type_sizes[elem_ty];
    if element_size == 1 {
        let len_ref_expr = ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
            op: FeltBinaryOperator::Sub,
            a: arr_end,
            b: DerefOrImmediate::Deref(arr_start),
        }));
        let output_expressions = [
            ReferenceExpression {
                cells: vec![CellExpression::Deref(arr_start), CellExpression::Deref(arr_end)],
            },
            len_ref_expr,
        ]
        .into_iter();
        return Ok(builder.build_only_reference_changes(output_expressions));
    }
    let mut casm_builder = CasmBuilder::default();
    let start = casm_builder.add_var(ResOperand::Deref(arr_start));
    let end = casm_builder.add_var(ResOperand::Deref(arr_end));
    casm_build_extend! {casm_builder,
        tempvar end_total_offset = end - start;
        const element_size = element_size;
        tempvar length = end_total_offset / element_size;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[start, end], &[length]], None)],
    ))
}
