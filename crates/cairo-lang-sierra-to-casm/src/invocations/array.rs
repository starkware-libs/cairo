use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::array::ArrayConcreteLibfunc;
use cairo_lang_sierra::ids::ConcreteTypeId;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{add_input_variables, get_non_fallthrough_statement_id};

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &ArrayConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        ArrayConcreteLibfunc::New(_) => build_array_new(builder),
        ArrayConcreteLibfunc::Append(_) => build_array_append(builder),
        ArrayConcreteLibfunc::PopFront(libfunc) => build_pop_front(&libfunc.ty, builder),
        ArrayConcreteLibfunc::At(libfunc) => build_array_get(&libfunc.ty, builder),
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

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(0) arr_start;
        buffer(elem.cells.len() as i16) arr_end;
    };
    for cell in &elem.cells {
        add_input_variables!(casm_builder, deref cell;);
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
    let element_size = builder.program_info.type_sizes[elem_ty];

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref arr_start;
        deref arr_end;
    };
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
fn build_array_get(
    elem_ty: &ConcreteTypeId,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_range_check, expr_arr, expr_index] = builder.try_get_refs()?;
    let range_check = expr_range_check.try_unpack_single()?;
    let [arr_start, arr_end] = expr_arr.try_unpack()?;
    let index = expr_index.try_unpack_single()?;

    let element_size = builder.program_info.type_sizes[elem_ty];

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref_or_immediate index;
        deref arr_start;
        deref arr_end;
        deref range_check;
    };
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
    let [arr_start, arr_end] = builder.try_get_refs::<1>()?[0].try_unpack()?;
    let element_size = builder.program_info.type_sizes[elem_ty];
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref arr_start;
        deref arr_end;
    };
    let length = if element_size == 1 {
        casm_build_extend! {casm_builder,
            let length = arr_end - arr_start;
        };
        length
    } else {
        casm_build_extend! {casm_builder,
            tempvar end_total_offset = arr_end - arr_start;
            const element_size = element_size;
            let length = end_total_offset / element_size;
        };
        length
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[arr_start, arr_end], &[length]], None)],
    ))
}
