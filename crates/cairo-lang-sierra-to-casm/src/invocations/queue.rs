use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::queue::QueueConcreteLibfunc;
use cairo_lang_sierra::ids::ConcreteTypeId;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{
    add_input_variables, get_non_fallthrough_statement_id, CostValidationInfo,
};

/// Builds instructions for Sierra queue operations.
pub fn build(
    libfunc: &QueueConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        QueueConcreteLibfunc::New(_) => build_queue_new(builder),
        QueueConcreteLibfunc::Append(_) => build_queue_append(builder),
        QueueConcreteLibfunc::PopFront(libfunc) => build_pop_front(&libfunc.ty, builder),
        QueueConcreteLibfunc::Get(libfunc) => build_queue_get(&libfunc.ty, builder),
        QueueConcreteLibfunc::Len(libfunc) => build_queue_len(&libfunc.ty, builder),
    }
}

/// Handles a Sierra statement for creating a new queue.
fn build_queue_new(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    builder.try_get_refs::<0>()?;
    let mut casm_builder = CasmBuilder::default();
    casm_build_extend! {casm_builder,
        tempvar queue_start;
        hint AllocSegment {} into {dst: queue_start};
        ap += 1;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[queue_start, queue_start]], None)],
        Default::default(),
    ))
}

/// Handles a Sierra statement for appending an element to a queue.
fn build_queue_append(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_queue, elem] = builder.try_get_refs()?;
    let [queue_start, queue_end] = expr_queue.try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(0) queue_start;
        buffer(elem.cells.len() as i16) queue_end;
    };
    for cell in &elem.cells {
        add_input_variables!(casm_builder, deref cell;);
        casm_build_extend!(casm_builder, assert cell = *(queue_end++););
    }
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[queue_start, queue_end]], None)],
        Default::default(),
    ))
}

/// Handles a Sierra statement for popping an element from the begining of a queue.
fn build_pop_front(
    elem_ty: &ConcreteTypeId,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [queue_start, queue_end] = builder.try_get_refs::<1>()?[0].try_unpack()?;
    let element_size = builder.program_info.type_sizes[elem_ty];

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref queue_start;
        deref queue_end;
    };
    casm_build_extend! {casm_builder,
        tempvar is_non_empty = queue_end - queue_start;
        jump NonEmpty if is_non_empty != 0;
        jump Failure;
        NonEmpty:
        const element_size_imm = element_size;
        let new_start = queue_start + element_size_imm;
    };
    let elem_cells: Vec<_> =
        (0..element_size).map(|i| casm_builder.double_deref(queue_start, i)).collect();
    let failure_handle = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[new_start, queue_end], &elem_cells], None),
            ("Failure", &[&[queue_start, queue_end]], Some(failure_handle)),
        ],
        Default::default(),
    ))
}

/// Handles a Sierra statement for fetching a queue element at a specific index.
fn build_queue_get(
    elem_ty: &ConcreteTypeId,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_range_check, expr_queue, expr_index] = builder.try_get_refs()?;
    let range_check = expr_range_check.try_unpack_single()?;
    let [queue_start, queue_end] = expr_queue.try_unpack()?;
    let index = expr_index.try_unpack_single()?;

    let element_size = builder.program_info.type_sizes[elem_ty];

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref_or_immediate index;
        deref queue_start;
        deref queue_end;
        deref range_check;
    };
    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        // Compute the length of the queue (in felts).
        tempvar queue_cell_size = queue_end - queue_start;
    };
    let element_offset = if element_size == 1 {
        index
    } else {
        casm_build_extend! {casm_builder,
            const element_size = element_size;
            // Compute the length of the queue (in felts).
            tempvar element_offset = index * element_size;
        };
        element_offset
    };
    casm_build_extend! {casm_builder,
        // Check offset is in range. Note that the offset may be as large as
        // `2^15 * (2^128 - 1)`, but still, `length - offset` is in [0, 2^128) if and only
        // if `offset <= length`.
        tempvar is_in_range;
        hint TestLessThan {lhs: element_offset, rhs: queue_cell_size} into {dst: is_in_range};
        jump InRange if is_in_range != 0;
        // Index out of bounds. Compute offset - length.
        tempvar offset_length_diff = element_offset - queue_cell_size;
    };
    let queue_length = if element_size == 1 {
        queue_cell_size
    } else {
        casm_build_extend! {casm_builder,
            // Divide by element size. We assume the length is divisible by element size, and by
            // construction, so is the offset.
            const element_size = element_size;
            tempvar queue_length = queue_cell_size / element_size;
        };
        queue_length
    };
    casm_build_extend! {casm_builder,
        // Assert offset - length >= 0.
        assert queue_length = *(range_check++);
        jump FailureHandle;
        InRange:
        // Assert offset < length, or that length-(offset+1) is in [0, 2^128).
        // Compute offset+1.
        const one = 1;
        tempvar element_offset_plus_1 = element_offset + one;
        // Compute length-(offset+1).
        tempvar offset_length_diff = element_offset_plus_1 - queue_cell_size;
        // Assert length-(offset+1) is in [0, 2^128).
        assert element_offset_plus_1 = *(range_check++);
        // Compute address of target cell.
        tempvar target_cell = queue_start + element_offset;
    };
    let elem_cells: Vec<_> =
        (0..element_size).map(|i| casm_builder.double_deref(target_cell, i)).collect();
    let failure_handle = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[queue_start, queue_end], &elem_cells], None),
            ("FailureHandle", &[&[range_check], &[queue_start, queue_end]], Some(failure_handle)),
        ],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}

/// Handles a Sierra statement for getting the length of a queue.
fn build_queue_len(
    elem_ty: &ConcreteTypeId,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [queue_start, queue_end] = builder.try_get_refs::<1>()?[0].try_unpack()?;
    let element_size = builder.program_info.type_sizes[elem_ty];
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref queue_start;
        deref queue_end;
    };
    let length = if element_size == 1 {
        casm_build_extend! {casm_builder,
            let length = queue_end - queue_start;
        };
        length
    } else {
        casm_build_extend! {casm_builder,
            tempvar end_total_offset = queue_end - queue_start;
            const element_size = element_size;
            let length = end_total_offset / element_size;
        };
        length
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[length]], None)],
        Default::default(),
    ))
}
