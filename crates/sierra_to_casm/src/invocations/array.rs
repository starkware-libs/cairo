use casm::builder::{CasmBuildResult, CasmBuilder};
use casm::casm_build_extend;
use casm::operand::{DerefOrImmediate, ResOperand};
use sierra::extensions::array::ArrayConcreteLibFunc;
use sierra::extensions::felt::FeltBinaryOperator;
use sierra::ids::ConcreteTypeId;
use sierra_ap_change::core_libfunc_ap_change;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::get_non_fallthrough_statement_id;
use crate::references::{BinOpExpression, CellExpression, ReferenceExpression, ReferenceValue};
use crate::relocations::{Relocation, RelocationEntry};

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &ArrayConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        ArrayConcreteLibFunc::New(_) => build_array_new(builder),
        ArrayConcreteLibFunc::Append(_) => build_array_append(builder),
        ArrayConcreteLibFunc::At(libfunc) => build_array_at(&libfunc.ty, builder),
        ArrayConcreteLibFunc::Len(libfunc) => build_array_len(&libfunc.ty, builder),
    }
}

/// Handles a Sierra statement for creating a new array.
fn build_array_new(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    if !builder.refs.is_empty() {
        return Err(InvocationError::WrongNumberOfArguments {
            expected: 0,
            actual: builder.refs.len(),
        });
    }
    let mut casm_builder = CasmBuilder::default();
    casm_build_extend! {casm_builder,
        tempvar arr_start;
        hint AllocSegment {} into {dst: arr_start};
        ap += 1;
    };
    let CasmBuildResult { instructions, fallthrough_state, .. } = casm_builder.build();
    // TODO(orizi): Extract the assertion out of the libfunc implementation.
    assert_eq!(
        core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
        [fallthrough_state.ap_change].map(sierra_ap_change::ApChange::Known)
    );
    let arr_start = CellExpression::Deref(fallthrough_state.get_adjusted_as_cell_ref(arr_start));
    Ok(builder.build(
        instructions,
        vec![],
        [[ReferenceExpression { cells: vec![arr_start.clone(), arr_start] }].into_iter()]
            .into_iter(),
    ))
}

/// Handles a Sierra statement for appending an element to an array.
fn build_array_append(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let ((arr_start, arr_end), elem) = match builder.refs {
        [
            ReferenceValue { expression: expr_arr, .. },
            ReferenceValue { expression: expr_elem, .. },
        ] => {
            let [start, end] = &expr_arr.cells[..] else {
                return Err(InvocationError::InvalidReferenceExpressionForArgument);
            };
            ((start.to_buffer(0)?, end.to_buffer(expr_elem.cells.len() as i16)?), expr_elem)
        }
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 2,
                actual: refs.len(),
            });
        }
    };
    let mut casm_builder = CasmBuilder::default();
    let arr_start = casm_builder.add_var(arr_start);
    let arr_end = casm_builder.add_var(arr_end);
    for cell in &elem.cells {
        let cell = casm_builder.add_var(ResOperand::Deref(cell.to_deref()?));
        casm_build_extend!(casm_builder, assert cell = *(arr_end++););
    }
    let CasmBuildResult { instructions, fallthrough_state, .. } = casm_builder.build();
    // TODO(orizi): Extract the assertion out of the libfunc implementation.
    assert_eq!(
        core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
        [fallthrough_state.ap_change].map(sierra_ap_change::ApChange::Known)
    );
    Ok(builder.build(
        instructions,
        vec![],
        [[ReferenceExpression {
            cells: vec![
                CellExpression::from_res_operand(fallthrough_state.get_adjusted(arr_start)),
                CellExpression::from_res_operand(fallthrough_state.get_adjusted(arr_end)),
            ],
        }]
        .into_iter()]
        .into_iter(),
    ))
}

/// Handles a Sierra statement for fetching an array element at a specific index.
fn build_array_at(
    elem_ty: &ConcreteTypeId,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (range_check, (arr_start, arr_end), index_deref_or_imm) = match builder.refs {
        [
            ReferenceValue { expression: expr_range_check, .. },
            ReferenceValue { expression: expr_arr, .. },
            ReferenceValue { expression: expr_index, .. },
        ] => {
            let [start, end] = &expr_arr.cells[..] else {
                return Err(InvocationError::InvalidReferenceExpressionForArgument);
            };
            let index_deref_or_imm = match expr_index.try_unpack_single()? {
                CellExpression::Deref(op) => DerefOrImmediate::Deref(op),
                CellExpression::Immediate(op) => DerefOrImmediate::from(op),
                _ => return Err(InvocationError::InvalidReferenceExpressionForArgument),
            };
            (
                expr_range_check.try_unpack_single()?.to_deref()?,
                (start.to_deref()?, end.to_deref()?),
                index_deref_or_imm,
            )
        }
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 3,
                actual: refs.len(),
            });
        }
    };
    let element_size = builder.program_info.type_sizes[elem_ty];

    let mut casm_builder = CasmBuilder::default();
    let index = casm_builder.add_var(match index_deref_or_imm {
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
    let CasmBuildResult { instructions, awaiting_relocations, label_state, fallthrough_state } =
        casm_builder.build();
    // TODO(orizi): Extract the assertion out of the libfunc implementation.
    assert_eq!(
        core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
        [fallthrough_state.ap_change, label_state["FailureHandle"].ap_change]
            .map(sierra_ap_change::ApChange::Known)
    );
    let [relocation_index] = &awaiting_relocations[..] else { panic!("Malformed casm builder usage.") };
    let relocations = vec![RelocationEntry {
        instruction_idx: *relocation_index,
        relocation: Relocation::RelativeStatementId(get_non_fallthrough_statement_id(&builder)),
    }];
    let output_expressions = [
        vec![
            ReferenceExpression::from_cell(CellExpression::from_res_operand(
                fallthrough_state.get_adjusted(range_check),
            )),
            ReferenceExpression {
                cells: vec![
                    CellExpression::Deref(fallthrough_state.get_adjusted_as_cell_ref(arr_start)),
                    CellExpression::Deref(fallthrough_state.get_adjusted_as_cell_ref(arr_end)),
                ],
            },
            ReferenceExpression {
                cells: (0..element_size)
                    .map(|i| {
                        CellExpression::DoubleDeref(
                            fallthrough_state.get_adjusted_as_cell_ref(target_cell),
                            i,
                        )
                    })
                    .collect(),
            },
        ]
        .into_iter(),
        vec![
            ReferenceExpression::from_cell(CellExpression::from_res_operand(
                label_state["FailureHandle"].get_adjusted(range_check),
            )),
            ReferenceExpression {
                cells: vec![
                    CellExpression::Deref(
                        label_state["FailureHandle"].get_adjusted_as_cell_ref(arr_start),
                    ),
                    CellExpression::Deref(
                        label_state["FailureHandle"].get_adjusted_as_cell_ref(arr_end),
                    ),
                ],
            },
        ]
        .into_iter(),
    ]
    .into_iter();
    Ok(builder.build(instructions, relocations, output_expressions))
}

/// Handles a Sierra statement for getting the length of an array.
fn build_array_len(
    elem_ty: &ConcreteTypeId,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (arr_start, arr_end) = match builder.refs {
        [ReferenceValue { expression: expr_arr, .. }] => {
            let [start, end] = &expr_arr.cells[..] else {
                return Err(InvocationError::InvalidReferenceExpressionForArgument);
            };
            (start.to_deref()?, end.to_deref()?)
        }
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 1,
                actual: refs.len(),
            });
        }
    };

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
    let CasmBuildResult { instructions, fallthrough_state, .. } = casm_builder.build();
    // TODO(orizi): Extract the assertion out of the libfunc implementation.
    assert_eq!(
        core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
        [fallthrough_state.ap_change].map(sierra_ap_change::ApChange::Known)
    );
    let output_expressions = [vec![
        ReferenceExpression {
            cells: vec![
                CellExpression::Deref(fallthrough_state.get_adjusted_as_cell_ref(start)),
                CellExpression::Deref(fallthrough_state.get_adjusted_as_cell_ref(end)),
            ],
        },
        ReferenceExpression::from_cell(CellExpression::Deref(
            fallthrough_state.get_adjusted_as_cell_ref(length),
        )),
    ]
    .into_iter()]
    .into_iter();
    Ok(builder.build(instructions, vec![], output_expressions))
}
