use casm::builder::{CasmBuildResult, CasmBuilder};
use casm::operand::{ap_cell_ref, CellRef, DerefOrImmediate, ResOperand};
use casm::{casm, casm_build_extend, casm_extend};
use num_bigint::BigInt;
use sierra::extensions::array::ArrayConcreteLibFunc;
use sierra::extensions::felt::FeltBinaryOperator;
use sierra::extensions::ConcreteLibFunc;
use sierra::ids::ConcreteTypeId;
use sierra_ap_change::core_libfunc_ap_change;
use utils::try_extract_matches;

use super::{
    CompiledInvocation, CompiledInvocationBuilder, InvocationError, ReferenceExpressionView,
};
use crate::invocations::{get_non_fallthrough_statement_id, ProgramInfo};
use crate::references::{
    try_unpack_deref, BinOpExpression, CellExpression, ReferenceExpression, ReferenceValue,
    ReferencesError,
};
use crate::relocations::{Relocation, RelocationEntry};

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &ArrayConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        ArrayConcreteLibFunc::New(_) => build_array_new(builder),
        ArrayConcreteLibFunc::Append(_) => build_array_append(builder),
        ArrayConcreteLibFunc::At(_) => build_array_at(builder),
        ArrayConcreteLibFunc::Len(_) => build_array_len(builder),
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

    Ok(builder.build(
        casm! {
            %{ memory[ap + 0] = segments.add() %}
            ap += 1;
        }
        .instructions,
        vec![],
        [[ReferenceExpression {
            cells: vec![
                CellExpression::Deref(ap_cell_ref(-1)),
                CellExpression::Deref(ap_cell_ref(-1)),
            ],
        }]
        .into_iter()]
        .into_iter(),
    ))
}

/// Handles a Sierra statement for appending an element to an array.
fn build_array_append(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (mut array_view, elem) = match builder.refs {
        [
            ReferenceValue { expression: expr_arr, .. },
            ReferenceValue { expression: expr_elem, .. },
        ] => {
            let concrete_array_type = &builder.libfunc.param_signatures()[0].ty;
            let array_view =
                ArrayView::try_get_view(expr_arr, &builder.program_info, concrete_array_type)
                    .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?;
            (array_view, expr_elem)
        }
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 2,
                actual: refs.len(),
            });
        }
    };
    let mut ctx = casm! {};
    for expr in &elem.cells {
        let cell = try_extract_matches!(expr, CellExpression::Deref)
            .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;
        casm_extend!(ctx, (*cell) = [[&array_view.end] + array_view.end_offset];);
        array_view.end_offset += 1;
    }
    let output_expressions = [vec![array_view.to_reference_expression()].into_iter()].into_iter();
    Ok(builder.build(ctx.instructions, vec![], output_expressions))
}

/// Handles a Sierra statement for fetching an array element at a specific index.
fn build_array_at(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (range_check, array_view, element_size, index) = match builder.refs {
        [
            ReferenceValue { expression: expr_range_check, .. },
            ReferenceValue { expression: expr_arr, .. },
            ReferenceValue { expression: expr_value, .. },
        ] => {
            let concrete_array_type = &builder.libfunc.param_signatures()[0].ty;
            let array_element_size = builder.program_info.type_sizes[concrete_array_type];
            let array_view =
                ArrayView::try_get_view(expr_arr, &builder.program_info, concrete_array_type)
                    .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?;
            let elem_value = match expr_value
                .try_unpack_single()
                .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?
            {
                CellExpression::Deref(op) => DerefOrImmediate::Deref(op),
                CellExpression::Immediate(op) => DerefOrImmediate::from(op),
                _ => return Err(InvocationError::InvalidReferenceExpressionForArgument),
            };
            (try_unpack_deref(expr_range_check)?, array_view, array_element_size, elem_value)
        }
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 3,
                actual: refs.len(),
            });
        }
    };

    if array_view.end_offset != 0 {
        // TODO(Gil): handle when DoubleDeref will support a BinOp variant, e.g. [[ap+1]+1]
        return Err(InvocationError::NotImplemented(builder.invocation.clone()));
    }

    let mut casm_builder = CasmBuilder::default();
    let index = casm_builder.add_var(match index {
        DerefOrImmediate::Immediate(imm) => ResOperand::Immediate(imm),
        DerefOrImmediate::Deref(cell) => ResOperand::Deref(cell),
    });
    let array_start = casm_builder.add_var(ResOperand::Deref(array_view.start));
    let array_end = casm_builder.add_var(ResOperand::Deref(array_view.end));
    let element_size_var = casm_builder.add_var(ResOperand::Immediate(element_size.into()));
    let one = casm_builder.add_var(ResOperand::Immediate(1.into()));
    let range_check = casm_builder.add_var(ResOperand::Deref(range_check));
    casm_build_extend! {casm_builder,
        tempvar array_cell_size;
        // Compute the length of the array (in felts).
        assert array_end = array_cell_size + array_start;
    };
    let element_offset = if element_size == 1 {
        index
    } else {
        casm_build_extend! {casm_builder,
            tempvar element_offset;
            // Compute the length of the array (in felts).
            assert element_offset = index * element_size_var;
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
        tempvar offset_length_diff;
        assert element_offset = offset_length_diff + array_cell_size;
    };
    let array_length = if element_size == 1 {
        array_cell_size
    } else {
        casm_build_extend! {casm_builder,
            // Divide by element size. We assume the length is divisible by element size, and by
            // construction, so is the offset.
            tempvar array_length;
            assert array_cell_size = array_length * element_size_var;
        };
        array_length
    };
    casm_build_extend! {casm_builder,
        // Assert offset - length >= 0.
        assert *(range_check++) = array_length;
        jump FailureHandle;
        InRange:
        // Assert offset < length, or that length-(offset+1) is in [0, 2^128).
        // Compute offset+1.
        tempvar element_offset_plus_1;
        assert element_offset_plus_1 = element_offset + one;
        // Compute length-(offset+1).
        tempvar offset_length_diff;
        assert element_offset_plus_1 = offset_length_diff + array_cell_size;
        // Assert length-(offset+1) is in [0, 2^128).
        assert *(range_check++) = element_offset_plus_1;
        // Compute address of target cell.
        tempvar target_cell;
        assert target_cell = array_start + element_offset;
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
                    CellExpression::Deref(fallthrough_state.get_adjusted_as_cell_ref(array_start)),
                    CellExpression::Deref(fallthrough_state.get_adjusted_as_cell_ref(array_end)),
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
                        label_state["FailureHandle"].get_adjusted_as_cell_ref(array_start),
                    ),
                    CellExpression::Deref(
                        label_state["FailureHandle"].get_adjusted_as_cell_ref(array_end),
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
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let array_view = match builder.refs {
        [ReferenceValue { expression: expr_arr, .. }] => {
            let concrete_array_type = &builder.libfunc.param_signatures()[0].ty;
            ArrayView::try_get_view(expr_arr, &builder.program_info, concrete_array_type)
                .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?
        }
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 1,
                actual: refs.len(),
            });
        }
    };
    if array_view.end_offset != 0 {
        // The array must be stored before calling to array_len, as it is not possible to return
        // [end]-[start]+offset as a CellRef.
        return Err(InvocationError::InvalidReferenceExpressionForArgument);
    }
    let len_ref_expr = ReferenceExpression {
        cells: vec![CellExpression::BinOp(BinOpExpression {
            op: FeltBinaryOperator::Sub,
            a: array_view.end,
            b: DerefOrImmediate::Deref(array_view.start),
        })],
    };

    let output_expressions = [array_view.to_reference_expression(), len_ref_expr].into_iter();
    Ok(builder.build_only_reference_changes(output_expressions))
}

/// A struct representing an actual array value in the Sierra program.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ArrayView {
    /// A ref to the cell in which the start of the array address is stored.
    pub start: CellRef,
    /// A ref to the cell in which the last stored end_of_the_array_address is stored.
    /// The end of the array is the next cell to write to (i.e. \[\[end\] + end_offset\] is not
    /// initialized).
    pub end: CellRef,
    /// The number of elements appended to the array since the last store. The real end of the
    /// array is in the address \[end\] + end_offset.
    /// Never negative.
    pub end_offset: i16,
}

impl ReferenceExpressionView for ArrayView {
    type Error = ReferencesError;

    fn try_get_view(
        expr: &ReferenceExpression,
        _program_info: &ProgramInfo<'_>,
        _concrete_type_id: &ConcreteTypeId,
    ) -> Result<Self, Self::Error> {
        if expr.cells.len() != 2 {
            return Err(ReferencesError::InvalidReferenceTypeForArgument);
        };
        let start = try_extract_matches!(expr.cells[0], CellExpression::Deref)
            .ok_or(ReferencesError::InvalidReferenceTypeForArgument)?;
        let (end, end_offset) = match &expr.cells[1] {
            CellExpression::Deref(op) => (*op, 0),
            CellExpression::BinOp(binop) => {
                if binop.op != FeltBinaryOperator::Add {
                    return Err(ReferencesError::InvalidReferenceTypeForArgument);
                }
                (
                    binop.a,
                    i16::try_from(
                        try_extract_matches!(&binop.b, DerefOrImmediate::Immediate)
                            .ok_or(ReferencesError::InvalidReferenceTypeForArgument)?,
                    )
                    .unwrap(),
                )
            }
            _ => {
                return Err(ReferencesError::InvalidReferenceTypeForArgument);
            }
        };
        Ok(ArrayView { start, end, end_offset })
    }

    fn to_reference_expression(self) -> ReferenceExpression {
        let start_ref = CellExpression::Deref(self.start);
        if self.end_offset == 0 {
            ReferenceExpression { cells: vec![start_ref, CellExpression::Deref(self.end)] }
        } else {
            ReferenceExpression {
                cells: vec![
                    CellExpression::Deref(self.start),
                    CellExpression::BinOp(BinOpExpression {
                        op: FeltBinaryOperator::Add,
                        a: self.end,
                        b: DerefOrImmediate::Immediate(BigInt::from(self.end_offset)),
                    }),
                ],
            }
        }
    }
}
