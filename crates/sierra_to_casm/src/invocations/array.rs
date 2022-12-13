use casm::ap_change::ApplyApChange;
use casm::casm;
use casm::operand::{ap_cell_ref, CellRef, DerefOrImmediate};
use itertools::chain;
use num_bigint::BigInt;
use sierra::extensions::array::ArrayConcreteLibFunc;
use sierra::extensions::felt::FeltBinaryOperator;
use sierra::extensions::ConcreteLibFunc;
use sierra::ids::ConcreteTypeId;
use utils::try_extract_matches;

use super::{
    patch_jnz_to_end, CompiledInvocation, CompiledInvocationBuilder, InvocationError,
    ReferenceExpressionView,
};
use crate::invocations::{get_bool_comparison_target_statement_id, ProgramInfo};
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
    let (mut array_view, element_to_append) = match builder.refs {
        [
            ReferenceValue { expression: expr_arr, .. },
            ReferenceValue { expression: expr_elem, .. },
        ] => {
            let concrete_array_type = &builder.libfunc.param_signatures()[0].ty;
            let array_view =
                ArrayView::try_get_view(expr_arr, &builder.program_info, concrete_array_type)
                    .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?;
            let elem_val = match expr_elem
                .try_unpack_single()
                .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?
            {
                CellExpression::Deref(op) => DerefOrImmediate::Deref(op),
                CellExpression::Immediate(op) => DerefOrImmediate::from(op),
                _ => return Err(InvocationError::InvalidReferenceExpressionForArgument),
            };
            (array_view, elem_val)
        }
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 2,
                actual: refs.len(),
            });
        }
    };
    if array_view.end_offset != 0 {
        // TODO(Gil): handle when DoubleDeref will support a BinOp variant, e.g. [[ap+1]+1]
        return Err(InvocationError::NotImplemented(builder.invocation.clone()));
    }
    match element_to_append {
        DerefOrImmediate::Immediate(_) => {
            // TODO(Gil): handle when assertion of immediate to DoubleDeref (e.g. [[ap+0]] = 1)
            // will be supported.
            Err(InvocationError::NotImplemented(builder.invocation.clone()))
        }
        DerefOrImmediate::Deref(op) => {
            let instructions = casm! { op = [[array_view.end]]; }.instructions;
            array_view.end_offset += 1;
            let output_expressions =
                [vec![array_view.to_reference_expression()].into_iter()].into_iter();
            Ok(builder.build(instructions, vec![], output_expressions))
        }
    }
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

    match index {
        DerefOrImmediate::Immediate(_) => {
            // TODO(Gil): handle when assertion of immediate to DoubleDeref (e.g. [[ap+0]] = 1)
            // will be supported.
            Err(InvocationError::NotImplemented(builder.invocation.clone()))
        }
        DerefOrImmediate::Deref(index_cell) => {
            // TODO(dorimedini): Optimize for the case element_size==1.
            let mut index_out_of_bounds_branch = casm! {
                // Compute the length of the array (in felts).
                (array_view.end) = [ap + 0] + (array_view.start), ap++;
                // Compute the element offset (in felts).
                [ap + 0] = (index_cell.unchecked_apply_known_ap_change(1)) * (element_size), ap++;
                // Check offset is in range. Note that the offset may be as large as
                // `2^15 * (2^128 - 1)`, but still, `length - offset` is in [0, 2^128) if and only
                // if `offset <= length`.
                %{ memory[ap + 0] = memory[ap + -1] < memory[ap + -2] %}
                jmp rel 0 if [ap + 0] != 0, ap++;
                // Index out of bounds. Compute offset - length.
                [ap + -2] = [ap + 0] + [ap + -3], ap++;
                // Divide by element size. We assume the length is divisible by element size, and by
                // construction, so is the offset.
                [ap + -1] = [ap + 0] * (element_size), ap++;
                // Assert offset - length >= 0.
                [ap + -1] = [[(range_check.unchecked_apply_known_ap_change(5))]];
                jmp rel 0;
            };
            let success_branch = casm! {
                // Assert offset < length, or that length-(offset+1) is in [0, 2^128).
                // Compute offset+1.
                [ap + 0] = [ap + -2] + 1, ap++;
                // Compute length-(offset+1).
                [ap + -4] = [ap + 0] + [ap + -1], ap++;
                // Assert length-(offset+1) is in [0, 2^128).
                [ap + -1] = [[(range_check.unchecked_apply_known_ap_change(5))]];
                // Compute address of target cell.
                [ap + 0] = (array_view.start.unchecked_apply_known_ap_change(5)) + [ap + -4], ap++;
            };

            // Backpatch the JNZ target. It's the second instruction.
            patch_jnz_to_end(&mut index_out_of_bounds_branch, 2);

            // First (success) branch has AP change 4, second (failure) has AP change 3.
            // Both branches invoke range check once. Success branch also outputs the double-deref
            // (array cell value).
            let relocation_idx = index_out_of_bounds_branch.instructions.len() - 1;
            let instructions =
                chain!(index_out_of_bounds_branch.instructions, success_branch.instructions)
                    .collect();
            let array_ref = array_view.to_reference_expression();
            let relocations = vec![RelocationEntry {
                instruction_idx: relocation_idx,
                relocation: Relocation::RelativeStatementId(
                    get_bool_comparison_target_statement_id(&builder),
                ),
            }];
            let output_expressions = [
                vec![
                    ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                        op: FeltBinaryOperator::Add,
                        a: range_check.unchecked_apply_known_ap_change(6),
                        b: DerefOrImmediate::from(1),
                    })),
                    array_ref.clone(),
                    ReferenceExpression::from_cell(CellExpression::DoubleDeref(ap_cell_ref(-1), 0)),
                ]
                .into_iter(),
                vec![
                    ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                        op: FeltBinaryOperator::Add,
                        a: range_check.unchecked_apply_known_ap_change(5),
                        b: DerefOrImmediate::from(1),
                    })),
                    array_ref,
                ]
                .into_iter(),
            ]
            .into_iter();
            Ok(builder.build(instructions, relocations, output_expressions))
        }
    }
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
