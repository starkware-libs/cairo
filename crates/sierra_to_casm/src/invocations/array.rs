use casm::ap_change::ApplyApChange;
use casm::casm;
use casm::operand::{ap_cell_ref, CellRef, DerefOrImmediate};
use itertools::chain;
use num_bigint::ToBigInt;
use sierra::extensions::array::ArrayConcreteLibFunc;
use sierra::extensions::felt::FeltOperator;
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
    }
}

/// Handles instruction for creating a new array.
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

/// Handles instruction for appending an element to an array.
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

/// Handles instruction for fetching an array element at a specific index.
fn build_array_at(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (range_check, array_view, index) = match builder.refs {
        [
            ReferenceValue { expression: expr_range_check, .. },
            ReferenceValue { expression: expr_arr, .. },
            ReferenceValue { expression: expr_value, .. },
        ] => {
            let concrete_array_type = &builder.libfunc.param_signatures()[0].ty;
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
            (try_unpack_deref(expr_range_check)?, array_view, elem_value)
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
            let mut index_out_of_bounds_branch = casm! {
                // Compute the length of the array: [ap + 0]=end-start. The index is a uint128 type
                // so there are no overflow issues.
                (array_view.start) = [ap + 0] + (array_view.end), ap++;
                // Check index is in range.
                %{ memory[ap + 0] = index < memory[ap + -1] %}
                jmp rel 0 if [ap + 0] != 0, ap++;
                // Index out of bounds.
                // Compute index - length.
                (index_cell.unchecked_apply_known_ap_change(2)) = [ap + 0] + [ap + -2], ap++;
                // Assert index - length >= 0.
                [ap + -1] = [[(range_check.unchecked_apply_known_ap_change(3))]];
                jmp rel 0;
            };
            let success_branch = casm! {
                // Assert index < length, or that length-(index+1) is in [0, 2^128).
                // Compute index+1.
                [ap + 0] = (index_cell.unchecked_apply_known_ap_change(2)) + 1, ap++;
                // Compute length-(index+1).
                [ap + -3] = [ap + 0] + [ap + -1], ap++;
                // Assert length-(index+1) is in [0, 2^128).
                [ap + -1] = [[(range_check.unchecked_apply_known_ap_change(4))]];
                // Compute address of target cell.
                [ap + 0] = (array_view.start.unchecked_apply_known_ap_change(4)) + (index_cell.unchecked_apply_known_ap_change(4)), ap++;
            };

            // Backpatch the JNZ target. It's the second instruction.
            patch_jnz_to_end(&mut index_out_of_bounds_branch, 1);

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
                        op: FeltOperator::Add,
                        a: range_check.unchecked_apply_known_ap_change(4),
                        b: DerefOrImmediate::from(1),
                    })),
                    array_ref.clone(),
                    ReferenceExpression::from_cell(CellExpression::DoubleDeref(ap_cell_ref(-1))),
                ]
                .into_iter(),
                vec![
                    ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                        op: FeltOperator::Add,
                        a: range_check.unchecked_apply_known_ap_change(3),
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
    pub end_offset: u16,
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
            CellExpression::Deref(op) => (*op, 0u16),
            CellExpression::BinOp(binop) => {
                if binop.op != FeltOperator::Add {
                    return Err(ReferencesError::InvalidReferenceTypeForArgument);
                }
                (
                    binop.a,
                    u16::try_from(
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
                        op: FeltOperator::Add,
                        a: self.end,
                        b: DerefOrImmediate::Immediate(self.end_offset.to_bigint().unwrap()),
                    }),
                ],
            }
        }
    }
}
