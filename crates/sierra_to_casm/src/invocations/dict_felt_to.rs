use std::vec;

use casm::ap_change::ApplyApChange;
use casm::casm;
use casm::hints::Hint;
use casm::instructions::{AddApInstruction, Instruction, InstructionBody};
use casm::operand::{CellRef, DerefOrImmediate, Register, ResOperand};
use num_bigint::ToBigInt;
use sierra::extensions::dict_felt_to::DictFeltToConcreteLibFunc;
use sierra::extensions::felt::FeltOperator;
use sierra::extensions::ConcreteLibFunc;
use sierra::ids::ConcreteTypeId;
use utils::try_extract_matches;

use super::{
    CompiledInvocation, CompiledInvocationBuilder, InvocationError, ProgramInfo,
    ReferenceExpressionView,
};
use crate::references::{
    try_unpack_deref, BinOpExpression, CellExpression, ReferenceExpression, ReferenceValue,
    ReferencesError,
};

/// Builds instructions for Sierra single cell dict operations.
pub fn build(
    libfunc: &DictFeltToConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        DictFeltToConcreteLibFunc::New(_) => build_dict_felt_to_new(builder),
        DictFeltToConcreteLibFunc::Read(_) => build_dict_felt_to_read(builder),
        DictFeltToConcreteLibFunc::Write(_) => build_dict_felt_to_write(builder),
    }
}

/// Handles instruction for creating a new single cell dict.
fn build_dict_felt_to_new(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let default_value = match builder.refs {
        [ReferenceValue { expression: expr_default_value, .. }] => {
            try_unpack_deref(expr_default_value)?
        }
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 1,
                actual: refs.len(),
            });
        }
    };

    Ok(builder.build(
        vec![Instruction {
            body: InstructionBody::AddAp(AddApInstruction { operand: ResOperand::from(1) }),
            inc_ap: false,
            hints: vec![Hint::AllocDictFeltTo {
                dst: CellRef { register: Register::AP, offset: 0 },
                default_value,
            }],
        }],
        vec![],
        [[ReferenceExpression {
            cells: vec![
                CellExpression::Deref(CellRef { register: Register::AP, offset: -1 }),
                CellExpression::Deref(CellRef { register: Register::AP, offset: -1 }),
            ],
        }]
        .into_iter()]
        .into_iter(),
    ))
}

/// Handles instruction for reading from a single cell dict.
fn build_dict_felt_to_read(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (mut dict_view, mut key) = match builder.refs {
        [
            ReferenceValue { expression: expr_dict, .. },
            ReferenceValue { expression: expr_key, .. },
        ] => {
            let concrete_dict_type = &builder.libfunc.param_signatures()[0].ty;
            let dict_view =
                DictFeltToView::try_get_view(expr_dict, &builder.program_info, concrete_dict_type)
                    .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?;
            let key = try_unpack_deref(expr_key)?;
            (dict_view, key)
        }
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 2,
                actual: refs.len(),
            });
        }
    };
    let mut instructions = vec![Instruction {
        body: InstructionBody::AddAp(AddApInstruction { operand: ResOperand::from(1) }),
        inc_ap: false,
        hints: vec![Hint::DictFeltToRead {
            dict_ptr: dict_view.end,
            dict_offset: dict_view.end_offset,
            value_dst: CellRef { register: Register::AP, offset: 0 },
            key,
        }],
    }];
    // Correct references for the stack changes in the hint above.
    let ap_change = 1;
    key = key.unchecked_apply_known_ap_change(ap_change);
    dict_view = dict_view.unchecked_apply_known_ap_change(ap_change);
    instructions.extend(
        DictFeltToAccess {
            key,
            prev_value: CellRef { register: Register::AP, offset: -1 },
            new_value: CellRef { register: Register::AP, offset: -1 },
        }
        .get_instructions(&dict_view),
    );
    dict_view.end_offset += DictFeltToAccess::size() as u16;

    Ok(builder.build(
        instructions,
        vec![],
        [[
            dict_view.to_reference_expression(),
            ReferenceExpression {
                cells: vec![CellExpression::Deref(CellRef { register: Register::AP, offset: -1 })],
            },
        ]
        .into_iter()]
        .into_iter(),
    ))
}

/// Handles instruction for writing to a single cell dict.
fn build_dict_felt_to_write(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (mut dict_view, mut key, mut value) = match builder.refs {
        [
            ReferenceValue { expression: expr_dict, .. },
            ReferenceValue { expression: expr_key, .. },
            ReferenceValue { expression: expr_value, .. },
        ] => {
            let concrete_dict_type = &builder.libfunc.param_signatures()[0].ty;
            let dict_view =
                DictFeltToView::try_get_view(expr_dict, &builder.program_info, concrete_dict_type)
                    .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?;
            let key = try_unpack_deref(expr_key)?;
            let value = try_unpack_deref(expr_value)?;
            (dict_view, key, value)
        }
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 3,
                actual: refs.len(),
            });
        }
    };
    let mut instructions = vec![Instruction {
        body: InstructionBody::AddAp(AddApInstruction { operand: ResOperand::from(1) }),
        inc_ap: false,
        hints: vec![Hint::DictFeltToWrite {
            dict_ptr: dict_view.end,
            dict_offset: dict_view.end_offset,
            key,
            value,
            prev_value_dst: CellRef { register: Register::AP, offset: 0 },
        }],
    }];
    // Correct references for the stack changes in the hint above.
    let ap_change = 1;
    key = key.unchecked_apply_known_ap_change(ap_change);
    value = value.unchecked_apply_known_ap_change(ap_change);
    dict_view = dict_view.unchecked_apply_known_ap_change(ap_change);
    instructions.extend(
        DictFeltToAccess {
            key,
            prev_value: CellRef { register: Register::AP, offset: -1 },
            new_value: value,
        }
        .get_instructions(&dict_view),
    );
    dict_view.end_offset += DictFeltToAccess::size() as u16;
    Ok(builder.build(
        instructions,
        vec![],
        [[dict_view.to_reference_expression()].into_iter()].into_iter(),
    ))
}

/// A struct representing a dict in the Sierra program.
/// A dictionary is implemented as a list of changes in the form (key, prev_value, new_value)
/// stored in a segemnt of its own. A dictionary reference expression is therefore represented using
/// two values:
/// 1) A reference to a cell containing the start of the list address, stored by the dictionary
/// segment allocation hint.
/// 2) End of the list, start as a reference to the same cell as end of the list and insert
/// operations update it.Can be a reference to a stored cell or a BinOp add expression, which
/// indicates an offset relative to a stored end value.
// TODO(Gil): Dict is just a specific use case of appendable array, consider using ArrayView.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DictFeltToView {
    /// A ref to the cell in which the start address of the dict change list is stored.
    pub start: CellRef,
    /// A ref to the cell in which the last stored end_of_the_dict_change_list is stored.
    /// The end of the list is the next cell to write a change into (i.e. \[\[end\] + end_offset\]
    /// is not initialized).
    pub end: CellRef,
    /// The number of elements appended to the change_list since the last store. The real end of
    /// the change list is in the address \[end\] + end_offset.
    pub end_offset: u16,
}

impl ReferenceExpressionView for DictFeltToView {
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
        Ok(DictFeltToView { start, end, end_offset })
    }

    fn to_reference_expression(self) -> ReferenceExpression {
        let start_ref = CellExpression::Deref(self.start);
        if self.end_offset == 0 {
            ReferenceExpression { cells: vec![start_ref, CellExpression::Deref(self.end)] }
        } else {
            ReferenceExpression {
                cells: vec![
                    CellExpression::Deref(self.end),
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

impl ApplyApChange for DictFeltToView {
    fn apply_known_ap_change(self, ap_change: usize) -> Option<Self> {
        Some(DictFeltToView {
            start: self.start.apply_known_ap_change(ap_change)?,
            end: self.end.apply_known_ap_change(ap_change)?,
            end_offset: self.end_offset,
        })
    }

    fn can_apply_unknown(&self) -> bool {
        self.start.can_apply_unknown() && self.end.can_apply_unknown()
    }
}

/// Represents a read/write access to the dict.
struct DictFeltToAccess {
    key: CellRef,
    prev_value: CellRef,
    new_value: CellRef,
}

impl DictFeltToAccess {
    /// Returns a set of instructions for storing the dict access data into the set of consecutive
    /// cells at the end of the dict_segment.
    fn get_instructions(&self, dict_view: &DictFeltToView) -> Vec<Instruction> {
        // TODO(Gil): Try to avoid the following assignments.
        let key = self.key;
        let prev_value = self.prev_value;
        let new_value = self.new_value;
        casm! {
           key = [[dict_view.end] + dict_view.end_offset as i16];
           prev_value = [[dict_view.end] + (dict_view.end_offset + 1) as i16];
           new_value = [[dict_view.end] + (dict_view.end_offset + 2) as i16];
        }
        .instructions
    }
    /// Returns the number casm cells representing a DictAccess.
    fn size() -> usize {
        3
    }
}
