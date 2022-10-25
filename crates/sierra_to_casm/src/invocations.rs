use std::collections::VecDeque;

use casm::ap_change::{ApChange, ApplyApChange};
use casm::casm;
use casm::hints::Hint;
use casm::instructions::{AddApInstruction, AssertEqInstruction, Instruction, InstructionBody};
use casm::operand::{BinOpOperand, CellRef, DerefOrImmediate, Operation, Register, ResOperand};
use itertools::{chain, zip_eq};
use sierra::extensions::array::ArrayConcreteLibFunc;
use sierra::extensions::boxing::BoxConcreteLibFunc;
use sierra::extensions::core::CoreConcreteLibFunc;
use sierra::extensions::enm::{EnumConcreteLibFunc, EnumInitConcreteLibFunc};
use sierra::extensions::felt::{
    FeltBinaryOperationConcreteLibFunc, FeltConcrete, FeltOperationConcreteLibFunc,
    FeltOperationWithConstConcreteLibFunc, FeltOperator,
};
use sierra::extensions::function_call::FunctionCallConcreteLibFunc;
use sierra::extensions::gas::GasConcreteLibFunc;
use sierra::extensions::mem::{
    AllocLocalConcreteLibFunc, MemConcreteLibFunc, StoreLocalConcreteLibFunc,
    StoreTempConcreteLibFunc,
};
use sierra::extensions::ConcreteLibFunc;
use sierra::ids::ConcreteTypeId;
use sierra::program::{BranchInfo, BranchTarget, Invocation, StatementIdx};
use thiserror::Error;
use utils::{extract_matches, try_extract_matches};

use crate::environment::frame_state::FrameStateError;
use crate::environment::{frame_state, Environment};
use crate::metadata::Metadata;
use crate::references::{
    BinOpExpression, CellExpression, ReferenceExpression, ReferenceValue, ReferencesError,
};
use crate::relocations::{Relocation, RelocationEntry};
use crate::type_sizes::TypeSizeMap;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum InvocationError {
    #[error("One of the arguments does not satisfy the requirements of the libfunc.")]
    InvalidReferenceExpressionForArgument,
    #[error("Unexpected error - an unregistered type id used.")]
    UnknownTypeId(ConcreteTypeId),
    #[error("Expected a different number of arguments.")]
    WrongNumberOfArguments { expected: usize, actual: usize },
    #[error("The requested functionality is not implemented yet.")]
    NotImplemented(Invocation),
    #[error("The functionality is supported only for sized types.")]
    NotSized(Invocation),
    #[error("Expected variable data for statement not found.")]
    UnknownVariableData,
    #[error("An integer overflow occurred.")]
    IntegerOverflow,
    #[error(transparent)]
    FrameStateError(#[from] FrameStateError),
}

/// Describes the changes to the set of references at a single branch target, as well as changes to
/// the environment.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BranchChanges {
    /// New references defined at a given branch.
    /// should correspond to BranchInfo.results.
    pub refs: Vec<ReferenceValue>,
    /// The change to AP caused by the libfunc in the branch.
    pub ap_change: ApChange,
    /// The change to the remaing gas value in the wallet.
    pub gas_change: i64,
}
impl BranchChanges {
    fn new(
        ap_change: ApChange,
        gas_change: i64,
        expressions: impl Iterator<Item = ReferenceExpression>,
        types: impl Iterator<Item = ConcreteTypeId>,
    ) -> Self {
        Self {
            refs: zip_eq(expressions, types)
                .map(|(expression, ty)| ReferenceValue { expression, ty })
                .collect(),
            ap_change,
            gas_change,
        }
    }
}

/// The result from a compilation of a single invocation statement.
#[derive(Debug, Eq, PartialEq)]
pub struct CompiledInvocation {
    /// A vector of instructions that implement the invocation.
    pub instructions: Vec<Instruction>,
    /// A vector of static relocations.
    pub relocations: Vec<RelocationEntry>,
    /// A vector of BranchRefChanges, should correspond to the branches of the invocation
    /// statement.
    pub results: Vec<BranchChanges>,
    /// The environment after the invocation statement.
    pub environment: Environment,
}

/// Checks that the list of reference is contiguous on the stack and ends at ap - 1.
/// This is the requirement for function call and return statements.
pub fn check_references_on_stack(refs: &[ReferenceValue]) -> Result<(), InvocationError> {
    let mut expected_offset: i16 = -1;
    for return_ref in refs.iter().rev() {
        for cell_expr in return_ref.expression.cells.iter().rev() {
            match cell_expr {
                CellExpression::Deref(CellRef { register: Register::AP, offset })
                    if *offset == expected_offset =>
                {
                    expected_offset -= 1;
                }
                _ => return Err(InvocationError::InvalidReferenceExpressionForArgument),
            }
        }
    }
    Ok(())
}

/// Helper for building compiled invocations.
struct CompiledInvocationBuilder<'a> {
    pub program_info: ProgramInfo<'a>,
    pub invocation: &'a Invocation,
    pub libfunc: &'a CoreConcreteLibFunc,
    pub idx: StatementIdx,
    pub refs: &'a [ReferenceValue],
    pub environment: Environment,
}
impl CompiledInvocationBuilder<'_> {
    /// Creates a new invocation.
    fn build(
        self,
        instructions: Vec<Instruction>,
        relocations: Vec<RelocationEntry>,
        ap_changes: impl Iterator<Item = ApChange>,
        output_expressions: impl Iterator<Item = impl Iterator<Item = ReferenceExpression>>,
    ) -> CompiledInvocation {
        let gas_changes = sierra_gas::core_libfunc_cost::core_libfunc_cost(
            &self.program_info.metadata.gas_info,
            &self.idx,
            self.libfunc,
        );
        CompiledInvocation {
            instructions,
            relocations,
            results: zip_eq(
                zip_eq(ap_changes, gas_changes),
                zip_eq(output_expressions, self.libfunc.output_types()),
            )
            .map(|((ap_change, gas_change), (expressions, types))| {
                BranchChanges::new(
                    ap_change,
                    -gas_change.unwrap_or(0),
                    expressions,
                    types.iter().cloned(),
                )
            })
            .collect(),
            environment: self.environment,
        }
    }

    /// Creates a new invocation with only reference changes.
    fn build_only_reference_changes(
        self,
        output_expressions: impl Iterator<Item = ReferenceExpression>,
    ) -> CompiledInvocation {
        self.build(
            vec![],
            vec![],
            [ApChange::Known(0)].into_iter(),
            [output_expressions].into_iter(),
        )
    }

    /// Handles a felt operation with the given op.
    fn build_felt_op(self, op: FeltOperator) -> Result<CompiledInvocation, InvocationError> {
        let (expr_a, expr_b) = match self.refs {
            [
                ReferenceValue { expression: expr_a, .. },
                ReferenceValue { expression: expr_b, .. },
            ] => (expr_a, expr_b),
            refs => {
                return Err(InvocationError::WrongNumberOfArguments {
                    expected: 2,
                    actual: refs.len(),
                });
            }
        };
        let cell_a = expr_a
            .try_unpack_single()
            .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?;
        let cell_b = expr_b
            .try_unpack_single()
            .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?;
        let bin_expression = match (cell_a, cell_b) {
            (CellExpression::Deref(a), CellExpression::Deref(b)) => {
                BinOpExpression { op, a, b: DerefOrImmediate::Deref(b) }
            }
            (CellExpression::Deref(a), CellExpression::Immediate(b)) => {
                BinOpExpression { op, a, b: DerefOrImmediate::Immediate(b) }
            }
            _ => return Err(InvocationError::InvalidReferenceExpressionForArgument),
        };
        Ok(self.build_only_reference_changes(
            [ReferenceExpression::from_cell(CellExpression::BinOp(bin_expression))].into_iter(),
        ))
    }

    /// Handles a felt operation with a const.
    fn build_felt_op_with_const(
        self,
        op: FeltOperator,
        c: i128,
    ) -> Result<CompiledInvocation, InvocationError> {
        let expr = match self.refs {
            [ReferenceValue { expression, .. }] => expression,
            refs => {
                return Err(InvocationError::WrongNumberOfArguments {
                    expected: 1,
                    actual: refs.len(),
                });
            }
        };
        let cell_expr = expr
            .try_unpack_single()
            .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?;
        let ref_expression = if let CellExpression::Deref(a) = cell_expr {
            BinOpExpression { op, a, b: DerefOrImmediate::Immediate(c as i128) }
        } else {
            return Err(InvocationError::InvalidReferenceExpressionForArgument);
        };
        Ok(self.build_only_reference_changes(
            [ReferenceExpression::from_cell(CellExpression::BinOp(ref_expression))].into_iter(),
        ))
    }

    /// Handles a dup instruction.
    fn build_dup(self) -> Result<CompiledInvocation, InvocationError> {
        let expression = match self.refs {
            [ReferenceValue { expression, .. }] => expression,
            refs => {
                return Err(InvocationError::WrongNumberOfArguments {
                    expected: 1,
                    actual: refs.len(),
                });
            }
        };
        Ok(self.build_only_reference_changes([expression.clone(), expression.clone()].into_iter()))
    }

    /// Handles a function call.
    fn build_function_call(
        self,
        func_call: &FunctionCallConcreteLibFunc,
    ) -> Result<CompiledInvocation, InvocationError> {
        check_references_on_stack(self.refs)?;

        let output_types = func_call.output_types();
        let fallthrough_outputs = &output_types[0];

        let mut refs = VecDeque::with_capacity(fallthrough_outputs.len());

        let mut offset = -1;
        for output_type in fallthrough_outputs.iter().rev() {
            let size = self
                .program_info
                .type_sizes
                .get(output_type)
                .ok_or(InvocationError::UnknownVariableData)?;
            refs.push_front(ReferenceExpression {
                cells: ((offset - size + 1)..(offset + 1))
                    .map(|i| CellExpression::Deref(CellRef { register: Register::AP, offset: i }))
                    .collect(),
            });
            offset -= size;
        }

        let ap_change =
            match self.program_info.metadata.function_ap_change.get(&func_call.function.id) {
                // The call uses two stack slots.
                Some(ApChange::Known(change)) => ApChange::Known(change + 2),
                _ => ApChange::Unknown,
            };

        Ok(self.build(
            casm! { call rel 0; }.instructions,
            vec![RelocationEntry {
                instruction_idx: 0,
                relocation: Relocation::RelativeStatementId(func_call.function.entry_point),
            }],
            [ap_change].into_iter(),
            [refs.into_iter()].into_iter(),
        ))
    }

    /// Returns a store instruction. Helper function for store_temp and store_local.
    fn get_store_instructions(
        &self,
        src_type: &ConcreteTypeId,
        mut dst: CellRef,
        src_expr: &ReferenceExpression,
        inc_ap: bool,
    ) -> Result<Vec<Instruction>, InvocationError> {
        match self.program_info.type_sizes.get(src_type) {
            Some(0) => return Err(InvocationError::NotSized(self.invocation.clone())),
            None => return Err(InvocationError::NotImplemented(self.invocation.clone())),
            Some(_) => {}
        };

        let mut instructions = vec![];
        let mut ap_change = 0;
        // TODO(Gil): Consider using the casm! macros and add an if inc_ap.
        for cell_expr_orig in src_expr.cells.iter() {
            let cell_expr =
                cell_expr_orig.clone().apply_ap_change(ApChange::Known(ap_change)).unwrap();
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
                        b: ResOperand::DoubleDeref(operand),
                    }),
                    inc_ap,
                    hints: vec![],
                },
                CellExpression::IntoSingleCellRef(operand) => Instruction {
                    body: InstructionBody::AssertEq(AssertEqInstruction {
                        a: operand,
                        b: ResOperand::DoubleDeref(dst),
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
                        operand: ResOperand::Immediate(if inc_ap { 1 } else { 0 }),
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
    fn build_store_temp(self, ty: &ConcreteTypeId) -> Result<CompiledInvocation, InvocationError> {
        let expression = match self.refs {
            [ReferenceValue { expression, .. }] => expression,
            refs => {
                return Err(InvocationError::WrongNumberOfArguments {
                    expected: 1,
                    actual: refs.len(),
                });
            }
        };

        let dst = CellRef { register: Register::AP, offset: 0 };
        let instructions = self.get_store_instructions(ty, dst, expression, true)?;
        let type_size = self.program_info.type_sizes[ty];
        Ok(self.build(
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
    fn build_store_local(self, ty: &ConcreteTypeId) -> Result<CompiledInvocation, InvocationError> {
        let (dst_expr, src_expr) = match self.refs {
            [
                ReferenceValue { expression: dst_expr, .. },
                ReferenceValue { expression: src_expr, .. },
            ] => Ok((dst_expr, src_expr)),
            refs => {
                Err(InvocationError::WrongNumberOfArguments { expected: 2, actual: refs.len() })
            }
        }?;
        let dst = try_extract_matches!(
            dst_expr
                .try_unpack_single()
                .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?,
            CellExpression::Deref
        )
        .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;
        let instructions = self.get_store_instructions(ty, dst, src_expr, false)?;
        let type_size = self.program_info.type_sizes[ty];
        Ok(self.build(
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

    /// Handles a jump non zero statement.
    /// For example, this "Sierra statement"
    /// ```ignore
    /// felt_jump_nz(var=[ap-10]) { fallthrough() 1000(var) };
    /// ```
    /// translates to these casm instructions:
    /// ```ignore
    /// jmp rel <jump_offset_1000> if [ap-10] != 0
    /// ```
    fn build_jump_nz(self) -> Result<CompiledInvocation, InvocationError> {
        let dst_expr = match self.refs {
            [ReferenceValue { expression, .. }] => expression,
            refs => {
                return Err(InvocationError::WrongNumberOfArguments {
                    expected: 1,
                    actual: refs.len(),
                });
            }
        };
        let value = try_extract_matches!(
            dst_expr
                .try_unpack_single()
                .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?,
            CellExpression::Deref
        )
        .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;

        let target_statement_id = match self.invocation.branches.as_slice() {
            [
                BranchInfo { target: BranchTarget::Fallthrough, .. },
                BranchInfo { target: BranchTarget::Statement(statement_id), .. },
            ] => statement_id,
            _ => panic!("malformed invocation"),
        };

        Ok(self.build(
            casm! { jmp rel 0 if value != 0; }.instructions,
            vec![RelocationEntry {
                instruction_idx: 0,
                relocation: Relocation::RelativeStatementId(*target_statement_id),
            }],
            itertools::repeat_n(ApChange::Known(0), 2).into_iter(),
            [
                vec![].into_iter(),
                vec![ReferenceExpression::from_cell(CellExpression::Deref(value))].into_iter(),
            ]
            .into_iter(),
        ))
    }

    /// Handles a jump instruction.
    fn build_jump(self) -> Result<CompiledInvocation, InvocationError> {
        let target_statement_id = match self.invocation.branches.as_slice() {
            [BranchInfo { target: BranchTarget::Statement(statement_id), .. }] => statement_id,
            _ => panic!("malformed invocation"),
        };

        Ok(self.build(
            casm! { jmp rel 0; }.instructions,
            vec![RelocationEntry {
                instruction_idx: 0,
                relocation: Relocation::RelativeStatementId(*target_statement_id),
            }],
            [ApChange::Known(0)].into_iter(),
            [vec![].into_iter()].into_iter(),
        ))
    }

    /// Handles a locals alloction finalization instruction.
    fn build_finalize_locals(mut self) -> Result<CompiledInvocation, InvocationError> {
        let (n_slots, frame_state) = frame_state::handle_finalize_locals(
            self.environment.frame_state,
            self.environment.ap_tracking,
        )?;
        self.environment.frame_state = frame_state;
        Ok(self.build(
            casm! { ap += (n_slots as i128); }.instructions,
            vec![],
            [ApChange::Known(n_slots)].into_iter(),
            [[].into_iter()].into_iter(),
        ))
    }

    /// Handles instruction for creating a box.
    fn build_into_box(self) -> Result<CompiledInvocation, InvocationError> {
        if self.program_info.type_sizes.get(&self.libfunc.output_types()[0][0]) != Some(&1) {
            todo!("Add support for taking non-single cell references.");
        }
        let expression = match self.refs {
            [ReferenceValue { expression, .. }] => expression,
            refs => {
                return Err(InvocationError::WrongNumberOfArguments {
                    expected: 1,
                    actual: refs.len(),
                });
            }
        };
        if let CellExpression::Deref(operand) = expression
            .try_unpack_single()
            .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?
        {
            Ok(self.build_only_reference_changes(
                [ReferenceExpression::from_cell(CellExpression::IntoSingleCellRef(operand))]
                    .into_iter(),
            ))
        } else {
            Err(InvocationError::InvalidReferenceExpressionForArgument)
        }
    }

    /// Handles instruction for unboxing a box.
    fn build_unbox(self) -> Result<CompiledInvocation, InvocationError> {
        let expression = match self.refs {
            [ReferenceValue { expression, .. }] => expression,
            refs => {
                return Err(InvocationError::WrongNumberOfArguments {
                    expected: 1,
                    actual: refs.len(),
                });
            }
        };
        if let CellExpression::Deref(operand) = expression
            .try_unpack_single()
            .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?
        {
            Ok(self.build_only_reference_changes(
                [ReferenceExpression::from_cell(CellExpression::DoubleDeref(operand))].into_iter(),
            ))
        } else {
            Err(InvocationError::InvalidReferenceExpressionForArgument)
        }
    }

    /// Handles the local variable allocation instruction.
    fn build_alloc_local(
        mut self,
        ty: &ConcreteTypeId,
    ) -> Result<CompiledInvocation, InvocationError> {
        let allocation_size = match self.program_info.type_sizes.get(ty) {
            Some(0) => Err(InvocationError::NotSized(self.invocation.clone())),
            Some(size) => Ok(*size),
            _ => Err(InvocationError::NotImplemented(self.invocation.clone())),
        }?;

        let (slot, frame_state) = frame_state::handle_alloc_local(
            self.environment.frame_state,
            self.environment.ap_tracking,
            allocation_size,
        )?;
        self.environment.frame_state = frame_state;

        Ok(self.build_only_reference_changes(
            [ReferenceExpression::from_cell(CellExpression::Deref(CellRef {
                register: Register::FP,
                offset: slot,
            }))]
            .into_iter(),
        ))
    }

    /// Handles instruction for creating a new array.
    fn build_array_new(self) -> Result<CompiledInvocation, InvocationError> {
        if !self.refs.is_empty() {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 0,
                actual: self.refs.len(),
            });
        }

        Ok(self.build(
            // TODO(Gil): change to casm! macro when hints are supported.
            vec![Instruction {
                body: InstructionBody::AddAp(AddApInstruction {
                    operand: ResOperand::Immediate(1),
                }),
                inc_ap: false,
                hints: vec![Hint::AllocSegment {
                    dst: CellRef { register: Register::AP, offset: 0 },
                }],
            }],
            vec![],
            [ApChange::Known(1)].into_iter(),
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

    /// Handles instruction for appending an element to an array.
    fn build_array_append(self) -> Result<CompiledInvocation, InvocationError> {
        let (mut array_view, element_to_append) = match self.refs {
            [
                ReferenceValue { expression: expr_arr, .. },
                ReferenceValue { expression: expr_elem, .. },
            ] => {
                let array_view = ArrayView::try_get_view(expr_arr)
                    .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?;
                let elem_val = match expr_elem
                    .try_unpack_single()
                    .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?
                {
                    CellExpression::Deref(op) => DerefOrImmediate::Deref(op),
                    CellExpression::Immediate(op) => DerefOrImmediate::Immediate(op),
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
            return Err(InvocationError::NotImplemented(self.invocation.clone()));
        }
        match element_to_append {
            DerefOrImmediate::Immediate(_) => {
                // TODO(Gil): handle when assertion of immediate to DoubleDeref (e.g. [[ap+0]] = 1)
                // will be supported.
                Err(InvocationError::NotImplemented(self.invocation.clone()))
            }
            DerefOrImmediate::Deref(op) => {
                let instructions = casm! { op = [[array_view.end]]; }.instructions;
                array_view.end_offset += 1;
                Ok(self.build(
                    instructions,
                    vec![],
                    [ApChange::Known(0)].into_iter(),
                    [vec![array_view.to_reference_expression()].into_iter()].into_iter(),
                ))
            }
        }
    }

    /// Handles the get gas invocation.
    fn build_get_gas(self) -> Result<CompiledInvocation, InvocationError> {
        // TODO(orizi): Add Range-Check usage.
        let requested_count = self
            .program_info
            .metadata
            .gas_info
            .variable_values
            .get(&self.idx)
            .ok_or(InvocationError::UnknownVariableData)?;
        let (range_check_expression, gas_counter_expression) = match self.refs {
            [
                ReferenceValue { expression: range_check_expression, .. },
                ReferenceValue { expression: gas_counter_expression, .. },
            ] => (range_check_expression, gas_counter_expression),
            refs => {
                return Err(InvocationError::WrongNumberOfArguments {
                    expected: 2,
                    actual: refs.len(),
                });
            }
        };
        let gas_counter_value = try_extract_matches!(
            gas_counter_expression
                .try_unpack_single()
                .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?,
            CellExpression::Deref
        )
        .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;
        let range_check = try_extract_matches!(
            range_check_expression
                .try_unpack_single()
                .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?,
            CellExpression::Deref
        )
        .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;

        let target_statement_id = match self.invocation.branches.as_slice() {
            [BranchInfo { target: BranchTarget::Statement(statement_id), .. }, _] => statement_id,
            _ => panic!("malformed invocation"),
        };

        let gas_counter_value_for_branches =
            gas_counter_value.apply_ap_change(ApChange::Known(1)).unwrap();
        // The code up to the failure branch.
        let mut before_failure_branch = casm! {
            %{ memory[ap + 0] = memory gas_counter_value < (*requested_count as i128) %}
            jmp rel 0 if [ap + 0] != 0, ap++;
            // gas_counter >= requested_count:
            [ap + 0] = gas_counter_value_for_branches + (-requested_count as i128), ap++;
            [ap - 1] = [[range_check.apply_ap_change(ApChange::Known(2)).unwrap()]];
            jmp rel 0; // Fixed in relocations.
        };
        let branch_offset = before_failure_branch.current_code_offset;
        *extract_matches!(
            &mut extract_matches!(
                &mut before_failure_branch.instructions[0].body,
                InstructionBody::Jnz
            )
            .jump_offset,
            DerefOrImmediate::Immediate
        ) = branch_offset as i128;
        let relocation_index = before_failure_branch.instructions.len() - 1;
        let failure_branch = casm! {
            // gas_counter < requested_count:
            // TODO(orizi): Make into one command when wider constants are supported.
            [ap + 0] = gas_counter_value_for_branches + (1 - *requested_count as i128), ap++;
            [ap + 0] = [ap - 1] * (-1), ap++;
            [ap - 1] = [[range_check.apply_ap_change(ApChange::Known(3)).unwrap()]];
        };

        Ok(self.build(
            chain!(before_failure_branch.instructions, failure_branch.instructions).collect(),
            vec![RelocationEntry {
                instruction_idx: relocation_index,
                relocation: Relocation::RelativeStatementId(*target_statement_id),
            }],
            [ApChange::Known(2), ApChange::Known(3)].into_iter(),
            [
                vec![
                    ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                        op: FeltOperator::Add,
                        a: range_check.apply_ap_change(ApChange::Known(2)).unwrap(),
                        b: DerefOrImmediate::Immediate(1),
                    })),
                    ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                        op: FeltOperator::Sub,
                        a: gas_counter_value.apply_ap_change(ApChange::Known(2)).unwrap(),
                        b: DerefOrImmediate::Immediate(*requested_count as i128),
                    })),
                ]
                .into_iter(),
                vec![
                    ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                        op: FeltOperator::Add,
                        a: range_check.apply_ap_change(ApChange::Known(3)).unwrap(),
                        b: DerefOrImmediate::Immediate(1),
                    })),
                    ReferenceExpression::from_cell(CellExpression::Deref(
                        gas_counter_value.apply_ap_change(ApChange::Known(3)).unwrap(),
                    )),
                ]
                .into_iter(),
            ]
            .into_iter(),
        ))
    }

    /// Handles the refund gas invocation.
    fn build_refund_gas(self) -> Result<CompiledInvocation, InvocationError> {
        let requested_count = self
            .program_info
            .metadata
            .gas_info
            .variable_values
            .get(&self.idx)
            .ok_or(InvocationError::UnknownVariableData)?;
        let expression = match self.refs {
            [ReferenceValue { expression, .. }] => expression,
            refs => {
                return Err(InvocationError::WrongNumberOfArguments {
                    expected: 1,
                    actual: refs.len(),
                });
            }
        };
        let gas_counter_value = try_extract_matches!(
            expression
                .try_unpack_single()
                .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?,
            CellExpression::Deref
        )
        .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;

        Ok(self.build_only_reference_changes(
            [if *requested_count == 0 {
                ReferenceExpression::from_cell(CellExpression::Deref(gas_counter_value))
            } else {
                ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
                    op: FeltOperator::Add,
                    a: gas_counter_value,
                    b: DerefOrImmediate::Immediate(*requested_count as i128),
                }))
            }]
            .into_iter(),
        ))
    }

    /// Handles statement for initializing an enum.
    /// For example, with this setup
    /// ```ignore
    /// type felt_ty = felt;
    /// type unit_ty = Tuple;
    /// type Option = Enum<felt_ty, unit_ty>;
    /// libfunc init_option_some = enum_init<Option, 0>;
    /// felt_const<8>() -> (felt8);
    /// ````
    /// this "Sierra statement"
    /// ```ignore
    /// init_option_some(felt8=[ap-5]) -> (some_id);
    /// ```
    /// translates to these casm instructions:
    /// ```ignore
    /// [ap] = 0; ap++
    /// [ap] = [ap-5]; ap++
    /// ```
    fn build_enum_init(self, index: usize) -> Result<CompiledInvocation, InvocationError> {
        let expression = match self.refs {
            [ReferenceValue { expression, .. }] => expression,
            refs => {
                return Err(InvocationError::WrongNumberOfArguments {
                    expected: 1,
                    actual: refs.len(),
                });
            }
        };
        let init_arg = try_extract_matches!(
            expression
                .try_unpack_single()
                .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?,
            CellExpression::Deref
        )
        .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;

        let variant_selector = if self.invocation.branches.len() <= 2 {
            // For num_branches <= 2, we use the index as the variant_selector as the `match`
            // implementation jumps to the index 0 statement on 0, and to the index 1 statement on
            // 1.
            index as i128
        } else {
            // For num_branches > 2, the `enum_match` libfunc is implemented using a jump table. In
            // order to optimize `enum_match`, we define the variant_selector as the relevant
            // relative jump in case we match the actual variant.
            //
            // - To jump to the first variant (`index` == 0) we add "jump rel 1", as the jump
            // instruction with a deref operand is of size 1.
            // - To jump to the variant in index i, we add "jump rel (2 * i + 1)" as the rest of the
            // jump instructions are with an immediate operand, which makes them of size
            // 2.
            if index.checked_mul(2).and_then(|x| x.checked_add(1)).is_none() {
                return Err(InvocationError::IntegerOverflow);
            }
            (2 * index + 1) as i128
        };

        let enum_val = EnumView {
            variant_selector: CellExpression::Immediate(variant_selector),
            inner_value: CellExpression::Deref(init_arg),
        };
        Ok(self.build_only_reference_changes([enum_val.to_reference_expression()].into_iter()))
    }

    /// Handles statement for matching an enum.
    fn build_enum_match(self) -> Result<CompiledInvocation, InvocationError> {
        let matched_var = match self.refs {
            [ReferenceValue { expression, .. }] => EnumView::try_get_view(expression)
                .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?,
            refs => {
                return Err(InvocationError::WrongNumberOfArguments {
                    expected: 1,
                    actual: refs.len(),
                });
            }
        };
        // Verify variant_selector is of type deref. This is the case with an enum_value
        // that was validly created and then stored.
        let variant_selector =
            try_extract_matches!(matched_var.variant_selector, CellExpression::Deref)
                .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;

        let target_statement_ids = self.invocation.branches.iter().map(|b| match b {
            BranchInfo { target: BranchTarget::Statement(stmnt_id), .. } => *stmnt_id,
            _ => panic!("malformed invocation"),
        });

        let num_branches = self.invocation.branches.len();
        if num_branches <= 2 {
            self.build_enum_match_short(
                num_branches,
                matched_var,
                variant_selector,
                target_statement_ids,
            )
        } else {
            self.build_enum_match_long(
                num_branches,
                matched_var,
                variant_selector,
                target_statement_ids,
            )
        }
    }

    /// Handles statement for matching an enum with 1 or 2 variants.
    /// For example, with this setup
    /// ```ignore
    /// type felt_ty = felt;
    /// type unit_ty = Tuple;
    /// type Option = Enum<felt_ty, unit_ty>;
    /// libfunc init_option_some = enum_init<Option, 0>;
    /// felt_const<8>() -> (felt8);
    /// init_option_some(felt8=[ap-5]) -> (enum_var);
    /// ````
    /// this "Sierra statement" (2-variants-enum)
    /// ```ignore
    /// match_option(enum_var=[ap-10]) {1000(some=[ap-9]), 2000(none=[ap-9])};
    /// ```
    /// translates to these casm instructions:
    /// ```ignore
    /// jmp rel <jump_offset_2000> if [ap-10] != 0
    /// jmp rel <jump_offset_1000>
    /// ```
    /// Or this "Sierra statement" (single-variant-enum)
    /// ```ignore
    /// match_option(enum_var=[ap-10]) {1000(var=[ap-9])};
    /// ```
    /// translates to these casm instructions:
    /// ```ignore
    /// jmp rel <jump_offset_1000>
    /// ```
    ///
    /// Assumes that num_branches == self.invocation.branches.len() ==
    /// target_statement_ids_iter.len() and that num_branches <= 2.
    fn build_enum_match_short(
        self,
        num_branches: usize,
        matched_var: EnumView,
        variant_selector: CellRef,
        mut target_statement_ids: impl Iterator<Item = StatementIdx>,
    ) -> Result<CompiledInvocation, InvocationError> {
        let mut instructions = Vec::new();
        let mut relocations = Vec::new();
        // Add the jump_nz instruction if we have 2 branches.
        if num_branches == 2 {
            instructions.extend(casm! { jmp rel 0 if variant_selector != 0; }.instructions);
            // Add the first instruction of jumping to branch 1 if jmp_table_idx != 0 (1).
            relocations.push(RelocationEntry {
                instruction_idx: 0,
                relocation: Relocation::RelativeStatementId(target_statement_ids.nth(1).unwrap()),
            });
        }

        // TODO(yuval): this can be avoided with fallthrough.
        // Add the jump instruction to branch 0, anyway.
        instructions.extend(casm! { jmp rel 0; }.instructions);
        relocations.push(RelocationEntry {
            instruction_idx: instructions.len() - 1,
            relocation: Relocation::RelativeStatementId(target_statement_ids.next().unwrap()),
        });

        Ok(self.build(
            instructions,
            relocations,
            itertools::repeat_n(ApChange::Known(0), num_branches).into_iter(),
            itertools::repeat_n(
                vec![ReferenceExpression::from_cell(matched_var.inner_value)].into_iter(),
                num_branches,
            )
            .into_iter(),
        ))
    }

    /// Handles statement for matching an enum with 3+ variants.
    /// For example, with this setup
    /// ```ignore
    /// type felt_ty = felt;
    /// type unit_ty = Tuple;
    /// type Option = Enum<felt_ty, unit_ty>;
    /// libfunc init_option_some = enum_init<Option, 0>;
    /// felt_const<8>() -> (felt8);
    /// init_option_some(felt8=[ap-5]) -> (enum_var);
    /// ````
    /// this "Sierra statement" (3-variants-enum)
    /// ```ignore
    /// match_option(enum_var=[ap-10]) {1000(pos=[ap-9]), 2000(neg=[ap-9]), 3000(zero=[ap-9])};
    /// ```
    /// translates to these casm instructions:
    /// ```ignore
    /// jmp rel [ap-10]
    /// jmp rel <jump_offset_1000>
    /// jmp rel <jump_offset_2000>
    /// jmp rel <jump_offset_3000>
    /// ```
    /// Where in the first location of the enum_var there will be the jmp_table_idx (1 for the first
    /// branch, 2 for the second and so on).
    ///
    /// Assumes that num_branches == self.invocation.branches.len() == target_statement_ids.len()
    /// and that num_branches > 2.
    fn build_enum_match_long(
        self,
        num_branches: usize,
        matched_var: EnumView,
        variant_selector: CellRef,
        target_statement_ids: impl Iterator<Item = StatementIdx>,
    ) -> Result<CompiledInvocation, InvocationError> {
        // The first instruction is the jmp to the relevant index in the jmp table.
        let mut instructions = casm! { jmp rel variant_selector; }.instructions;
        let mut relocations = Vec::new();

        for (i, stmnt_id) in target_statement_ids.enumerate() {
            // Add the jump instruction to the relevant target.
            instructions.extend(casm! { jmp rel 0; }.instructions);
            relocations.push(RelocationEntry {
                instruction_idx: i + 1,
                relocation: Relocation::RelativeStatementId(stmnt_id),
            });
        }

        Ok(self.build(
            instructions,
            relocations,
            itertools::repeat_n(ApChange::Known(0), num_branches).into_iter(),
            itertools::repeat_n(
                vec![ReferenceExpression::from_cell(matched_var.inner_value)].into_iter(),
                num_branches,
            )
            .into_iter(),
        ))
    }
}

/// Information in the program level required for compiling an invocation.
pub struct ProgramInfo<'a> {
    pub metadata: &'a Metadata,
    pub type_sizes: &'a TypeSizeMap,
}

/// Given a Sierra invocation statement and concrete libfunc, creates a compiled casm representation
/// of the Sierra statement.
pub fn compile_invocation(
    program_info: ProgramInfo<'_>,
    invocation: &Invocation,
    libfunc: &CoreConcreteLibFunc,
    idx: StatementIdx,
    refs: &[ReferenceValue],
    environment: Environment,
) -> Result<CompiledInvocation, InvocationError> {
    let builder =
        CompiledInvocationBuilder { program_info, invocation, libfunc, idx, refs, environment };
    match libfunc {
        // TODO(ilya, 10/10/2022): Handle type.
        CoreConcreteLibFunc::Felt(FeltConcrete::Operation(
            FeltOperationConcreteLibFunc::Binary(FeltBinaryOperationConcreteLibFunc {
                operator,
                ..
            }),
        )) => builder.build_felt_op(*operator),
        CoreConcreteLibFunc::Felt(FeltConcrete::Operation(
            FeltOperationConcreteLibFunc::Const(FeltOperationWithConstConcreteLibFunc {
                operator,
                c,
                ..
            }),
        )) => builder.build_felt_op_with_const(*operator, *c),
        CoreConcreteLibFunc::Felt(FeltConcrete::JumpNotZero(_)) => builder.build_jump_nz(),
        CoreConcreteLibFunc::Felt(FeltConcrete::Const(libfunc)) => Ok(builder
            .build_only_reference_changes(
                [ReferenceExpression::from_cell(CellExpression::Immediate(libfunc.c as i128))]
                    .into_iter(),
            )),
        CoreConcreteLibFunc::Gas(GasConcreteLibFunc::GetGas(_)) => builder.build_get_gas(),
        CoreConcreteLibFunc::Gas(GasConcreteLibFunc::RefundGas(_)) => builder.build_refund_gas(),
        CoreConcreteLibFunc::Gas(GasConcreteLibFunc::BurnGas(_)) => {
            Ok(builder.build_only_reference_changes([].into_iter()))
        }
        CoreConcreteLibFunc::Array(ArrayConcreteLibFunc::New(_)) => builder.build_array_new(),
        CoreConcreteLibFunc::Array(ArrayConcreteLibFunc::Append(_)) => builder.build_array_append(),
        CoreConcreteLibFunc::Drop(_) => Ok(builder.build_only_reference_changes([].into_iter())),
        CoreConcreteLibFunc::Dup(_) => builder.build_dup(),
        CoreConcreteLibFunc::Mem(MemConcreteLibFunc::StoreTemp(StoreTempConcreteLibFunc {
            ty,
            ..
        })) => builder.build_store_temp(ty),
        CoreConcreteLibFunc::Mem(MemConcreteLibFunc::Rename(_))
        | CoreConcreteLibFunc::UnwrapNonZero(_) => {
            Ok(builder.build_only_reference_changes(refs.iter().map(|r| r.expression.clone())))
        }
        CoreConcreteLibFunc::FunctionCall(func_call) => builder.build_function_call(func_call),
        CoreConcreteLibFunc::UnconditionalJump(_) => builder.build_jump(),
        CoreConcreteLibFunc::ApTracking(_) => Ok(builder.build(
            vec![],
            vec![],
            [ApChange::Unknown].into_iter(),
            [[].into_iter()].into_iter(),
        )),
        CoreConcreteLibFunc::Mem(MemConcreteLibFunc::FinalizeLocals(_)) => {
            builder.build_finalize_locals()
        }
        CoreConcreteLibFunc::Box(BoxConcreteLibFunc::Into(_)) => builder.build_into_box(),
        CoreConcreteLibFunc::Box(BoxConcreteLibFunc::Unbox(_)) => builder.build_unbox(),
        CoreConcreteLibFunc::Mem(MemConcreteLibFunc::AllocLocal(AllocLocalConcreteLibFunc {
            ty,
            ..
        })) => builder.build_alloc_local(ty),
        CoreConcreteLibFunc::Mem(MemConcreteLibFunc::StoreLocal(StoreLocalConcreteLibFunc {
            ty,
            ..
        })) => builder.build_store_local(ty),
        CoreConcreteLibFunc::Enum(EnumConcreteLibFunc::Init(EnumInitConcreteLibFunc {
            index,
            ..
        })) => builder.build_enum_init(*index),
        CoreConcreteLibFunc::Enum(EnumConcreteLibFunc::Match(_)) => builder.build_enum_match(),
        _ => Err(InvocationError::NotImplemented(invocation.clone())),
    }
}

// ===================================== Complex structs =====================================

/// A struct representing an actual enum value in the Sierra program.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EnumView {
    /// This would be ReferenceExpression::Immediate after enum_init, and would be
    /// ReferenceExpression::Deref after store_*.
    pub variant_selector: CellExpression,
    /// The inner value of the enum - can be any ReferenceExpression.
    pub inner_value: CellExpression,
}

impl ReferenceExpressionView for EnumView {
    type Error = ReferencesError;

    fn try_get_view(expr: &ReferenceExpression) -> Result<Self, Self::Error> {
        if expr.cells.len() != 2 {
            return Err(ReferencesError::InvalidReferenceTypeForArgument);
        }
        Ok(EnumView { variant_selector: expr.cells[0].clone(), inner_value: expr.cells[1].clone() })
    }
    fn to_reference_expression(self) -> ReferenceExpression {
        ReferenceExpression { cells: vec![self.variant_selector, self.inner_value] }
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

    fn try_get_view(expr: &ReferenceExpression) -> Result<Self, Self::Error> {
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
                    try_extract_matches!(binop.b, DerefOrImmediate::Immediate)
                        .ok_or(ReferencesError::InvalidReferenceTypeForArgument)?
                        as u16,
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
                        b: DerefOrImmediate::Immediate(self.end_offset as i128),
                    }),
                ],
            }
        }
    }
}

/// A trait for views of the Complex ReferenceExpressions as specific data structures (e.g.
/// enum/array).
trait ReferenceExpressionView: Sized {
    type Error;

    /// Extracts the specific view from the reference expressions. Can include validations and thus
    /// returns a result.
    fn try_get_view(expr: &ReferenceExpression) -> Result<Self, Self::Error>;
    /// Converts the view into a ReferenceExpression.
    fn to_reference_expression(self) -> ReferenceExpression;
}
