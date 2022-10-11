use std::collections::VecDeque;

use casm::ap_change::ApChange;
use casm::hints::Hint;
use casm::instructions::{
    AddApInstruction, AssertEqInstruction, CallInstruction, Instruction, InstructionBody,
    JnzInstruction, JumpInstruction,
};
use casm::operand::{
    BinOpOperand, DerefOperand, DerefOrImmediate, DoubleDerefOperand, ImmediateOperand, Operation,
    Register, ResOperand,
};
use itertools::zip_eq;
use sierra::extensions::arithmetic::{
    BinaryOperationConcreteLibFunc, OperationConcreteLibFunc, Operator,
};
use sierra::extensions::core::CoreConcreteLibFunc;
use sierra::extensions::felt::FeltConcrete;
use sierra::extensions::function_call::FunctionCallConcreteLibFunc;
use sierra::extensions::mem::{
    AllocLocalConcreteLibFunc, MemConcreteLibFunc, StoreLocalConcreteLibFunc,
    StoreTempConcreteLibFunc,
};
use sierra::extensions::reference::RefConcreteLibFunc;
use sierra::extensions::ConcreteLibFunc;
use sierra::ids::ConcreteTypeId;
use sierra::program::{BranchInfo, BranchTarget, Invocation};
use thiserror::Error;

use crate::environment::frame_state::FrameStateError;
use crate::environment::{frame_state, Environment};
use crate::references::{BinOpExpression, ReferenceExpression, ReferenceValue};
use crate::relocations::{Relocation, RelocationEntry};
use crate::type_sizes::TypeSizeMap;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum InvocationError {
    #[error("One of the arguments does not satisfy the requirements of the libfunc.")]
    InvalidReferenceExpressionForArgument,
    #[error("Unexpected error - an unregistered type id used.")]
    UnknownTypeId(ConcreteTypeId),
    #[error("Expected a different number of arguments.")]
    WrongNumberOfArguments,
    #[error("The requested functionality is not implemented yet.")]
    NotImplemented(Invocation),
    #[error("The functionality is supported only for sized types.")]
    NotSized(Invocation),
    #[error(transparent)]
    FrameStateError(#[from] FrameStateError),
}

/// Describes the changes to the set of references at the branch target.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BranchRefChanges {
    // New references defined at a given branch.
    // should correspond to BranchInfo.results.
    pub refs: Vec<ReferenceValue>,
    pub ap_change: ApChange,
}
impl BranchRefChanges {
    fn new(
        ap_change: ApChange,
        expressions: impl Iterator<Item = ReferenceExpression>,
        types: impl Iterator<Item = ConcreteTypeId>,
    ) -> Self {
        Self {
            refs: zip_eq(expressions, types)
                .map(|(expression, ty)| ReferenceValue { expression, ty })
                .collect(),
            ap_change,
        }
    }
}

/// The result from a compilation of a single invocation statement.
#[derive(Debug, Eq, PartialEq)]
pub struct CompiledInvocation {
    // A vector instructions that implement the Invocation.
    pub instructions: Vec<Instruction>,
    // A vector of static relocation.
    pub relocations: Vec<RelocationEntry>,
    // A vector of BranchRefChanges, should correspond to Invocation.branches.
    pub results: Vec<BranchRefChanges>,
    // The environment after the invocation.
    pub environment: Environment,
}

/// Checks that the list of reference is contiguous on the stack and ends at ap - 1.
/// This is the requirement for function call and return statements.
pub fn check_references_on_stack(
    type_sizes: &TypeSizeMap,
    refs: &[ReferenceValue],
) -> Result<(), InvocationError> {
    let mut expected_offset: i16 = -1;
    for return_ref in refs.iter().rev() {
        match return_ref.expression {
            ReferenceExpression::Deref(DerefOperand { register: Register::AP, offset })
                if offset == expected_offset =>
            {
                expected_offset -= type_sizes
                    .get(&return_ref.ty)
                    .ok_or_else(|| InvocationError::UnknownTypeId(return_ref.ty.clone()))?;
            }
            _ => return Err(InvocationError::InvalidReferenceExpressionForArgument),
        }
    }
    Ok(())
}

/// Helper for building compiled invocations.
struct CompiledInvocationBuilder<'a> {
    pub environment: Environment,
    pub libfunc: &'a CoreConcreteLibFunc,
    pub invocation: &'a Invocation,
    pub refs: &'a [ReferenceValue],
    pub type_sizes: &'a TypeSizeMap,
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
        CompiledInvocation {
            instructions,
            relocations,
            results: zip_eq(ap_changes, zip_eq(output_expressions, self.libfunc.output_types()))
                .map(|(ap_change, (expressions, types))| {
                    BranchRefChanges::new(ap_change, expressions, types.iter().cloned())
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
    fn build_felt_op(self, op: Operator) -> Result<CompiledInvocation, InvocationError> {
        let (expr_a, expr_b) = match self.refs {
            [
                ReferenceValue { expression: expr_a, .. },
                ReferenceValue { expression: expr_b, .. },
            ] => (expr_a, expr_b),
            _ => return Err(InvocationError::WrongNumberOfArguments),
        };

        let ref_expression = match (expr_a, expr_b) {
            (ReferenceExpression::Deref(a), ReferenceExpression::Deref(b)) => {
                BinOpExpression { op, a: *a, b: DerefOrImmediate::Deref(*b) }
            }
            (ReferenceExpression::Deref(a), ReferenceExpression::Immediate(b)) => {
                BinOpExpression { op, a: *a, b: DerefOrImmediate::Immediate(*b) }
            }
            _ => return Err(InvocationError::InvalidReferenceExpressionForArgument),
        };
        Ok(self
            .build_only_reference_changes([ReferenceExpression::BinOp(ref_expression)].into_iter()))
    }

    /// Handles a dup instruction.
    fn build_dup(self) -> Result<CompiledInvocation, InvocationError> {
        let expression = match self.refs {
            [ReferenceValue { expression, .. }] => expression,
            _ => return Err(InvocationError::WrongNumberOfArguments),
        };

        Ok(self.build_only_reference_changes([expression.clone(), expression.clone()].into_iter()))
    }

    /// Handles a function call.
    fn build_function_call(
        self,
        func_call: &FunctionCallConcreteLibFunc,
    ) -> Result<CompiledInvocation, InvocationError> {
        check_references_on_stack(self.type_sizes, self.refs)?;

        let output_types = func_call.output_types();
        let fallthrough_outputs = &output_types[0];

        let mut refs = VecDeque::with_capacity(fallthrough_outputs.len());

        let mut offset = -1;
        for output_type in fallthrough_outputs.iter().rev() {
            refs.push_front(ReferenceExpression::Deref(DerefOperand {
                register: Register::AP,
                offset,
            }));

            offset -= self
                .type_sizes
                .get(output_type)
                .ok_or_else(|| InvocationError::UnknownTypeId(output_type.clone()))?;
        }

        // TODO(ilya, 10/10/2022): Support functions with known ap change.
        Ok(self.build(
            vec![Instruction::new(
                InstructionBody::Call(CallInstruction {
                    target: DerefOrImmediate::Immediate(ImmediateOperand { value: 0 }),
                    relative: true,
                }),
                false,
            )],
            vec![RelocationEntry {
                instruction_idx: 0,
                relocation: Relocation::RelativeStatementId(func_call.function.entry_point),
            }],
            [ApChange::Unknown].into_iter(),
            [refs.into_iter()].into_iter(),
        ))
    }

    /// Returns a store instruction. Helper function for store_temp and store_local.
    fn get_store_instruction(
        &self,
        src_type: &ConcreteTypeId,
        dst: DerefOperand,
        src_expr: &ReferenceExpression,
        inc_ap: bool,
    ) -> Result<Instruction, InvocationError> {
        match self.type_sizes.get(src_type) {
            Some(1) => Ok(()),
            Some(0) => Err(InvocationError::NotSized(self.invocation.clone())),
            _ => Err(InvocationError::NotImplemented(self.invocation.clone())),
        }?;

        let mut hints = vec![];

        let (dst_operand, res_operand) = match src_expr {
            ReferenceExpression::Deref(operand) => (dst, ResOperand::Deref(*operand)),
            ReferenceExpression::DoubleDeref(operand) => {
                (dst, ResOperand::DoubleDeref(operand.clone()))
            }
            ReferenceExpression::IntoSingleCellRef(operand) => {
                hints.push(Hint::AllocSegment { dst });
                (*operand, ResOperand::DoubleDeref(DoubleDerefOperand { inner_deref: dst }))
            }
            ReferenceExpression::Immediate(operand) => (dst, ResOperand::Immediate(*operand)),
            ReferenceExpression::BinOp(BinOpExpression { op, a, b }) => match op {
                Operator::Add => (
                    dst,
                    ResOperand::BinOp(BinOpOperand { op: Operation::Add, a: *a, b: b.clone() }),
                ),
                Operator::Mul => (
                    dst,
                    ResOperand::BinOp(BinOpOperand { op: Operation::Mul, a: *a, b: b.clone() }),
                ),

                // dst = a - b => a = dst + b
                Operator::Sub => (
                    *a,
                    ResOperand::BinOp(BinOpOperand { op: Operation::Add, a: dst, b: b.clone() }),
                ),
                // dst = a / b => a = dst * b
                Operator::Div => (
                    *a,
                    ResOperand::BinOp(BinOpOperand { op: Operation::Mul, a: dst, b: b.clone() }),
                ),
                _ => return Err(InvocationError::NotImplemented(self.invocation.clone())),
            },
        };
        Ok(Instruction {
            body: InstructionBody::AssertEq(AssertEqInstruction { a: dst_operand, b: res_operand }),
            inc_ap,
            hints,
        })
    }

    /// Handles store_temp for the given type.
    fn build_store_temp(self, ty: &ConcreteTypeId) -> Result<CompiledInvocation, InvocationError> {
        let expression = match self.refs {
            [ReferenceValue { expression, .. }] => expression,
            _ => return Err(InvocationError::WrongNumberOfArguments),
        };

        let dst = DerefOperand { register: Register::AP, offset: 0 };
        let instruction = self.get_store_instruction(ty, dst, expression, true)?;
        Ok(self.build(
            vec![instruction],
            vec![],
            [ApChange::Known(1)].into_iter(),
            [[ReferenceExpression::Deref(DerefOperand { register: Register::AP, offset: -1 })]
                .into_iter()]
            .into_iter(),
        ))
    }

    /// Handles store_local for the given type.
    fn build_store_local(self, ty: &ConcreteTypeId) -> Result<CompiledInvocation, InvocationError> {
        let (dst, src_expr) = match self.refs {
            [
                ReferenceValue { expression: ReferenceExpression::Deref(dst), .. },
                ReferenceValue { expression: src_expr, .. },
            ] => Ok((dst, src_expr)),
            [_, _] => Err(InvocationError::InvalidReferenceExpressionForArgument),
            _ => Err(InvocationError::WrongNumberOfArguments),
        }?;

        let instruction = self.get_store_instruction(ty, *dst, src_expr, false)?;
        Ok(self.build(
            vec![instruction],
            vec![],
            [ApChange::Known(0)].into_iter(),
            [[ReferenceExpression::Deref(*dst)].into_iter()].into_iter(),
        ))
    }

    /// Handles a jump non zero instruction.
    fn build_jump_nz(self) -> Result<CompiledInvocation, InvocationError> {
        let condition = match self.refs {
            [ReferenceValue { expression: ReferenceExpression::Deref(deref_operand), .. }] => {
                deref_operand
            }
            [_] => return Err(InvocationError::InvalidReferenceExpressionForArgument),
            _ => return Err(InvocationError::WrongNumberOfArguments),
        };

        let target_statement_id = match self.invocation.branches.as_slice() {
            [BranchInfo { target: BranchTarget::Statement(statement_id), .. }, _] => statement_id,
            _ => panic!("malformed invocation"),
        };

        Ok(self.build(
            vec![Instruction::new(
                InstructionBody::Jnz(JnzInstruction {
                    jump_offset: DerefOrImmediate::Immediate(ImmediateOperand { value: 0 }),
                    condition: *condition,
                }),
                false,
            )],
            vec![RelocationEntry {
                instruction_idx: 0,
                relocation: Relocation::RelativeStatementId(*target_statement_id),
            }],
            [ApChange::Known(0), ApChange::Known(0)].into_iter(),
            [vec![ReferenceExpression::Deref(*condition)].into_iter(), vec![].into_iter()]
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
            vec![Instruction::new(
                InstructionBody::Jump(JumpInstruction {
                    target: DerefOrImmediate::Immediate(ImmediateOperand { value: 0 }),
                    relative: true,
                }),
                false,
            )],
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
            vec![Instruction::new(
                InstructionBody::AddAp(AddApInstruction {
                    operand: ResOperand::Immediate(ImmediateOperand { value: n_slots as i128 }),
                }),
                false,
            )],
            vec![],
            [ApChange::Known(n_slots)].into_iter(),
            [[].into_iter()].into_iter(),
        ))
    }

    /// Handles instruction for taking a reference.
    fn build_into_ref(self) -> Result<CompiledInvocation, InvocationError> {
        if self.type_sizes.get(&self.libfunc.output_types()[0][0]) != Some(&1) {
            todo!("Add support for taking non-single cell references.");
        }
        let expression = match self.refs {
            [ReferenceValue { expression, .. }] => expression,
            _ => return Err(InvocationError::WrongNumberOfArguments),
        };
        if let ReferenceExpression::Deref(operand) = expression {
            Ok(self.build_only_reference_changes(
                [ReferenceExpression::IntoSingleCellRef(*operand)].into_iter(),
            ))
        } else {
            Err(InvocationError::InvalidReferenceExpressionForArgument)
        }
    }

    /// Handles instruction for dereferencing a reference.
    fn build_deref(self) -> Result<CompiledInvocation, InvocationError> {
        let expression = match self.refs {
            [ReferenceValue { expression, .. }] => expression,
            _ => return Err(InvocationError::WrongNumberOfArguments),
        };
        if let ReferenceExpression::Deref(operand) = expression {
            Ok(self.build_only_reference_changes(
                [ReferenceExpression::DoubleDeref(DoubleDerefOperand { inner_deref: *operand })]
                    .into_iter(),
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
        let allocation_size = match self.type_sizes.get(ty) {
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
            [ReferenceExpression::Deref(DerefOperand { register: Register::FP, offset: slot })]
                .into_iter(),
        ))
    }
}

/// Given an invocation and concrete libfunc, creates a compiled representation of the Sierra
/// command.
pub fn compile_invocation(
    invocation: &Invocation,
    libfunc: &CoreConcreteLibFunc,
    refs: &[ReferenceValue],
    type_sizes: &TypeSizeMap,
    environment: Environment,
) -> Result<CompiledInvocation, InvocationError> {
    let builder = CompiledInvocationBuilder { environment, libfunc, refs, invocation, type_sizes };
    match libfunc {
        // TODO(ilya, 10/10/2022): Handle type.
        CoreConcreteLibFunc::Felt(FeltConcrete::Operation(OperationConcreteLibFunc::Binary(
            BinaryOperationConcreteLibFunc { operator, .. },
        ))) => builder.build_felt_op(*operator),
        CoreConcreteLibFunc::Felt(FeltConcrete::JumpNotZero(_)) => builder.build_jump_nz(),
        CoreConcreteLibFunc::Felt(FeltConcrete::Const(libfunc)) => Ok(builder
            .build_only_reference_changes(
                [ReferenceExpression::Immediate(ImmediateOperand { value: libfunc.c as i128 })]
                    .into_iter(),
            )),
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
        CoreConcreteLibFunc::Ref(RefConcreteLibFunc::Take(_)) => builder.build_into_ref(),
        CoreConcreteLibFunc::Ref(RefConcreteLibFunc::Deref(_)) => builder.build_deref(),
        CoreConcreteLibFunc::Mem(MemConcreteLibFunc::AllocLocal(AllocLocalConcreteLibFunc {
            ty,
            ..
        })) => builder.build_alloc_local(ty),
        CoreConcreteLibFunc::Mem(MemConcreteLibFunc::StoreLocal(StoreLocalConcreteLibFunc {
            ty,
            ..
        })) => builder.build_store_local(ty),
        _ => Err(InvocationError::NotImplemented(invocation.clone())),
    }
}
