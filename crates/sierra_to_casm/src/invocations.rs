use std::collections::VecDeque;

use casm::ap_change::ApChange;
use casm::instructions::{
    AddApInstruction, AssertEqInstruction, CallInstruction, Instruction, InstructionBody,
    JnzInstruction, JumpInstruction,
};
use casm::operand::{
    BinOpOperand, DerefOperand, DerefOrImmediate, ImmediateOperand, Operation, Register, ResOperand,
};
use itertools::zip_eq;
use sierra::extensions::arithmetic::{
    BinaryOperationConcreteLibFunc, OperationConcreteLibFunc, Operator,
};
use sierra::extensions::core::CoreConcreteLibFunc;
use sierra::extensions::felt::FeltConcrete;
use sierra::extensions::function_call::FunctionCallConcreteLibFunc;
use sierra::extensions::lib_func::SignatureOnlyConcreteLibFunc;
use sierra::extensions::mem::{MemConcreteLibFunc, StoreTempConcreteLibFunc};
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
impl CompiledInvocation {
    fn new(
        instructions: Vec<Instruction>,
        relocations: Vec<RelocationEntry>,
        ap_changes: impl Iterator<Item = ApChange>,
        output_expressions: impl Iterator<Item = impl Iterator<Item = ReferenceExpression>>,
        output_types: &[Vec<ConcreteTypeId>],
        environment: Environment,
    ) -> Self {
        Self {
            instructions,
            relocations,
            results: zip_eq(ap_changes, zip_eq(output_expressions, output_types))
                .map(|(ap_change, (expressions, types))| {
                    BranchRefChanges::new(ap_change, expressions, types.iter().cloned())
                })
                .collect(),
            environment,
        }
    }

    // A constructor for the trivial case of fallthrough without any instructions.
    fn only_reference_changes(
        output_expressions: impl Iterator<Item = ReferenceExpression>,
        output_types: &[Vec<ConcreteTypeId>],
        environment: Environment,
    ) -> Self {
        Self::new(
            vec![],
            vec![],
            [ApChange::Known(0)].into_iter(),
            [output_expressions].into_iter(),
            output_types,
            environment,
        )
    }
}

fn handle_felt_op(
    _invocation: &Invocation,
    felt_op: &BinaryOperationConcreteLibFunc,
    refs: &[ReferenceValue],
    environment: Environment,
) -> Result<CompiledInvocation, InvocationError> {
    let (expr_a, expr_b) = match refs {
        [ReferenceValue { expression: expr_a, .. }, ReferenceValue { expression: expr_b, .. }] => {
            (expr_a, expr_b)
        }
        _ => return Err(InvocationError::WrongNumberOfArguments),
    };

    let ref_expression = match (expr_a, expr_b) {
        (ReferenceExpression::Deref(a), ReferenceExpression::Deref(b)) => {
            BinOpExpression { op: felt_op.operator, a: *a, b: DerefOrImmediate::Deref(*b) }
        }
        (ReferenceExpression::Deref(a), ReferenceExpression::Immediate(b)) => {
            BinOpExpression { op: felt_op.operator, a: *a, b: DerefOrImmediate::Immediate(*b) }
        }
        _ => return Err(InvocationError::InvalidReferenceExpressionForArgument),
    };
    Ok(CompiledInvocation::only_reference_changes(
        [ReferenceExpression::BinOp(ref_expression)].into_iter(),
        felt_op.output_types(),
        environment,
    ))
}

// Checks that the list of reference is contiguous on the stack and ends at ap - 1.
// This is the requirement for function call and return statements.
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

fn handle_felt_dup(
    felt_dup: &SignatureOnlyConcreteLibFunc,
    refs: &[ReferenceValue],
    environment: Environment,
) -> Result<CompiledInvocation, InvocationError> {
    let expression = match refs {
        [ReferenceValue { expression, .. }] => expression,
        _ => return Err(InvocationError::WrongNumberOfArguments),
    };

    Ok(CompiledInvocation::only_reference_changes(
        [expression.clone(), expression.clone()].into_iter(),
        felt_dup.output_types(),
        environment,
    ))
}

fn handle_function_call(
    type_sizes: &TypeSizeMap,
    func_call: &FunctionCallConcreteLibFunc,
    refs: &[ReferenceValue],
    environment: Environment,
) -> Result<CompiledInvocation, InvocationError> {
    check_references_on_stack(type_sizes, refs)?;

    let output_types = func_call.output_types();
    let fallthrough_outputs = &output_types[0];

    let mut refs = VecDeque::with_capacity(fallthrough_outputs.len());

    let mut offset = -1;
    for output_type in fallthrough_outputs.iter().rev() {
        refs.push_front(ReferenceExpression::Deref(DerefOperand {
            register: Register::AP,
            offset,
        }));

        offset -= type_sizes
            .get(output_type)
            .ok_or_else(|| InvocationError::UnknownTypeId(output_type.clone()))?;
    }

    // TODO(ilya, 10/10/2022): Support functions with known ap change.
    Ok(CompiledInvocation::new(
        vec![Instruction {
            body: InstructionBody::Call(CallInstruction {
                target: DerefOrImmediate::Immediate(ImmediateOperand { value: 0 }),
                relative: true,
            }),
            inc_ap: false,
        }],
        vec![RelocationEntry {
            instruction_idx: 0,
            relocation: Relocation::RelativeStatementId(func_call.function.entry),
        }],
        [ApChange::Unknown].into_iter(),
        [refs.into_iter()].into_iter(),
        func_call.output_types(),
        environment,
    ))
}

fn handle_store_temp(
    invocation: &Invocation,
    store_temp: &StoreTempConcreteLibFunc,
    refs: &[ReferenceValue],
    environment: Environment,
) -> Result<CompiledInvocation, InvocationError> {
    let expression = match refs {
        [ReferenceValue { expression, .. }] => expression,
        _ => return Err(InvocationError::WrongNumberOfArguments),
    };

    let dst = DerefOperand { register: Register::AP, offset: 0 };

    let (dst_operand, res_operand) = match expression {
        ReferenceExpression::Deref(operand) => (dst, ResOperand::Deref(*operand)),
        ReferenceExpression::DoubleDeref(operand) => {
            (dst, ResOperand::DoubleDeref(operand.clone()))
        }
        ReferenceExpression::Immediate(operand) => (dst, ResOperand::Immediate(*operand)),
        ReferenceExpression::BinOp(BinOpExpression { op, a, b }) => match op {
            Operator::Add => {
                (dst, ResOperand::BinOp(BinOpOperand { op: Operation::Add, a: *a, b: b.clone() }))
            }
            Operator::Mul => {
                (dst, ResOperand::BinOp(BinOpOperand { op: Operation::Mul, a: *a, b: b.clone() }))
            }

            // dst = a - b => a = dst + b
            Operator::Sub => {
                (*a, ResOperand::BinOp(BinOpOperand { op: Operation::Add, a: dst, b: b.clone() }))
            }
            // dst = a / b => a = dst * b
            Operator::Div => {
                (*a, ResOperand::BinOp(BinOpOperand { op: Operation::Mul, a: dst, b: b.clone() }))
            }
            _ => return Err(InvocationError::NotImplemented(invocation.clone())),
        },
    };

    Ok(CompiledInvocation::new(
        vec![Instruction {
            body: InstructionBody::AssertEq(AssertEqInstruction { a: dst_operand, b: res_operand }),
            inc_ap: true,
        }],
        vec![],
        [ApChange::Known(1)].into_iter(),
        [[ReferenceExpression::Deref(DerefOperand { register: Register::AP, offset: -1 })]
            .into_iter()]
        .into_iter(),
        store_temp.output_types(),
        environment,
    ))
}

fn handle_jump_nz(
    invocation: &Invocation,
    jnz: &SignatureOnlyConcreteLibFunc,
    refs: &[ReferenceValue],
    environment: Environment,
) -> Result<CompiledInvocation, InvocationError> {
    let condition = match refs {
        [ReferenceValue { expression: ReferenceExpression::Deref(deref_operand), .. }] => {
            deref_operand
        }
        [_] => return Err(InvocationError::InvalidReferenceExpressionForArgument),
        _ => return Err(InvocationError::WrongNumberOfArguments),
    };

    let target_statement_id = match invocation.branches.as_slice() {
        [BranchInfo { target: BranchTarget::Statement(statement_id), .. }, _] => statement_id,
        _ => panic!("malformed invocation"),
    };

    Ok(CompiledInvocation::new(
        vec![Instruction {
            body: InstructionBody::Jnz(JnzInstruction {
                jump_offset: DerefOrImmediate::Immediate(ImmediateOperand { value: 0 }),
                condition: *condition,
            }),
            inc_ap: false,
        }],
        vec![RelocationEntry {
            instruction_idx: 0,
            relocation: Relocation::RelativeStatementId(*target_statement_id),
        }],
        [ApChange::Known(0), ApChange::Known(0)].into_iter(),
        [vec![ReferenceExpression::Deref(*condition)].into_iter(), vec![].into_iter()].into_iter(),
        jnz.output_types(),
        environment,
    ))
}

fn handle_jump(
    invocation: &Invocation,
    libfunc: &SignatureOnlyConcreteLibFunc,
    environment: Environment,
) -> Result<CompiledInvocation, InvocationError> {
    let target_statement_id = match invocation.branches.as_slice() {
        [BranchInfo { target: BranchTarget::Statement(statement_id), .. }] => statement_id,
        _ => panic!("malformed invocation"),
    };

    Ok(CompiledInvocation::new(
        vec![Instruction {
            body: InstructionBody::Jump(JumpInstruction {
                target: DerefOrImmediate::Immediate(ImmediateOperand { value: 0 }),
                relative: true,
            }),
            inc_ap: false,
        }],
        vec![RelocationEntry {
            instruction_idx: 0,
            relocation: Relocation::RelativeStatementId(*target_statement_id),
        }],
        [ApChange::Known(0)].into_iter(),
        [vec![].into_iter()].into_iter(),
        libfunc.output_types(),
        environment,
    ))
}

fn handle_alloc_locals(
    libfunc: &SignatureOnlyConcreteLibFunc,
    environment: Environment,
) -> Result<CompiledInvocation, InvocationError> {
    let (n_slots, frame_state) =
        frame_state::handle_alloc_locals(environment.frame_state, environment.ap_tracking)?;
    Ok(CompiledInvocation::new(
        vec![Instruction {
            body: InstructionBody::AddAp(AddApInstruction {
                operand: ResOperand::Immediate(ImmediateOperand { value: n_slots as i128 }),
            }),
            inc_ap: false,
        }],
        vec![],
        [ApChange::Known(n_slots)].into_iter(),
        [[].into_iter()].into_iter(),
        libfunc.output_types(),
        Environment { frame_state, ..environment },
    ))
}

pub fn compile_invocation(
    invocation: &Invocation,
    libfunc: &CoreConcreteLibFunc,
    refs: &[ReferenceValue],
    type_sizes: &TypeSizeMap,
    environment: Environment,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        // TODO(ilya, 10/10/2022): Handle type.
        CoreConcreteLibFunc::Felt(FeltConcrete::Operation(OperationConcreteLibFunc::Binary(
            felt_op,
        ))) => handle_felt_op(invocation, felt_op, refs, environment),
        CoreConcreteLibFunc::Felt(FeltConcrete::Duplicate(felt_dup)) => {
            handle_felt_dup(felt_dup, refs, environment)
        }
        CoreConcreteLibFunc::Felt(FeltConcrete::JumpNotZero(jnz)) => {
            handle_jump_nz(invocation, jnz, refs, environment)
        }
        CoreConcreteLibFunc::Felt(FeltConcrete::Drop(libfunc)) => {
            Ok(CompiledInvocation::only_reference_changes(
                [].into_iter(),
                libfunc.output_types(),
                environment,
            ))
        }
        CoreConcreteLibFunc::Felt(FeltConcrete::Const(libfunc)) => {
            Ok(CompiledInvocation::only_reference_changes(
                [ReferenceExpression::Immediate(ImmediateOperand { value: libfunc.c as i128 })]
                    .into_iter(),
                libfunc.output_types(),
                environment,
            ))
        }
        CoreConcreteLibFunc::Mem(MemConcreteLibFunc::StoreTemp(store_temp)) => {
            handle_store_temp(invocation, store_temp, refs, environment)
        }
        CoreConcreteLibFunc::Mem(MemConcreteLibFunc::Rename(libfunc))
        | CoreConcreteLibFunc::UnwrapNonZero(libfunc) => {
            Ok(CompiledInvocation::only_reference_changes(
                refs.iter().map(|r| r.expression.clone()),
                libfunc.output_types(),
                environment,
            ))
        }
        CoreConcreteLibFunc::FunctionCall(func_call) => {
            handle_function_call(type_sizes, func_call, refs, environment)
        }
        CoreConcreteLibFunc::UnconditionalJump(libfunc) => {
            handle_jump(invocation, libfunc, environment)
        }
        CoreConcreteLibFunc::ApTracking(libfunc) => Ok(CompiledInvocation::new(
            vec![],
            vec![],
            [ApChange::Unknown].into_iter(),
            [[].into_iter()].into_iter(),
            libfunc.output_types(),
            environment,
        )),
        CoreConcreteLibFunc::Mem(MemConcreteLibFunc::AllocLocals(libfunc)) => {
            handle_alloc_locals(libfunc, environment)
        }
        _ => Err(InvocationError::NotImplemented(invocation.clone())),
    }
}
