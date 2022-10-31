use casm::ap_change::ApChange;
use casm::instructions::Instruction;
use casm::operand::{CellRef, Register};
use itertools::zip_eq;
use sierra::extensions::core::CoreConcreteLibFunc;
use sierra::extensions::ConcreteLibFunc;
use sierra::ids::ConcreteTypeId;
use sierra::program::{Invocation, StatementIdx};
use thiserror::Error;

use crate::environment::frame_state::FrameStateError;
use crate::environment::Environment;
use crate::metadata::Metadata;
use crate::references::{CellExpression, ReferenceExpression, ReferenceValue};
use crate::relocations::RelocationEntry;
use crate::type_sizes::TypeSizeMap;

mod array;
mod boxing;
mod enm;
mod felt;
mod function_call;
mod gas;
mod mem;
mod misc;
mod uint128;

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
    #[error("Expected type data not found.")]
    UnknownTypeData,
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
pub struct CompiledInvocationBuilder<'a> {
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
        CoreConcreteLibFunc::Felt(libfunc) => felt::build(libfunc, builder),
        CoreConcreteLibFunc::Uint128(libfunc) => uint128::build(libfunc, builder),
        CoreConcreteLibFunc::Gas(libfunc) => gas::build(libfunc, builder),
        CoreConcreteLibFunc::Array(libfunc) => array::build(libfunc, builder),
        CoreConcreteLibFunc::Drop(_) => misc::build_drop(builder),
        CoreConcreteLibFunc::Dup(_) => misc::build_dup(builder),
        CoreConcreteLibFunc::Mem(libfunc) => mem::build(libfunc, builder),
        CoreConcreteLibFunc::UnwrapNonZero(_) => misc::build_identity(builder),
        CoreConcreteLibFunc::FunctionCall(libfunc) => function_call::build(libfunc, builder),
        CoreConcreteLibFunc::UnconditionalJump(_) => misc::build_jump(builder),
        CoreConcreteLibFunc::ApTracking(_) => misc::build_revoke_ap_tracking(builder),
        CoreConcreteLibFunc::Box(libfunc) => boxing::build(libfunc, builder),
        CoreConcreteLibFunc::Enum(libfunc) => enm::build(libfunc, builder),
    }
}

/// A trait for views of the Complex ReferenceExpressions as specific data structures (e.g.
/// enum/array).
trait ReferenceExpressionView: Sized {
    type Error;
    /// Extracts the specific view from the reference expressions. Can include validations and thus
    /// returns a result.
    /// `concrete_type_id` - the concrete type this view should represent.
    fn try_get_view(
        expr: &ReferenceExpression,
        program_info: &ProgramInfo<'_>,
        concrete_type_id: &ConcreteTypeId,
    ) -> Result<Self, Self::Error>;
    /// Converts the view into a ReferenceExpression.
    fn to_reference_expression(self) -> ReferenceExpression;
}
