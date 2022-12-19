use assert_matches::assert_matches;
use casm::ap_change::ApChange;
use casm::instructions::Instruction;
use casm::operand::{CellRef, Register};
use itertools::zip_eq;
use sierra::extensions::builtin_cost::CostTokenType;
use sierra::extensions::core::CoreConcreteLibFunc;
use sierra::extensions::lib_func::BranchSignature;
use sierra::extensions::{ConcreteLibFunc, OutputVarReferenceInfo};
use sierra::ids::ConcreteTypeId;
use sierra::program::{BranchInfo, BranchTarget, Invocation, StatementIdx};
use sierra_ap_change::core_libfunc_ap_change::core_libfunc_ap_change;
use thiserror::Error;
use utils::ordered_hash_map::OrderedHashMap;
use {casm, sierra};

use crate::environment::frame_state::{FrameState, FrameStateError};
use crate::environment::Environment;
use crate::metadata::Metadata;
use crate::references::{CellExpression, ReferenceExpression, ReferenceValue};
use crate::relocations::RelocationEntry;
use crate::type_sizes::TypeSizeMap;

mod array;
mod boolean;
mod boxing;
mod builtin_cost;
mod dict_felt_to;
mod enm;
mod felt;
mod function_call;
mod gas;
mod mem;
mod misc;
mod pedersen;
mod starknet;

mod strct;
mod uint128;

#[cfg(test)]
mod test_utils;

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
    InvalidGenericArg,
    #[error("Invalid generic argument for libfunc.")]
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
    pub gas_change: OrderedHashMap<CostTokenType, i64>,
}
impl BranchChanges {
    fn new(
        ap_change: ApChange,
        gas_change: OrderedHashMap<CostTokenType, i64>,
        expressions: impl Iterator<Item = ReferenceExpression>,
        branch_signature: &BranchSignature,
    ) -> Self {
        Self {
            refs: zip_eq(expressions, &branch_signature.vars)
                .map(|(expression, var_info)| {
                    match var_info.ref_info {
                        OutputVarReferenceInfo::NewTempVar { .. } => {
                            expression.cells.iter().for_each(|cell| {
                                assert_matches!(
                                    cell,
                                    CellExpression::Deref(CellRef { register: Register::AP, .. })
                                )
                            });
                        }
                        OutputVarReferenceInfo::NewLocalVar { .. } => {
                            expression.cells.iter().for_each(|cell| {
                                assert_matches!(
                                    cell,
                                    CellExpression::Deref(CellRef { register: Register::FP, .. })
                                )
                            });
                        }
                        _ => (),
                    };
                    ReferenceValue { expression, ty: var_info.ty.clone() }
                })
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
                zip_eq(self.libfunc.branch_signatures(), gas_changes),
                zip_eq(output_expressions, core_libfunc_ap_change(self.libfunc)),
            )
            .map(|((branch_signature, gas_change), (expressions, ap_change))| {
                let ap_change = match ap_change {
                    sierra_ap_change::ApChange::Known(x) => ApChange::Known(x),
                    sierra_ap_change::ApChange::AtLocalsFinalizationByTypeSize(_) => {
                        ApChange::Known(0)
                    }
                    sierra_ap_change::ApChange::FinalizeLocals => {
                        match self.environment.frame_state {
                            FrameState::Finalized { allocated } => ApChange::Known(allocated),
                            _ => panic!("Unexpected frame state."),
                        }
                    }
                    sierra_ap_change::ApChange::KnownByTypeSize(ty) => {
                        ApChange::Known(self.program_info.type_sizes[&ty] as usize)
                    }
                    sierra_ap_change::ApChange::FunctionCall(id) => self
                        .program_info
                        .metadata
                        .ap_change_info
                        .function_ap_change
                        .get(&id)
                        .map_or(ApChange::Unknown, |x| ApChange::Known(x + 2)),
                    sierra_ap_change::ApChange::FromMetadata => ApChange::Known(
                        *self
                            .program_info
                            .metadata
                            .ap_change_info
                            .variable_values
                            .get(&self.idx)
                            .unwrap_or(&0),
                    ),
                    sierra_ap_change::ApChange::Unknown => ApChange::Unknown,
                };

                BranchChanges::new(
                    ap_change,
                    gas_change
                        .unwrap_or_default()
                        .iter()
                        .map(|(token_type, val)| (*token_type, -val))
                        .collect(),
                    expressions,
                    branch_signature,
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
        self.build(vec![], vec![], [output_expressions].into_iter())
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
        CoreConcreteLibFunc::Bitwise(_) => panic!("Not implemented."),
        CoreConcreteLibFunc::Bool(libfunc) => boolean::build(libfunc, builder),
        CoreConcreteLibFunc::Uint128(libfunc) => uint128::build(libfunc, builder),
        CoreConcreteLibFunc::Gas(libfunc) => gas::build(libfunc, builder),
        CoreConcreteLibFunc::BranchAlign(_) => misc::build_branch_align(builder),
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
        CoreConcreteLibFunc::Struct(libfunc) => strct::build(libfunc, builder),
        CoreConcreteLibFunc::DictFeltTo(libfunc) => dict_felt_to::build(libfunc, builder),
        CoreConcreteLibFunc::Pedersen(libfunc) => pedersen::build(libfunc, builder),
        CoreConcreteLibFunc::BuiltinCost(libfunc) => builtin_cost::build(libfunc, builder),
        CoreConcreteLibFunc::StarkNet(libfunc) => starknet::build(libfunc, builder),
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

/// Fetches the non-fallthrough jump target of the invocation, assuming this invocation is a
/// conditional jump.
pub fn get_non_fallthrough_statement_id(builder: &CompiledInvocationBuilder<'_>) -> StatementIdx {
    match builder.invocation.branches.as_slice() {
        [
            BranchInfo { target: BranchTarget::Fallthrough, .. },
            BranchInfo { target: BranchTarget::Statement(target_statement_id), .. },
        ] => *target_statement_id,
        _ => panic!("malformed invocation"),
    }
}
