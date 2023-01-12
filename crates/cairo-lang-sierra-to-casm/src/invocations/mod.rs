use assert_matches::assert_matches;
use cairo_lang_casm::ap_change::ApChange;
use cairo_lang_casm::builder::{CasmBuildResult, CasmBuilder, Var};
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_casm::instructions::Instruction;
use cairo_lang_casm::operand::{CellRef, Register};
use cairo_lang_sierra::extensions::builtin_cost::CostTokenType;
use cairo_lang_sierra::extensions::core::CoreConcreteLibfunc;
use cairo_lang_sierra::extensions::lib_func::BranchSignature;
use cairo_lang_sierra::extensions::{ConcreteLibfunc, OutputVarReferenceInfo};
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra::program::{BranchInfo, BranchTarget, Invocation, StatementIdx};
use cairo_lang_sierra_ap_change::core_libfunc_ap_change::{
    core_libfunc_ap_change, InvocationApChangeInfoProvider,
};
use cairo_lang_sierra_gas::core_libfunc_cost::InvocationCostInfoProvider;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::{zip_eq, Itertools};
use thiserror::Error;
use {cairo_lang_casm, cairo_lang_sierra};

use crate::environment::frame_state::{FrameState, FrameStateError};
use crate::environment::Environment;
use crate::metadata::Metadata;
use crate::references::{ReferenceExpression, ReferenceValue};
use crate::relocations::{Relocation, RelocationEntry};
use crate::type_sizes::TypeSizeMap;

mod array;
mod bitwise;
mod boolean;
mod boxing;
mod builtin_cost;
mod debug;
mod dict_felt_to;
mod ec;
mod enm;
mod felt;
mod function_call;
mod gas;
mod mem;
mod misc;
mod nullable;
mod pedersen;
mod starknet;

mod strct;
mod uint;
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
    #[error("The requested functionality is not implemented yet: {message}")]
    NotImplementedStr { invocation: Invocation, message: String },
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
        expressions: impl ExactSizeIterator<Item = ReferenceExpression>,
        branch_signature: &BranchSignature,
    ) -> Self {
        assert_eq!(
            expressions.len(),
            branch_signature.vars.len(),
            "The number of expressions does not match the number of expected results in the \
             branch."
        );
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

/// The cells per returned Sierra variables, in casm-builder vars.
type VarCells = [Var];
/// The configuration for all Sierra variables returned from a libfunc.
type AllVars<'a> = [&'a VarCells];

impl<'a> InvocationApChangeInfoProvider for CompiledInvocationBuilder<'a> {
    fn type_size(&self, ty: &ConcreteTypeId) -> usize {
        self.program_info.type_sizes[ty] as usize
    }

    fn token_usages(&self, token_type: CostTokenType) -> usize {
        self.program_info
            .metadata
            .gas_info
            .variable_values
            .get(&(self.idx, token_type))
            .copied()
            .unwrap_or(0) as usize
    }
}

impl<'a> InvocationCostInfoProvider for CompiledInvocationBuilder<'a> {
    fn type_size(&self, ty: &ConcreteTypeId) -> usize {
        self.program_info.type_sizes[ty] as usize
    }
}

/// Helper for building compiled invocations.
pub struct CompiledInvocationBuilder<'a> {
    pub program_info: ProgramInfo<'a>,
    pub invocation: &'a Invocation,
    pub libfunc: &'a CoreConcreteLibfunc,
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
        output_expressions: impl ExactSizeIterator<
            Item = impl ExactSizeIterator<Item = ReferenceExpression>,
        >,
    ) -> CompiledInvocation {
        let gas_changes = cairo_lang_sierra_gas::core_libfunc_cost::core_libfunc_cost(
            &self.program_info.metadata.gas_info,
            &self.idx,
            self.libfunc,
            &self,
        );

        let branch_signatures = self.libfunc.branch_signatures();
        assert_eq!(
            branch_signatures.len(),
            output_expressions.len(),
            "The number of output expressions does not match signature."
        );
        let ap_changes = core_libfunc_ap_change(self.libfunc, &self);
        assert_eq!(
            branch_signatures.len(),
            ap_changes.len(),
            "The number of ap changes does not match signature."
        );
        assert_eq!(
            branch_signatures.len(),
            gas_changes.len(),
            "The number of gas changes does not match signature."
        );

        CompiledInvocation {
            instructions,
            relocations,
            results: zip_eq(
                zip_eq(branch_signatures, gas_changes),
                zip_eq(output_expressions, ap_changes),
            )
            .map(|((branch_signature, gas_change), (expressions, ap_change))| {
                let ap_change = match ap_change {
                    cairo_lang_sierra_ap_change::ApChange::Known(x) => ApChange::Known(x),
                    cairo_lang_sierra_ap_change::ApChange::AtLocalsFinalization(_) => {
                        ApChange::Known(0)
                    }
                    cairo_lang_sierra_ap_change::ApChange::FinalizeLocals => {
                        match self.environment.frame_state {
                            FrameState::Finalized { allocated } => ApChange::Known(allocated),
                            _ => panic!("Unexpected frame state."),
                        }
                    }
                    cairo_lang_sierra_ap_change::ApChange::FunctionCall(id) => self
                        .program_info
                        .metadata
                        .ap_change_info
                        .function_ap_change
                        .get(&id)
                        .map_or(ApChange::Unknown, |x| ApChange::Known(x + 2)),
                    cairo_lang_sierra_ap_change::ApChange::FromMetadata => ApChange::Known(
                        *self
                            .program_info
                            .metadata
                            .ap_change_info
                            .variable_values
                            .get(&self.idx)
                            .unwrap_or(&0),
                    ),
                    cairo_lang_sierra_ap_change::ApChange::Unknown => ApChange::Unknown,
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

    /// Builds a `CompiledInvocation` from a casm builder and branch extractions.
    /// Per branch requires `(name, result_variables, target_statement_id)`.
    fn build_from_casm_builder<const BRANCH_COUNT: usize>(
        self,
        casm_builder: CasmBuilder,
        branch_extractions: [(&str, &AllVars<'_>, Option<StatementIdx>); BRANCH_COUNT],
    ) -> CompiledInvocation {
        let CasmBuildResult { instructions, branches } =
            casm_builder.build(branch_extractions.map(|(name, _, _)| name));
        itertools::assert_equal(
            core_libfunc_ap_change(self.libfunc, &self),
            branches
                .iter()
                .map(|(state, _)| cairo_lang_sierra_ap_change::ApChange::Known(state.ap_change)),
        );
        let relocations = branches
            .iter()
            .zip_eq(branch_extractions.iter())
            .flat_map(|((_, relocations), (_, _, target))| {
                assert_eq!(
                    relocations.is_empty(),
                    target.is_none(),
                    "No relocations if nowhere to relocate to."
                );
                relocations.iter().map(|idx| RelocationEntry {
                    instruction_idx: *idx,
                    relocation: Relocation::RelativeStatementId(target.unwrap()),
                })
            })
            .collect();
        let output_expressions = branches.into_iter().zip_eq(branch_extractions.into_iter()).map(
            |((state, _), (_, vars, _))| {
                vars.iter().map(move |var_cells| ReferenceExpression {
                    cells: var_cells.iter().map(|cell| state.get_adjusted(*cell)).collect(),
                })
            },
        );
        self.build(instructions, relocations, output_expressions)
    }

    /// Creates a new invocation with only reference changes.
    fn build_only_reference_changes(
        self,
        output_expressions: impl ExactSizeIterator<Item = ReferenceExpression>,
    ) -> CompiledInvocation {
        self.build(vec![], vec![], [output_expressions].into_iter())
    }

    /// Returns the reference expressions if the size is correct.
    pub fn try_get_refs<const COUNT: usize>(
        &self,
    ) -> Result<[&ReferenceExpression; COUNT], InvocationError> {
        if self.refs.len() == COUNT {
            Ok(core::array::from_fn(|i| &self.refs[i].expression))
        } else {
            Err(InvocationError::WrongNumberOfArguments {
                expected: COUNT,
                actual: self.refs.len(),
            })
        }
    }

    /// Returns the reference expressions, assuming all contains one cell if the size is correct.
    pub fn try_get_single_cells<const COUNT: usize>(
        &self,
    ) -> Result<[&CellExpression; COUNT], InvocationError> {
        let refs = self.try_get_refs::<COUNT>()?;
        let mut last_err = None;
        const FAKE_CELL: CellExpression =
            CellExpression::Deref(CellRef { register: Register::AP, offset: 0 });
        // TODO(orizi): Use `refs.try_map` once it is a stable feature.
        let result = refs.map(|r| match r.try_unpack_single() {
            Ok(cell) => cell,
            Err(err) => {
                last_err = Some(err);
                &FAKE_CELL
            }
        });
        if let Some(err) = last_err { Err(err) } else { Ok(result) }
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
    libfunc: &CoreConcreteLibfunc,
    idx: StatementIdx,
    refs: &[ReferenceValue],
    environment: Environment,
) -> Result<CompiledInvocation, InvocationError> {
    let builder =
        CompiledInvocationBuilder { program_info, invocation, libfunc, idx, refs, environment };
    match libfunc {
        // TODO(ilya, 10/10/2022): Handle type.
        CoreConcreteLibfunc::Felt(libfunc) => felt::build(libfunc, builder),
        CoreConcreteLibfunc::Bitwise(_) => bitwise::build(builder),
        CoreConcreteLibfunc::Bool(libfunc) => boolean::build(libfunc, builder),
        CoreConcreteLibfunc::Ec(libfunc) => ec::build(libfunc, builder),
        CoreConcreteLibfunc::Uint8(libfunc) => uint::build_u8(libfunc, builder),
        CoreConcreteLibfunc::Uint128(libfunc) => uint128::build(libfunc, builder),
        CoreConcreteLibfunc::Gas(libfunc) => gas::build(libfunc, builder),
        CoreConcreteLibfunc::BranchAlign(_) => misc::build_branch_align(builder),
        CoreConcreteLibfunc::Array(libfunc) => array::build(libfunc, builder),
        CoreConcreteLibfunc::Drop(_) => misc::build_drop(builder),
        CoreConcreteLibfunc::Dup(_) => misc::build_dup(builder),
        CoreConcreteLibfunc::Mem(libfunc) => mem::build(libfunc, builder),
        CoreConcreteLibfunc::UnwrapNonZero(_) => misc::build_identity(builder),
        CoreConcreteLibfunc::FunctionCall(libfunc) => function_call::build(libfunc, builder),
        CoreConcreteLibfunc::UnconditionalJump(_) => misc::build_jump(builder),
        CoreConcreteLibfunc::ApTracking(_) => misc::build_revoke_ap_tracking(builder),
        CoreConcreteLibfunc::Box(libfunc) => boxing::build(libfunc, builder),
        CoreConcreteLibfunc::Enum(libfunc) => enm::build(libfunc, builder),
        CoreConcreteLibfunc::Struct(libfunc) => strct::build(libfunc, builder),
        CoreConcreteLibfunc::DictFeltTo(libfunc) => dict_felt_to::build(libfunc, builder),
        CoreConcreteLibfunc::Pedersen(libfunc) => pedersen::build(libfunc, builder),
        CoreConcreteLibfunc::BuiltinCost(libfunc) => builtin_cost::build(libfunc, builder),
        CoreConcreteLibfunc::StarkNet(libfunc) => starknet::build(libfunc, builder),
        CoreConcreteLibfunc::Nullable(libfunc) => nullable::build(libfunc, builder),
        CoreConcreteLibfunc::Debug(libfunc) => debug::build(libfunc, builder),
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

/// Adds input variables into the builder while validating their type.
macro_rules! add_input_variables {
    ($casm_builder:ident,) => {};
    ($casm_builder:ident, deref $var:ident; $($tok:tt)*) => {
        let $var = $casm_builder.add_var(cairo_lang_casm::cell_expression::CellExpression::Deref(
            $var.to_deref().ok_or(InvocationError::InvalidReferenceExpressionForArgument)?,
        ));
        $crate::invocations::add_input_variables!($casm_builder, $($tok)*)
    };
    ($casm_builder:ident, deref_or_immediate $var:ident; $($tok:tt)*) => {
        let $var = $casm_builder.add_var(
            match $var
                .to_deref_or_immediate()
                .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?
            {
                cairo_lang_casm::operand::DerefOrImmediate::Deref(cell) => {
                    cairo_lang_casm::cell_expression::CellExpression::Deref(cell)
                }
                cairo_lang_casm::operand::DerefOrImmediate::Immediate(cell) => {
                    cairo_lang_casm::cell_expression::CellExpression::Immediate(cell)
                }
            },
        );
        $crate::invocations::add_input_variables!($casm_builder, $($tok)*)
    };
    ($casm_builder:ident, buffer($slack:expr) $var:ident; $($tok:tt)*) => {
        let $var = $casm_builder.add_var(
            $var.to_buffer($slack).ok_or(InvocationError::InvalidReferenceExpressionForArgument)?,
        );
        $crate::invocations::add_input_variables!($casm_builder, $($tok)*)
    };
}
use add_input_variables;
