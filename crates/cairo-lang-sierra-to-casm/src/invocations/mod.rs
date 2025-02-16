use assert_matches::assert_matches;
use cairo_lang_casm::ap_change::ApChange;
use cairo_lang_casm::builder::{CasmBuildResult, CasmBuilder, Var};
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_casm::instructions::Instruction;
use cairo_lang_casm::operand::{CellRef, Register};
use cairo_lang_sierra::extensions::circuit::CircuitInfo;
use cairo_lang_sierra::extensions::core::CoreConcreteLibfunc::{self, *};
use cairo_lang_sierra::extensions::coupon::CouponConcreteLibfunc;
use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::extensions::lib_func::{BranchSignature, OutputVarInfo, SierraApChange};
use cairo_lang_sierra::extensions::{ConcreteLibfunc, OutputVarReferenceInfo};
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra::program::{BranchInfo, BranchTarget, Invocation, StatementIdx};
use cairo_lang_sierra_ap_change::core_libfunc_ap_change::{
    InvocationApChangeInfoProvider, core_libfunc_ap_change,
};
use cairo_lang_sierra_gas::core_libfunc_cost::{InvocationCostInfoProvider, core_libfunc_cost};
use cairo_lang_sierra_gas::objects::ConstCost;
use cairo_lang_sierra_type_size::TypeSizeMap;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::{Itertools, chain, zip_eq};
use num_bigint::BigInt;
use thiserror::Error;

use crate::circuit::CircuitsInfo;
use crate::environment::Environment;
use crate::environment::frame_state::{FrameState, FrameStateError};
use crate::metadata::Metadata;
use crate::references::{
    OutputReferenceValue, OutputReferenceValueIntroductionPoint, ReferenceExpression,
    ReferenceValue,
};
use crate::relocations::{InstructionsWithRelocations, Relocation, RelocationEntry};

mod array;
mod bitwise;
mod blake;
mod boolean;
mod boxing;
mod bytes31;
mod casts;
mod circuit;
mod const_type;
mod debug;
mod ec;
pub mod enm;
mod felt252;
mod felt252_dict;
mod function_call;
mod gas;
mod int;
mod mem;
mod misc;
mod nullable;
mod pedersen;
mod poseidon;
mod range;
mod range_reduction;
mod squashed_felt252_dict;
mod starknet;
mod structure;

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
    // TODO(lior): Remove this error once not used.
    #[error("This libfunc does not support pre-cost metadata yet.")]
    PreCostMetadataNotSupported,
    #[error("{output_ty} is not a contained in the circuit {circuit_ty}.")]
    InvalidCircuitOutput { output_ty: ConcreteTypeId, circuit_ty: ConcreteTypeId },
}

/// Describes a simple change in the ap tracking itself.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ApTrackingChange {
    /// Enables the tracking if not already enabled.
    Enable,
    /// Disables the tracking.
    Disable,
    /// No changes.
    None,
}

/// Describes the changes to the set of references at a single branch target, as well as changes to
/// the environment.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BranchChanges {
    /// New references defined at a given branch.
    /// should correspond to BranchInfo.results.
    pub refs: Vec<OutputReferenceValue>,
    /// The change to AP caused by the libfunc in the branch.
    pub ap_change: ApChange,
    /// A change to the ap tracking status.
    pub ap_tracking_change: ApTrackingChange,
    /// The change to the remaining gas value in the wallet.
    pub gas_change: OrderedHashMap<CostTokenType, i64>,
    /// Should the stack be cleared due to a gap between stack items.
    pub clear_old_stack: bool,
    /// The expected size of the known stack after the change.
    pub new_stack_size: usize,
}
impl BranchChanges {
    /// Creates a `BranchChanges` object.
    /// `param_ref` is used to fetch the reference value of a param of the libfunc.
    fn new<'a, ParamRef: Fn(usize) -> &'a ReferenceValue>(
        ap_change: ApChange,
        ap_tracking_change: ApTrackingChange,
        gas_change: OrderedHashMap<CostTokenType, i64>,
        expressions: impl ExactSizeIterator<Item = ReferenceExpression>,
        branch_signature: &BranchSignature,
        prev_env: &Environment,
        param_ref: ParamRef,
    ) -> Self {
        assert_eq!(
            expressions.len(),
            branch_signature.vars.len(),
            "The number of expressions does not match the number of expected results in the \
             branch."
        );
        let clear_old_stack =
            !matches!(&branch_signature.ap_change, SierraApChange::Known { new_vars_only: true });
        let stack_base = if clear_old_stack { 0 } else { prev_env.stack_size };
        let mut new_stack_size = stack_base;

        let refs: Vec<_> = zip_eq(expressions, &branch_signature.vars)
            .enumerate()
            .map(|(output_idx, (expression, OutputVarInfo { ref_info, ty }))| {
                validate_output_var_refs(ref_info, &expression);
                let stack_idx =
                    calc_output_var_stack_idx(ref_info, stack_base, clear_old_stack, &param_ref);
                if let Some(stack_idx) = stack_idx {
                    new_stack_size = new_stack_size.max(stack_idx + 1);
                }
                let introduction_point =
                    if let OutputVarReferenceInfo::SameAsParam { param_idx } = ref_info {
                        OutputReferenceValueIntroductionPoint::Existing(
                            param_ref(*param_idx).introduction_point.clone(),
                        )
                    } else {
                        // Marking the statement as unknown to be fixed later.
                        OutputReferenceValueIntroductionPoint::New(output_idx)
                    };
                OutputReferenceValue { expression, ty: ty.clone(), stack_idx, introduction_point }
            })
            .collect();
        validate_stack_top(ap_change, branch_signature, &refs);
        Self { refs, ap_change, ap_tracking_change, gas_change, clear_old_stack, new_stack_size }
    }
}

/// Validates that a new temp or local var have valid references in their matching expression.
fn validate_output_var_refs(ref_info: &OutputVarReferenceInfo, expression: &ReferenceExpression) {
    match ref_info {
        OutputVarReferenceInfo::SameAsParam { .. } => {}
        _ if expression.cells.is_empty() => {
            assert_matches!(ref_info, OutputVarReferenceInfo::ZeroSized);
        }
        OutputVarReferenceInfo::ZeroSized => {
            unreachable!("Non empty ReferenceExpression for zero sized variable.")
        }
        OutputVarReferenceInfo::NewTempVar { .. } => {
            expression.cells.iter().for_each(|cell| {
                assert_matches!(cell, CellExpression::Deref(CellRef { register: Register::AP, .. }))
            });
        }
        OutputVarReferenceInfo::NewLocalVar => {
            expression.cells.iter().for_each(|cell| {
                assert_matches!(cell, CellExpression::Deref(CellRef { register: Register::FP, .. }))
            });
        }
        OutputVarReferenceInfo::SimpleDerefs => {
            expression
                .cells
                .iter()
                .for_each(|cell| assert_matches!(cell, CellExpression::Deref(_)));
        }
        OutputVarReferenceInfo::PartialParam { .. } | OutputVarReferenceInfo::Deferred(_) => {}
    };
}

/// Validates that the variables that are now on the top of the stack are contiguous and that if the
/// stack was not broken the size of all the variables is consistent with the ap change.
fn validate_stack_top(
    ap_change: ApChange,
    branch_signature: &BranchSignature,
    refs: &[OutputReferenceValue],
) {
    // A mapping for the new temp vars allocated on the top of the stack from their index on the
    // top of the stack to their index in the `refs` vector.
    let stack_top_vars = UnorderedHashMap::<usize, usize>::from_iter(
        branch_signature.vars.iter().enumerate().filter_map(|(arg_idx, var)| {
            if let OutputVarReferenceInfo::NewTempVar { idx: stack_idx } = var.ref_info {
                Some((stack_idx, arg_idx))
            } else {
                None
            }
        }),
    );
    let mut prev_ap_offset = None;
    let mut stack_top_size = 0;
    for i in 0..stack_top_vars.len() {
        let Some(arg) = stack_top_vars.get(&i) else {
            panic!("Missing top stack var #{i} out of {}.", stack_top_vars.len());
        };
        let cells = &refs[*arg].expression.cells;
        stack_top_size += cells.len();
        for cell in cells {
            let ap_offset = match cell {
                CellExpression::Deref(CellRef { register: Register::AP, offset }) => *offset,
                _ => unreachable!("Tested in `validate_output_var_refs`."),
            };
            if let Some(prev_ap_offset) = prev_ap_offset {
                assert_eq!(ap_offset, prev_ap_offset + 1, "Top stack vars are not contiguous.");
            }
            prev_ap_offset = Some(ap_offset);
        }
    }
    if matches!(branch_signature.ap_change, SierraApChange::Known { new_vars_only: true }) {
        assert_eq!(
            ap_change,
            ApChange::Known(stack_top_size),
            "New tempvar variables are not contiguous with the old stack."
        );
    }
    // TODO(orizi): Add assertion for the non-new_vars_only case, that it is optimal.
}

/// Calculates the continuous stack index for an output var of a branch.
/// `param_ref` is used to fetch the reference value of a param of the libfunc.
fn calc_output_var_stack_idx<'a, ParamRef: Fn(usize) -> &'a ReferenceValue>(
    ref_info: &OutputVarReferenceInfo,
    stack_base: usize,
    clear_old_stack: bool,
    param_ref: &ParamRef,
) -> Option<usize> {
    match ref_info {
        OutputVarReferenceInfo::NewTempVar { idx } => Some(stack_base + idx),
        OutputVarReferenceInfo::SameAsParam { param_idx } if !clear_old_stack => {
            param_ref(*param_idx).stack_idx
        }
        OutputVarReferenceInfo::SameAsParam { .. }
        | OutputVarReferenceInfo::SimpleDerefs
        | OutputVarReferenceInfo::NewLocalVar
        | OutputVarReferenceInfo::PartialParam { .. }
        | OutputVarReferenceInfo::Deferred(_)
        | OutputVarReferenceInfo::ZeroSized => None,
    }
}

/// The result from a compilation of a single invocation statement.
#[derive(Debug)]
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

/// Checks that the list of references is contiguous on the stack and ends at ap - 1.
/// This is the requirement for function call and return statements.
pub fn check_references_on_stack(refs: &[ReferenceValue]) -> Result<(), InvocationError> {
    let mut expected_offset: i16 = -1;
    for reference in refs.iter().rev() {
        for cell_expr in reference.expression.cells.iter().rev() {
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

impl InvocationApChangeInfoProvider for CompiledInvocationBuilder<'_> {
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

impl InvocationCostInfoProvider for CompiledInvocationBuilder<'_> {
    fn type_size(&self, ty: &ConcreteTypeId) -> usize {
        self.program_info.type_sizes[ty] as usize
    }

    fn ap_change_var_value(&self) -> usize {
        self.program_info
            .metadata
            .ap_change_info
            .variable_values
            .get(&self.idx)
            .copied()
            .unwrap_or_default()
    }

    fn token_usages(&self, token_type: CostTokenType) -> usize {
        InvocationApChangeInfoProvider::token_usages(self, token_type)
    }

    fn circuit_info(&self, ty: &ConcreteTypeId) -> &CircuitInfo {
        self.program_info.circuits_info.circuits.get(ty).unwrap()
    }
}

/// Cost validation info for a builtin.
struct BuiltinInfo {
    /// The cost token type associated with the builtin.
    cost_token_ty: CostTokenType,
    /// The builtin pointer at the start of the libfunc.
    start: Var,
    /// The builtin pointer at the end of all the libfunc branches.
    end: Var,
}

/// Information required for validating libfunc cost.
#[derive(Default)]
struct CostValidationInfo<const BRANCH_COUNT: usize> {
    /// infos about builtin usage.
    pub builtin_infos: Vec<BuiltinInfo>,
    /// Possible extra cost per branch.
    /// Useful for amortized costs, as well as gas withdrawal libfuncs.
    pub extra_costs: Option<[i32; BRANCH_COUNT]>,
}

/// Helper for building compiled invocations.
pub struct CompiledInvocationBuilder<'a> {
    pub program_info: ProgramInfo<'a>,
    pub invocation: &'a Invocation,
    pub libfunc: &'a CoreConcreteLibfunc,
    pub idx: StatementIdx,
    /// The arguments of the libfunc.
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
        let gas_changes =
            core_libfunc_cost(&self.program_info.metadata.gas_info, &self.idx, self.libfunc, &self);

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
                let ap_tracking_change = match ap_change {
                    cairo_lang_sierra_ap_change::ApChange::EnableApTracking => {
                        ApTrackingChange::Enable
                    }
                    cairo_lang_sierra_ap_change::ApChange::DisableApTracking => {
                        ApTrackingChange::Disable
                    }
                    _ => ApTrackingChange::None,
                };
                let ap_change = match ap_change {
                    cairo_lang_sierra_ap_change::ApChange::Known(x) => ApChange::Known(x),
                    cairo_lang_sierra_ap_change::ApChange::AtLocalsFinalization(_)
                    | cairo_lang_sierra_ap_change::ApChange::EnableApTracking
                    | cairo_lang_sierra_ap_change::ApChange::DisableApTracking => {
                        ApChange::Known(0)
                    }
                    cairo_lang_sierra_ap_change::ApChange::FinalizeLocals => {
                        if let FrameState::Finalized { allocated } = self.environment.frame_state {
                            ApChange::Known(allocated)
                        } else {
                            panic!("Unexpected frame state.")
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
                    ap_tracking_change,
                    gas_change.iter().map(|(token_type, val)| (*token_type, -val)).collect(),
                    expressions,
                    branch_signature,
                    &self.environment,
                    |idx| &self.refs[idx],
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
        cost_validation: CostValidationInfo<BRANCH_COUNT>,
    ) -> CompiledInvocation {
        self.build_from_casm_builder_ex(
            casm_builder,
            branch_extractions,
            cost_validation,
            Default::default(),
        )
    }

    /// Builds a `CompiledInvocation` from a casm builder and branch extractions.
    /// Per branch requires `(name, result_variables, target_statement_id)`.
    ///
    /// `pre_instructions` - Instructions to execute before the ones created by the builder.
    fn build_from_casm_builder_ex<const BRANCH_COUNT: usize>(
        self,
        casm_builder: CasmBuilder,
        branch_extractions: [(&str, &AllVars<'_>, Option<StatementIdx>); BRANCH_COUNT],
        cost_validation: CostValidationInfo<BRANCH_COUNT>,
        pre_instructions: InstructionsWithRelocations,
    ) -> CompiledInvocation {
        let CasmBuildResult { instructions, branches } =
            casm_builder.build(branch_extractions.map(|(name, _, _)| name));
        let expected_ap_changes = core_libfunc_ap_change(self.libfunc, &self);
        let actual_ap_changes = branches
            .iter()
            .map(|(state, _)| cairo_lang_sierra_ap_change::ApChange::Known(state.ap_change));
        if !itertools::equal(expected_ap_changes.iter().cloned(), actual_ap_changes.clone()) {
            panic!(
                "Wrong ap changes for {}. Expected: {expected_ap_changes:?}, actual: {:?}.",
                self.invocation,
                actual_ap_changes.collect_vec(),
            );
        }
        let gas_changes =
            core_libfunc_cost(&self.program_info.metadata.gas_info, &self.idx, self.libfunc, &self)
                .into_iter()
                .map(|costs| costs.get(&CostTokenType::Const).copied().unwrap_or_default());
        let mut final_costs: [ConstCost; BRANCH_COUNT] =
            std::array::from_fn(|_| Default::default());
        for (cost, (state, _)) in final_costs.iter_mut().zip(branches.iter()) {
            cost.steps += state.steps as i32;
        }

        for BuiltinInfo { cost_token_ty, start, end } in cost_validation.builtin_infos {
            for (cost, (state, _)) in final_costs.iter_mut().zip(branches.iter()) {
                let (start_base, start_offset) =
                    state.get_adjusted(start).to_deref_with_offset().unwrap();
                let (end_base, end_offset) =
                    state.get_adjusted(end).to_deref_with_offset().unwrap();
                assert_eq!(start_base, end_base);
                let diff = end_offset - start_offset;
                match cost_token_ty {
                    CostTokenType::RangeCheck => {
                        cost.range_checks += diff;
                    }
                    CostTokenType::RangeCheck96 => {
                        cost.range_checks96 += diff;
                    }
                    _ => panic!("Cost token type not supported."),
                }
            }
        }

        let extra_costs =
            cost_validation.extra_costs.unwrap_or(std::array::from_fn(|_| Default::default()));
        let final_costs_with_extra =
            final_costs.iter().zip(extra_costs).map(|(final_cost, extra)| {
                (final_cost.cost() + extra + pre_instructions.cost.cost()) as i64
            });
        if !itertools::equal(gas_changes.clone(), final_costs_with_extra.clone()) {
            panic!(
                "Wrong costs for {}. Expected: {:?}, actual: {:?}, Costs from casm_builder: {:?}.",
                self.invocation,
                gas_changes.collect_vec(),
                final_costs_with_extra.collect_vec(),
                final_costs,
            );
        }
        let branch_relocations = branches.iter().zip_eq(branch_extractions.iter()).flat_map(
            |((_, relocations), (_, _, target))| {
                assert_eq!(
                    relocations.is_empty(),
                    target.is_none(),
                    "No relocations if nowhere to relocate to."
                );
                relocations.iter().map(|idx| RelocationEntry {
                    instruction_idx: pre_instructions.instructions.len() + *idx,
                    relocation: Relocation::RelativeStatementId(target.unwrap()),
                })
            },
        );
        let relocations = chain!(pre_instructions.relocations, branch_relocations).collect();
        let output_expressions =
            zip_eq(branches, branch_extractions).map(|((state, _), (_, vars, _))| {
                vars.iter().map(move |var_cells| ReferenceExpression {
                    cells: var_cells.iter().map(|cell| state.get_adjusted(*cell)).collect(),
                })
            });
        self.build(
            chain!(pre_instructions.instructions, instructions).collect(),
            relocations,
            output_expressions,
        )
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
    /// Information about the circuits in the program.
    pub circuits_info: &'a CircuitsInfo,
    /// Returns the given a const type returns a vector of cells value representing it.
    pub const_data_values: &'a dyn Fn(&ConcreteTypeId) -> Vec<BigInt>,
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
        Felt252(libfunc) => felt252::build(libfunc, builder),
        Felt252SquashedDict(libfunc) => squashed_felt252_dict::build(libfunc, builder),
        Bool(libfunc) => boolean::build(libfunc, builder),
        Cast(libfunc) => casts::build(libfunc, builder),
        Ec(libfunc) => ec::build(libfunc, builder),
        Uint8(libfunc) => int::unsigned::build_uint::<_, 0x100>(libfunc, builder),
        Uint16(libfunc) => int::unsigned::build_uint::<_, 0x10000>(libfunc, builder),
        Uint32(libfunc) => int::unsigned::build_uint::<_, 0x100000000>(libfunc, builder),
        Uint64(libfunc) => int::unsigned::build_uint::<_, 0x10000000000000000>(libfunc, builder),
        Uint128(libfunc) => int::unsigned128::build(libfunc, builder),
        Uint256(libfunc) => int::unsigned256::build(libfunc, builder),
        Uint512(libfunc) => int::unsigned512::build(libfunc, builder),
        Sint8(libfunc) => {
            int::signed::build_sint::<_, { i8::MIN as i128 }, { i8::MAX as i128 }>(libfunc, builder)
        }
        Sint16(libfunc) => {
            int::signed::build_sint::<_, { i16::MIN as i128 }, { i16::MAX as i128 }>(
                libfunc, builder,
            )
        }
        Sint32(libfunc) => {
            int::signed::build_sint::<_, { i32::MIN as i128 }, { i32::MAX as i128 }>(
                libfunc, builder,
            )
        }
        Sint64(libfunc) => {
            int::signed::build_sint::<_, { i64::MIN as i128 }, { i64::MAX as i128 }>(
                libfunc, builder,
            )
        }
        Sint128(libfunc) => int::signed128::build(libfunc, builder),
        Gas(libfunc) => gas::build(libfunc, builder),
        BranchAlign(_) => misc::build_branch_align(builder),
        Array(libfunc) => array::build(libfunc, builder),
        Drop(_) => misc::build_drop(builder),
        Dup(_) => misc::build_dup(builder),
        Mem(libfunc) => mem::build(libfunc, builder),
        UnwrapNonZero(_) => misc::build_identity(builder),
        FunctionCall(libfunc) | CouponCall(libfunc) => function_call::build(libfunc, builder),
        UnconditionalJump(_) => misc::build_jump(builder),
        ApTracking(_) => misc::build_update_ap_tracking(builder),
        Box(libfunc) => boxing::build(libfunc, builder),
        Enum(libfunc) => enm::build(libfunc, builder),
        Struct(libfunc) => structure::build(libfunc, builder),
        Felt252Dict(libfunc) => felt252_dict::build_dict(libfunc, builder),
        Pedersen(libfunc) => pedersen::build(libfunc, builder),
        Poseidon(libfunc) => poseidon::build(libfunc, builder),
        Starknet(libfunc) => starknet::build(libfunc, builder),
        Nullable(libfunc) => nullable::build(libfunc, builder),
        Debug(libfunc) => debug::build(libfunc, builder),
        SnapshotTake(_) => misc::build_dup(builder),
        Felt252DictEntry(libfunc) => felt252_dict::build_entry(libfunc, builder),
        Bytes31(libfunc) => bytes31::build(libfunc, builder),
        Const(libfunc) => const_type::build(libfunc, builder),
        Coupon(libfunc) => match libfunc {
            CouponConcreteLibfunc::Buy(_) => Ok(builder
                .build_only_reference_changes([ReferenceExpression::zero_sized()].into_iter())),
            CouponConcreteLibfunc::Refund(_) => {
                Ok(builder.build_only_reference_changes([].into_iter()))
            }
        },
        BoundedInt(libfunc) => int::bounded::build(libfunc, builder),
        Circuit(libfunc) => circuit::build(libfunc, builder),
        IntRange(libfunc) => range::build(libfunc, builder),
        Blake(libfunc) => blake::build(libfunc, builder),
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
            BranchInfo { target: BranchTarget::Fallthrough, results: _ },
            BranchInfo { target: BranchTarget::Statement(target_statement_id), results: _ },
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
                    cairo_lang_casm::cell_expression::CellExpression::Immediate(cell.value)
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
