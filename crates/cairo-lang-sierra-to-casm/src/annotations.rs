use std::iter;

use cairo_lang_casm::ap_change::{ApChangeError, ApplyApChange};
use cairo_lang_sierra::edit_state::{put_results, take_args};
use cairo_lang_sierra::ids::{ConcreteTypeId, FunctionId, VarId};
use cairo_lang_sierra::program::{BranchInfo, Function, StatementIdx};
use cairo_lang_sierra_type_size::TypeSizeMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use itertools::{chain, zip_eq};
use thiserror::Error;

use crate::environment::ap_tracking::update_ap_tracking;
use crate::environment::frame_state::FrameStateError;
use crate::environment::gas_wallet::{GasWallet, GasWalletError};
use crate::environment::{
    ApTracking, ApTrackingBase, Environment, EnvironmentError, validate_environment_equality,
    validate_final_environment,
};
use crate::invocations::{ApTrackingChange, BranchChanges};
use crate::metadata::Metadata;
use crate::references::{
    IntroductionPoint, OutputReferenceValueIntroductionPoint, ReferenceExpression, ReferenceValue,
    ReferencesError, StatementRefs, build_function_parameters_refs, check_types_match,
};

#[derive(Error, Debug, Eq, PartialEq)]
pub enum AnnotationError {
    #[error("#{statement_idx}: Inconsistent references annotations: {error}")]
    InconsistentReferencesAnnotation {
        statement_idx: StatementIdx,
        error: InconsistentReferenceError,
    },
    #[error("#{source_statement_idx}->#{destination_statement_idx}: Annotation was already set.")]
    AnnotationAlreadySet {
        source_statement_idx: StatementIdx,
        destination_statement_idx: StatementIdx,
    },
    #[error("#{statement_idx}: {error}")]
    InconsistentEnvironments { statement_idx: StatementIdx, error: EnvironmentError },
    #[error("#{statement_idx}: Belongs to two different functions.")]
    InconsistentFunctionId { statement_idx: StatementIdx },
    #[error("#{statement_idx}: Invalid convergence.")]
    InvalidConvergence { statement_idx: StatementIdx },
    #[error("InvalidStatementIdx")]
    InvalidStatementIdx,
    #[error("MissingAnnotationsForStatement")]
    MissingAnnotationsForStatement(StatementIdx),
    #[error("#{statement_idx}: {var_id} is undefined.")]
    MissingReferenceError { statement_idx: StatementIdx, var_id: VarId },
    #[error("#{source_statement_idx}->#{destination_statement_idx}: {var_id} was overridden.")]
    OverrideReferenceError {
        source_statement_idx: StatementIdx,
        destination_statement_idx: StatementIdx,
        var_id: VarId,
    },
    #[error(transparent)]
    FrameStateError(#[from] FrameStateError),
    #[error("#{source_statement_idx}->#{destination_statement_idx}: {error}")]
    GasWalletError {
        source_statement_idx: StatementIdx,
        destination_statement_idx: StatementIdx,
        error: GasWalletError,
    },
    #[error("#{statement_idx}: {error}")]
    ReferencesError { statement_idx: StatementIdx, error: ReferencesError },
    #[error("#{statement_idx}: Attempting to enable ap tracking when already enabled.")]
    ApTrackingAlreadyEnabled { statement_idx: StatementIdx },
    #[error(
        "#{source_statement_idx}->#{destination_statement_idx}: Got '{error}' error while moving \
         {var_id} introduced at {} .", {introduction_point}
    )]
    ApChangeError {
        var_id: VarId,
        source_statement_idx: StatementIdx,
        destination_statement_idx: StatementIdx,
        introduction_point: IntroductionPoint,
        error: ApChangeError,
    },
    #[error("#{source_statement_idx} -> #{destination_statement_idx}: Ap tracking error")]
    ApTrackingError {
        source_statement_idx: StatementIdx,
        destination_statement_idx: StatementIdx,
        error: ApChangeError,
    },
    #[error(
        "#{statement_idx}: Invalid function ap change annotation. Expected ap tracking: \
         {expected:?}, got: {actual:?}."
    )]
    InvalidFunctionApChange {
        statement_idx: StatementIdx,
        expected: ApTracking,
        actual: ApTracking,
    },
}

impl AnnotationError {
    pub fn stmt_indices(&self) -> Vec<StatementIdx> {
        match self {
            AnnotationError::ApChangeError {
                source_statement_idx,
                destination_statement_idx,
                introduction_point,
                ..
            } => chain!(
                [source_statement_idx, destination_statement_idx],
                &introduction_point.source_statement_idx,
                [&introduction_point.destination_statement_idx]
            )
            .cloned()
            .collect(),
            _ => vec![],
        }
    }
}

/// Error representing an inconsistency in the references annotations.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum InconsistentReferenceError {
    #[error("Variable {var} type mismatch. Expected `{expected}`, got `{actual}`.")]
    TypeMismatch { var: VarId, expected: ConcreteTypeId, actual: ConcreteTypeId },
    #[error("Variable {var} expression mismatch. Expected `{expected}`, got `{actual}`.")]
    ExpressionMismatch { var: VarId, expected: ReferenceExpression, actual: ReferenceExpression },
    #[error("Variable {var} stack index mismatch. Expected `{expected:?}`, got `{actual:?}`.")]
    StackIndexMismatch { var: VarId, expected: Option<usize>, actual: Option<usize> },
    #[error("Variable {var} introduction point mismatch. Expected `{expected}`, got `{actual}`.")]
    IntroductionPointMismatch { var: VarId, expected: IntroductionPoint, actual: IntroductionPoint },
    #[error("Variable count mismatch.")]
    VariableCountMismatch,
    #[error("Missing expected variable {0}.")]
    VariableMissing(VarId),
    #[error("Ap tracking is disabled while trying to merge {0}.")]
    ApTrackingDisabled(VarId),
}

/// Annotation that represent the state at each program statement.
#[derive(Clone, Debug)]
pub struct StatementAnnotations {
    pub refs: StatementRefs,
    /// The function id that the statement belongs to.
    pub function_id: FunctionId,
    /// Indicates whether convergence in allowed in the given statement.
    pub convergence_allowed: bool,
    pub environment: Environment,
}

/// Annotations of the program statements.
/// See StatementAnnotations.
pub struct ProgramAnnotations {
    /// Optional per statement annotation.
    per_statement_annotations: Vec<Option<StatementAnnotations>>,
    /// The indices of the statements that are the targets of backwards jumps.
    backwards_jump_indices: UnorderedHashSet<StatementIdx>,
}
impl ProgramAnnotations {
    fn new(n_statements: usize, backwards_jump_indices: UnorderedHashSet<StatementIdx>) -> Self {
        ProgramAnnotations {
            per_statement_annotations: iter::repeat_with(|| None).take(n_statements).collect(),
            backwards_jump_indices,
        }
    }

    /// Creates a ProgramAnnotations object based on 'n_statements', a given functions list
    /// and metadata for the program.
    pub fn create(
        n_statements: usize,
        backwards_jump_indices: UnorderedHashSet<StatementIdx>,
        functions: &[Function],
        metadata: &Metadata,
        gas_usage_check: bool,
        type_sizes: &TypeSizeMap,
    ) -> Result<Self, AnnotationError> {
        let mut annotations = ProgramAnnotations::new(n_statements, backwards_jump_indices);
        for func in functions {
            annotations.set_or_assert(
                func.entry_point,
                StatementAnnotations {
                    refs: build_function_parameters_refs(func, type_sizes).map_err(|error| {
                        AnnotationError::ReferencesError { statement_idx: func.entry_point, error }
                    })?,
                    function_id: func.id.clone(),
                    convergence_allowed: false,
                    environment: Environment::new(if gas_usage_check {
                        GasWallet::Value(metadata.gas_info.function_costs[&func.id].clone())
                    } else {
                        GasWallet::Disabled
                    }),
                },
            )?
        }

        Ok(annotations)
    }

    /// Sets the annotations at 'statement_idx' to 'annotations'
    /// If the annotations for this statement were set previously asserts that the previous
    /// assignment is consistent with the new assignment and verifies that convergence_allowed
    /// is true.
    pub fn set_or_assert(
        &mut self,
        statement_idx: StatementIdx,
        annotations: StatementAnnotations,
    ) -> Result<(), AnnotationError> {
        let idx = statement_idx.0;
        match self.per_statement_annotations.get(idx).ok_or(AnnotationError::InvalidStatementIdx)? {
            None => self.per_statement_annotations[idx] = Some(annotations),
            Some(expected_annotations) => {
                if expected_annotations.function_id != annotations.function_id {
                    return Err(AnnotationError::InconsistentFunctionId { statement_idx });
                }
                validate_environment_equality(
                    &expected_annotations.environment,
                    &annotations.environment,
                )
                .map_err(|error| AnnotationError::InconsistentEnvironments {
                    statement_idx,
                    error,
                })?;
                self.test_references_consistency(&annotations, expected_annotations).map_err(
                    |error| AnnotationError::InconsistentReferencesAnnotation {
                        statement_idx,
                        error,
                    },
                )?;

                // Note that we ignore annotations here.
                // a flow cannot converge with a branch target.
                if !expected_annotations.convergence_allowed {
                    return Err(AnnotationError::InvalidConvergence { statement_idx });
                }
            }
        };
        Ok(())
    }

    /// Checks whether or not `actual` and `expected` references are consistent.
    /// Returns an error representing the inconsistency.
    fn test_references_consistency(
        &self,
        actual: &StatementAnnotations,
        expected: &StatementAnnotations,
    ) -> Result<(), InconsistentReferenceError> {
        // Check if there is a mismatch at the number of variables.
        if actual.refs.len() != expected.refs.len() {
            return Err(InconsistentReferenceError::VariableCountMismatch);
        }
        let ap_tracking_enabled =
            matches!(actual.environment.ap_tracking, ApTracking::Enabled { .. });
        for (var_id, actual_ref) in actual.refs.iter() {
            // Check if the variable exists in just one of the branches.
            let Some(expected_ref) = expected.refs.get(var_id) else {
                return Err(InconsistentReferenceError::VariableMissing(var_id.clone()));
            };
            // Check if the variable doesn't match on type, expression or stack information.
            if actual_ref.ty != expected_ref.ty {
                return Err(InconsistentReferenceError::TypeMismatch {
                    var: var_id.clone(),
                    expected: expected_ref.ty.clone(),
                    actual: actual_ref.ty.clone(),
                });
            }
            if actual_ref.expression != expected_ref.expression {
                return Err(InconsistentReferenceError::ExpressionMismatch {
                    var: var_id.clone(),
                    expected: expected_ref.expression.clone(),
                    actual: actual_ref.expression.clone(),
                });
            }
            if actual_ref.stack_idx != expected_ref.stack_idx {
                return Err(InconsistentReferenceError::StackIndexMismatch {
                    var: var_id.clone(),
                    expected: expected_ref.stack_idx,
                    actual: actual_ref.stack_idx,
                });
            }
            test_var_consistency(var_id, actual_ref, expected_ref, ap_tracking_enabled)?;
        }
        Ok(())
    }

    /// Returns the result of applying take_args to the StatementAnnotations at statement_idx.
    /// Can be called only once per item, the item is removed from the annotations, and can no
    /// longer be used for merges.
    pub fn get_annotations_after_take_args<'a>(
        &mut self,
        statement_idx: StatementIdx,
        ref_ids: impl Iterator<Item = &'a VarId>,
    ) -> Result<(StatementAnnotations, Vec<ReferenceValue>), AnnotationError> {
        let existing = self.per_statement_annotations[statement_idx.0]
            .as_mut()
            .ok_or(AnnotationError::MissingAnnotationsForStatement(statement_idx))?;
        let mut updated = if self.backwards_jump_indices.contains(&statement_idx) {
            existing.clone()
        } else {
            std::mem::replace(
                existing,
                StatementAnnotations {
                    refs: Default::default(),
                    function_id: existing.function_id.clone(),
                    // Merging with this data is no longer allowed.
                    convergence_allowed: false,
                    environment: existing.environment.clone(),
                },
            )
        };
        let refs = std::mem::take(&mut updated.refs);
        let (statement_refs, taken_refs) = take_args(refs, ref_ids).map_err(|error| {
            AnnotationError::MissingReferenceError { statement_idx, var_id: error.var_id() }
        })?;
        updated.refs = statement_refs;
        Ok((updated, taken_refs))
    }

    /// Propagates the annotations from `statement_idx` to 'destination_statement_idx'.
    ///
    /// `annotations` is the result of calling get_annotations_after_take_args at
    /// `source_statement_idx` and `branch_changes` are the reference changes at each branch.
    ///  if `must_set` is true, asserts that destination_statement_idx wasn't annotated before.
    pub fn propagate_annotations(
        &mut self,
        source_statement_idx: StatementIdx,
        destination_statement_idx: StatementIdx,
        mut annotations: StatementAnnotations,
        branch_info: &BranchInfo,
        branch_changes: BranchChanges,
        must_set: bool,
    ) -> Result<(), AnnotationError> {
        if must_set && self.per_statement_annotations[destination_statement_idx.0].is_some() {
            return Err(AnnotationError::AnnotationAlreadySet {
                source_statement_idx,
                destination_statement_idx,
            });
        }

        for (var_id, ref_value) in annotations.refs.iter_mut() {
            if branch_changes.clear_old_stack {
                ref_value.stack_idx = None;
            }
            ref_value.expression =
                std::mem::replace(&mut ref_value.expression, ReferenceExpression::zero_sized())
                    .apply_ap_change(branch_changes.ap_change)
                    .map_err(|error| AnnotationError::ApChangeError {
                        var_id: var_id.clone(),
                        source_statement_idx,
                        destination_statement_idx,
                        introduction_point: ref_value.introduction_point.clone(),
                        error,
                    })?;
        }
        let mut refs = put_results(
            annotations.refs,
            zip_eq(
                &branch_info.results,
                branch_changes.refs.into_iter().map(|value| ReferenceValue {
                    expression: value.expression,
                    ty: value.ty,
                    stack_idx: value.stack_idx,
                    introduction_point: match value.introduction_point {
                        OutputReferenceValueIntroductionPoint::New(output_idx) => {
                            IntroductionPoint {
                                source_statement_idx: Some(source_statement_idx),
                                destination_statement_idx,
                                output_idx,
                            }
                        }
                        OutputReferenceValueIntroductionPoint::Existing(introduction_point) => {
                            introduction_point
                        }
                    },
                }),
            ),
        )
        .map_err(|error| AnnotationError::OverrideReferenceError {
            source_statement_idx,
            destination_statement_idx,
            var_id: error.var_id(),
        })?;

        // Since some variables on the stack may have been consumed by the libfunc, we need to
        // find the new stack size. This is done by searching from the bottom of the stack until we
        // find a missing variable.
        let available_stack_indices: UnorderedHashSet<_> =
            refs.values().flat_map(|r| r.stack_idx).collect();
        let new_stack_size_opt = (0..branch_changes.new_stack_size)
            .find(|i| !available_stack_indices.contains(&(branch_changes.new_stack_size - 1 - i)));
        let stack_size = if let Some(new_stack_size) = new_stack_size_opt {
            // The number of stack elements which were removed.
            let stack_removal = branch_changes.new_stack_size - new_stack_size;
            for (_, r) in refs.iter_mut() {
                // Subtract the number of stack elements removed. If the result is negative,
                // `stack_idx` is set to `None` and the variable is removed from the stack.
                r.stack_idx =
                    r.stack_idx.and_then(|stack_idx| stack_idx.checked_sub(stack_removal));
            }
            new_stack_size
        } else {
            branch_changes.new_stack_size
        };

        let ap_tracking = match branch_changes.ap_tracking_change {
            ApTrackingChange::Disable => ApTracking::Disabled,
            ApTrackingChange::Enable => {
                if !matches!(annotations.environment.ap_tracking, ApTracking::Disabled) {
                    return Err(AnnotationError::ApTrackingAlreadyEnabled {
                        statement_idx: source_statement_idx,
                    });
                }
                ApTracking::Enabled {
                    ap_change: 0,
                    base: ApTrackingBase::Statement(destination_statement_idx),
                }
            }
            ApTrackingChange::None => {
                update_ap_tracking(annotations.environment.ap_tracking, branch_changes.ap_change)
                    .map_err(|error| AnnotationError::ApTrackingError {
                        source_statement_idx,
                        destination_statement_idx,
                        error,
                    })?
            }
        };

        self.set_or_assert(
            destination_statement_idx,
            StatementAnnotations {
                refs,
                function_id: annotations.function_id,
                convergence_allowed: !must_set,
                environment: Environment {
                    ap_tracking,
                    stack_size,
                    frame_state: annotations.environment.frame_state,
                    gas_wallet: annotations
                        .environment
                        .gas_wallet
                        .update(branch_changes.gas_change)
                        .map_err(|error| AnnotationError::GasWalletError {
                            source_statement_idx,
                            destination_statement_idx,
                            error,
                        })?,
                },
            },
        )
    }

    /// Validates the ap change and return types in a return statement.
    pub fn validate_return_properties(
        &self,
        statement_idx: StatementIdx,
        annotations: &StatementAnnotations,
        functions: &[Function],
        metadata: &Metadata,
        return_refs: &[ReferenceValue],
    ) -> Result<(), AnnotationError> {
        // TODO(ilya): Don't use linear search.
        let func = &functions.iter().find(|func| func.id == annotations.function_id).unwrap();

        let expected_ap_tracking = match metadata.ap_change_info.function_ap_change.get(&func.id) {
            Some(x) => ApTracking::Enabled { ap_change: *x, base: ApTrackingBase::FunctionStart },
            None => ApTracking::Disabled,
        };
        if annotations.environment.ap_tracking != expected_ap_tracking {
            return Err(AnnotationError::InvalidFunctionApChange {
                statement_idx,
                expected: expected_ap_tracking,
                actual: annotations.environment.ap_tracking,
            });
        }

        // Checks that the list of return reference contains has the expected types.
        check_types_match(return_refs, &func.signature.ret_types)
            .map_err(|error| AnnotationError::ReferencesError { statement_idx, error })?;
        Ok(())
    }

    /// Validates the final annotation in a return statement.
    pub fn validate_final_annotations(
        &self,
        statement_idx: StatementIdx,
        annotations: &StatementAnnotations,
        functions: &[Function],
        metadata: &Metadata,
        return_refs: &[ReferenceValue],
    ) -> Result<(), AnnotationError> {
        self.validate_return_properties(
            statement_idx,
            annotations,
            functions,
            metadata,
            return_refs,
        )?;
        validate_final_environment(&annotations.environment)
            .map_err(|error| AnnotationError::InconsistentEnvironments { statement_idx, error })
    }
}

/// Checks whether or not the references `actual` and `expected` are consistent and can be merged
/// in a way that will be re-compilable.
/// Returns an error representing the inconsistency.
fn test_var_consistency(
    var_id: &VarId,
    actual: &ReferenceValue,
    expected: &ReferenceValue,
    ap_tracking_enabled: bool,
) -> Result<(), InconsistentReferenceError> {
    // If the variable is on the stack, it can always be merged.
    if actual.stack_idx.is_some() {
        return Ok(());
    }
    // If the variable is not ap-dependent it can always be merged.
    // Note: This makes the assumption that empty variables are always mergeable.
    if actual.expression.can_apply_unknown() {
        return Ok(());
    }
    // Ap tracking must be enabled when merging non-stack ap-dependent variables.
    if !ap_tracking_enabled {
        return Err(InconsistentReferenceError::ApTrackingDisabled(var_id.clone()));
    }
    // Merged variables must have the same introduction point.
    if actual.introduction_point == expected.introduction_point {
        Ok(())
    } else {
        Err(InconsistentReferenceError::IntroductionPointMismatch {
            var: var_id.clone(),
            expected: expected.introduction_point.clone(),
            actual: actual.introduction_point.clone(),
        })
    }
}
