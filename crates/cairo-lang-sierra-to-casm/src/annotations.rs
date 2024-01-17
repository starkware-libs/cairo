use std::collections::HashMap;
use std::iter;

use cairo_lang_casm::ap_change::{ApChangeError, ApplyApChange};
use cairo_lang_sierra::edit_state::{put_results, take_args};
use cairo_lang_sierra::ids::{FunctionId, VarId};
use cairo_lang_sierra::program::{BranchInfo, Function, StatementIdx};
use cairo_lang_sierra_type_size::TypeSizeMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use itertools::zip_eq;
use thiserror::Error;

use crate::environment::ap_tracking::update_ap_tracking;
use crate::environment::frame_state::FrameStateError;
use crate::environment::gas_wallet::{GasWallet, GasWalletError};
use crate::environment::{
    validate_environment_equality, validate_final_environment, ApTracking, ApTrackingBase,
    Environment, EnvironmentError,
};
use crate::invocations::{ApTrackingChange, BranchChanges};
use crate::metadata::Metadata;
use crate::references::{
    build_function_parameters_refs, check_types_match, IntroductionPoint,
    OutputReferenceValueIntroductionPoint, ReferenceValue, ReferencesError, StatementRefs,
};

#[derive(Error, Debug, Eq, PartialEq)]
pub enum AnnotationError {
    #[error("#{0}: Inconsistent references annotations.")]
    InconsistentReferencesAnnotation(StatementIdx),
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
         {var_id}."
    )]
    ApChangeError {
        var_id: VarId,
        source_statement_idx: StatementIdx,
        destination_statement_idx: StatementIdx,
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
}
impl ProgramAnnotations {
    fn new(n_statements: usize) -> Self {
        ProgramAnnotations {
            per_statement_annotations: iter::repeat_with(|| None).take(n_statements).collect(),
        }
    }

    /// Creates a ProgramAnnotations object based on 'n_statements', a given functions list
    /// and metadata for the program.
    pub fn create(
        n_statements: usize,
        functions: &[Function],
        metadata: &Metadata,
        gas_usage_check: bool,
        type_sizes: &TypeSizeMap,
    ) -> Result<Self, AnnotationError> {
        let mut annotations = ProgramAnnotations::new(n_statements);
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
                if !self.test_references_consistency(&annotations, expected_annotations) {
                    return Err(AnnotationError::InconsistentReferencesAnnotation(statement_idx));
                }

                // Note that we ignore annotations here.
                // a flow cannot converge with a branch target.
                if !expected_annotations.convergence_allowed {
                    return Err(AnnotationError::InvalidConvergence { statement_idx });
                }
            }
        };
        Ok(())
    }

    /// Returns whether or not `actual` and `expected` references are consistent.
    fn test_references_consistency(
        &self,
        actual: &StatementAnnotations,
        expected: &StatementAnnotations,
    ) -> bool {
        // Check if there is a mismatch at the number of variables.
        if actual.refs.len() != expected.refs.len() {
            return false;
        }
        let ap_tracking_enabled =
            matches!(actual.environment.ap_tracking, ApTracking::Enabled { .. });
        for (var_id, actual_ref) in actual.refs.iter() {
            // Check if the variable exists in just one of the branches.
            let Some(expected_ref) = expected.refs.get(var_id) else {
                return false;
            };
            // Check if the variable doesn't match on type, expression or stack information.
            if !(actual_ref.ty == expected_ref.ty
                && actual_ref.expression == expected_ref.expression
                && actual_ref.stack_idx == expected_ref.stack_idx)
            {
                return false;
            }
            if !test_var_consistency(actual_ref, expected_ref, ap_tracking_enabled) {
                return false;
            }
        }
        true
    }

    /// Returns the result of applying take_args to the StatementAnnotations at statement_idx.
    pub fn get_annotations_after_take_args<'a>(
        &self,
        statement_idx: StatementIdx,
        ref_ids: impl Iterator<Item = &'a VarId>,
    ) -> Result<(StatementAnnotations, Vec<ReferenceValue>), AnnotationError> {
        let statement_annotations = self.per_statement_annotations[statement_idx.0]
            .as_ref()
            .ok_or(AnnotationError::MissingAnnotationsForStatement(statement_idx))?
            .clone();

        let (statement_refs, taken_refs) =
            take_args(statement_annotations.refs, ref_ids).map_err(|error| {
                AnnotationError::MissingReferenceError { statement_idx, var_id: error.var_id() }
            })?;
        Ok((StatementAnnotations { refs: statement_refs, ..statement_annotations }, taken_refs))
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
        annotations: &StatementAnnotations,
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

        let mut new_refs: StatementRefs =
            HashMap::with_capacity(annotations.refs.len() + branch_changes.refs.len());
        for (var_id, ref_value) in &annotations.refs {
            new_refs.insert(
                var_id.clone(),
                ReferenceValue {
                    expression: ref_value
                        .expression
                        .clone()
                        .apply_ap_change(branch_changes.ap_change)
                        .map_err(|error| AnnotationError::ApChangeError {
                            var_id: var_id.clone(),
                            source_statement_idx,
                            destination_statement_idx,
                            error,
                        })?,
                    ty: ref_value.ty.clone(),
                    stack_idx: if branch_changes.clear_old_stack {
                        None
                    } else {
                        ref_value.stack_idx
                    },
                    introduction_point: ref_value.introduction_point.clone(),
                },
            );
        }
        let mut refs = put_results(
            new_refs,
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
            for r in refs.values_mut() {
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
                function_id: annotations.function_id.clone(),
                convergence_allowed: !must_set,
                environment: Environment {
                    ap_tracking,
                    stack_size,
                    frame_state: annotations.environment.frame_state.clone(),
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

/// Returns whether or not the references `actual` and `expected` are consistent and can be merged
/// in a way that will be re-compilable.
fn test_var_consistency(
    actual: &ReferenceValue,
    expected: &ReferenceValue,
    ap_tracking_enabled: bool,
) -> bool {
    // If the variable is on the stack, it can always be merged.
    if actual.stack_idx.is_some() {
        return true;
    }
    // If the variable is not ap-dependent it can always be merged.
    // Note: This makes the assumption that empty variables are always mergeable.
    if actual.expression.can_apply_unknown() {
        return true;
    }
    // Ap tracking must be enabled when merging non-stack ap-dependent variables.
    if !ap_tracking_enabled {
        return false;
    }
    // Merged variables must have the same introduction point.
    actual.introduction_point == expected.introduction_point
}
