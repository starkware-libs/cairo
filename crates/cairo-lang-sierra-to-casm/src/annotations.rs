use std::collections::HashMap;
use std::iter;

use cairo_lang_casm::ap_change::{ApChange, ApChangeError, ApplyApChange};
use cairo_lang_sierra::edit_state::{put_results, take_args};
use cairo_lang_sierra::ids::{FunctionId, VarId};
use cairo_lang_sierra::program::{BranchInfo, Function, StatementIdx};
use itertools::zip_eq;
use thiserror::Error;

use crate::environment::ap_tracking::update_ap_tracking;
use crate::environment::frame_state::FrameStateError;
use crate::environment::gas_wallet::{GasWallet, GasWalletError};
use crate::environment::{
    validate_environment_equality, validate_final_environment, Environment, EnvironmentError,
};
use crate::invocations::BranchChanges;
use crate::metadata::Metadata;
use crate::references::{
    build_function_arguments_refs, check_types_match, ReferenceValue, ReferencesError,
    StatementRefs,
};
use crate::type_sizes::TypeSizeMap;

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
    #[error("#{statement_idx}: Invalid Ap change annotation. expected: {expected} got: {actual}.")]
    InvalidApChangeAnnotation { statement_idx: StatementIdx, expected: ApChange, actual: ApChange },
}

/// Annotation that represent the state at each program statement.
#[derive(Clone, Debug, Eq, PartialEq)]
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
                    refs: build_function_arguments_refs(func, type_sizes).map_err(|error| {
                        AnnotationError::ReferencesError { statement_idx: func.entry_point, error }
                    })?,
                    function_id: func.id.clone(),
                    convergence_allowed: false,
                    environment: if gas_usage_check {
                        Environment::new(GasWallet::Value(
                            metadata.gas_info.function_costs[func.id.clone()].clone(),
                        ))
                    } else {
                        Environment::new(GasWallet::Disabled)
                    },
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
                if expected_annotations.refs != annotations.refs {
                    return Err(AnnotationError::InconsistentReferencesAnnotation(statement_idx));
                }
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

                // Note that we ignore annotations here.
                // a flow cannot converge with a branch target.
                if !expected_annotations.convergence_allowed {
                    return Err(AnnotationError::InvalidConvergence { statement_idx });
                }
            }
        };
        Ok(())
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
                },
            );
        }
        self.set_or_assert(
            destination_statement_idx,
            StatementAnnotations {
                refs: put_results(new_refs, zip_eq(&branch_info.results, branch_changes.refs))
                    .map_err(|error| AnnotationError::OverrideReferenceError {
                        source_statement_idx,
                        destination_statement_idx,
                        var_id: error.var_id(),
                    })?,
                function_id: annotations.function_id.clone(),
                convergence_allowed: !must_set,

                environment: Environment {
                    ap_tracking: update_ap_tracking(
                        annotations.environment.ap_tracking,
                        branch_changes.ap_change,
                    )
                    .map_err(|error| AnnotationError::ApTrackingError {
                        source_statement_idx,
                        destination_statement_idx,
                        error,
                    })?,
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

        let expected_ap_change = match metadata.ap_change_info.function_ap_change.get(&func.id) {
            Some(x) => ApChange::Known(*x),
            _ => ApChange::Unknown,
        };

        if expected_ap_change != ApChange::Unknown
            && expected_ap_change != annotations.environment.ap_tracking
        {
            return Err(AnnotationError::InvalidApChangeAnnotation {
                statement_idx,
                expected: expected_ap_change,
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
