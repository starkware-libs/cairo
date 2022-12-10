use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::iter;

use casm::ap_change::{ApChange, ApChangeError, ApplyApChange};
use itertools::zip_eq;
use sierra::edit_state::{put_results, take_args};
use sierra::ids::{ConcreteTypeId, VarId};
use sierra::program::{BranchInfo, Function, StatementIdx};
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
    #[error("#{statement_idx}: {error}")]
    InconsistentEnvironments { statement_idx: StatementIdx, error: EnvironmentError },
    #[error("Inconsistent return type annotation.")]
    InconsistentReturnAnnotation(StatementIdx),
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
    #[error(transparent)]
    GasWalletError(#[from] GasWalletError),
    #[error(transparent)]
    ReferencesError(#[from] ReferencesError),

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

/// An annotation that specifies the expected return properties at each statement.
/// Used to propagate the return properties to return statements.
/// Implemented as an index into ProgramAnnotations.return_properties.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ReturnAnnotation(usize);

/// Annotation that represent the state at each program statement.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StatementAnnotations {
    pub refs: StatementRefs,
    pub return_annotation: ReturnAnnotation,
    pub environment: Environment,
}

/// A set of properties that need to be validated in return statement.
/// Each statement in the program is annotated with said properties in order to
/// propagate them to return statements.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ReturnProperties {
    /// The expected return types at a return statement.
    return_types: Vec<ConcreteTypeId>,

    /// The expected ap change at a return statement.
    ap_change: ApChange,
}

/// Annotations of the program statements.
/// See StatementAnnotations.
pub struct ProgramAnnotations {
    /// A vector of return properties.
    /// ReturnAnnotation is an index in this vector.
    return_properties: Vec<ReturnProperties>,

    /// Optional per statement annotation.
    per_statement_annotations: Vec<Option<StatementAnnotations>>,
}
impl ProgramAnnotations {
    fn new(n_statements: usize) -> Self {
        ProgramAnnotations {
            return_properties: vec![],
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
        let mut return_annotations: HashMap<ReturnProperties, ReturnAnnotation> = HashMap::new();
        for func in functions {
            let ap_change = match metadata.ap_change_info.function_ap_change.get(&func.id) {
                Some(x) => ApChange::Known(*x),
                _ => ApChange::Unknown,
            };

            let return_properties =
                ReturnProperties { return_types: (func.signature.ret_types.clone()), ap_change };

            let return_annotation = match return_annotations.entry(return_properties) {
                Entry::Occupied(entry) => entry.get().clone(),
                Entry::Vacant(entry) => {
                    let new_type_annotations =
                        ReturnAnnotation(annotations.return_properties.len());

                    annotations.return_properties.push(entry.key().clone());
                    entry.insert(new_type_annotations.clone());
                    new_type_annotations
                }
            };

            annotations.set_or_assert(
                func.entry_point,
                StatementAnnotations {
                    refs: build_function_arguments_refs(func, type_sizes)?,
                    return_annotation,
                    environment: if gas_usage_check {
                        Environment::new(GasWallet::Value(
                            metadata.gas_info.function_costs[&func.id].clone(),
                        ))
                    } else {
                        Environment::new(GasWallet::Disabled)
                    },
                },
            )?
        }

        Ok(annotations)
    }

    /// Sets the annotations at 'statement_id' to 'annotations'
    /// If the annotations for this statement were set previously, assert that the previous
    /// assignment is consistent with the new assignment.
    pub fn set_or_assert(
        &mut self,
        statement_id: StatementIdx,
        annotations: StatementAnnotations,
    ) -> Result<(), AnnotationError> {
        let idx = statement_id.0;
        match self.per_statement_annotations.get(idx).ok_or(AnnotationError::InvalidStatementIdx)? {
            None => self.per_statement_annotations[idx] = Some(annotations),
            Some(expected_annotations) => {
                if expected_annotations.refs != annotations.refs {
                    return Err(AnnotationError::InconsistentReferencesAnnotation(statement_id));
                }
                if expected_annotations.return_annotation != annotations.return_annotation {
                    return Err(AnnotationError::InconsistentReturnAnnotation(statement_id));
                }

                validate_environment_equality(
                    &expected_annotations.environment,
                    &annotations.environment,
                )
                .map_err(|error| AnnotationError::InconsistentEnvironments {
                    statement_idx: statement_id,
                    error,
                })?;
            }
        };
        Ok(())
    }

    /// Returns the result of applying take_args to the StatementAnnotations at statement_idx.
    /// Assumes statement_idx is a valid index.
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

    /// Propagates the annotations from the statement at `statement_idx` to all the branches
    /// from said statement.
    /// `annotations` is the result of calling get_annotations_after_take_args at
    /// `statement_idx` and `per_branch_ref_changes` are the reference changes at each branch.
    pub fn propagate_annotations(
        &mut self,
        statement_idx: StatementIdx,
        annotations: StatementAnnotations,
        branches: &[BranchInfo],
        per_branch_ref_changes: impl Iterator<Item = BranchChanges>,
    ) -> Result<(), AnnotationError> {
        for (branch_info, branch_result) in zip_eq(branches, per_branch_ref_changes) {
            let destination_statement_idx = statement_idx.next(&branch_info.target);

            let mut new_refs: StatementRefs =
                HashMap::with_capacity(annotations.refs.len() + branch_result.refs.len());
            for (var_id, ref_value) in &annotations.refs {
                new_refs.insert(
                    var_id.clone(),
                    ReferenceValue {
                        expression: ref_value
                            .expression
                            .clone()
                            .apply_ap_change(branch_result.ap_change)
                            .map_err(|error| AnnotationError::ApChangeError {
                                var_id: var_id.clone(),
                                source_statement_idx: statement_idx,
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
                    refs: put_results(new_refs, zip_eq(&branch_info.results, branch_result.refs))
                        .map_err(|error| AnnotationError::OverrideReferenceError {
                        source_statement_idx: statement_idx,
                        destination_statement_idx,
                        var_id: error.var_id(),
                    })?,
                    return_annotation: annotations.return_annotation.clone(),
                    environment: Environment {
                        ap_tracking: update_ap_tracking(
                            annotations.environment.ap_tracking,
                            branch_result.ap_change,
                        )
                        .map_err(|error| {
                            AnnotationError::ApTrackingError {
                                source_statement_idx: statement_idx,
                                destination_statement_idx,
                                error,
                            }
                        })?,
                        frame_state: annotations.environment.frame_state.clone(),
                        gas_wallet: annotations
                            .environment
                            .gas_wallet
                            .update(branch_result.gas_change)?,
                    },
                },
            )?;
        }
        Ok(())
    }

    /// Validates the ap change and return types in a return statement.
    pub fn validate_return_properties(
        &self,
        statement_idx: StatementIdx,
        annotations: &StatementAnnotations,
        return_refs: &[ReferenceValue],
    ) -> Result<(), AnnotationError> {
        let return_properties = &self.return_properties[annotations.return_annotation.0];

        if return_properties.ap_change != ApChange::Unknown
            && return_properties.ap_change != annotations.environment.ap_tracking
        {
            return Err(AnnotationError::InvalidApChangeAnnotation {
                statement_idx,
                expected: return_properties.ap_change,
                actual: annotations.environment.ap_tracking,
            });
        }

        // Checks that the list of return reference contains has the expected types.
        check_types_match(return_refs, &return_properties.return_types)?;
        Ok(())
    }

    /// Validates the final annotation in a return statement.
    pub fn validate_final_annotations(
        &self,
        statement_idx: StatementIdx,
        annotations: &StatementAnnotations,
        return_refs: &[ReferenceValue],
    ) -> Result<(), AnnotationError> {
        self.validate_return_properties(statement_idx, annotations, return_refs)?;
        validate_final_environment(&annotations.environment)
            .map_err(|error| AnnotationError::InconsistentEnvironments { statement_idx, error })
    }
}
