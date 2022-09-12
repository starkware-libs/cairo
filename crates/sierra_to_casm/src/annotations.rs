use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::iter;

use casm::ap_change::{ApChangeError, ApplyApChange};
use itertools::zip_eq;
use sierra::edit_state::{put_results, take_args};
use sierra::ids::{ConcreteTypeId, VarId};
use sierra::program::{BranchInfo, Function, StatementIdx};
use thiserror::Error;

use crate::environment::frame_state::{update_frame_state, FrameState, FrameStateError};
use crate::environment::{validate_equal_equality, Environment, EnvironmentError};
use crate::invocations::BranchRefChanges;
use crate::references::{
    build_function_parameter_refs, check_types_match, ReferenceValue, ReferencesError,
    StatementRefs,
};

#[derive(Error, Debug, Eq, PartialEq)]
pub enum AnnotationError {
    #[error("#{0}: Inconsistent references annotations.")]
    InconsistentReferencesAnnotation(StatementIdx),
    #[error("{error} at #{statement_idx}.")]
    InconsistentEnvironments { statement_idx: StatementIdx, error: EnvironmentError },
    #[error("Inconsistent return type annotation.")]
    InconsistentReturnTypesAnnotation(StatementIdx),
    #[error("InvalidStatementIdx")]
    InvalidStatementIdx,
    #[error("MissingAnnotationsForStatement")]
    MissingAnnotationsForStatement(StatementIdx),
    #[error("{var_id} is not defined at #{statement_idx}.")]
    MissingReferenceError { statement_idx: StatementIdx, var_id: VarId },
    #[error(
        "{var_id} is overridden when moving from #{source_statement_idx} to \
         #{destination_statement_idx}."
    )]
    OverrideReferenceError {
        source_statement_idx: StatementIdx,
        destination_statement_idx: StatementIdx,
        var_id: VarId,
    },

    #[error(transparent)]
    FrameStateError(#[from] FrameStateError),
    #[error(transparent)]
    ReferencesError(#[from] ReferencesError),

    #[error(
        "Got '{error}' error while moving {var_id} from #{source_statement_idx} to \
         #{destination_statement_idx}."
    )]
    ApChangeError {
        var_id: VarId,
        source_statement_idx: StatementIdx,
        destination_statement_idx: StatementIdx,
        error: ApChangeError,
    },
}

/// An annotation that specifies the expected return types at each statement.
/// Used to propagate the return type to return statements.
/// Note that this is less strict then annotating each statement with the function it belongs to.
/// It is implemented as an index into ProgramAnnotations.return_types.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ReturnTypesAnnotation(usize);

/// Annotation that represent the state at each program statement.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StatementAnnotations {
    pub refs: StatementRefs,
    pub return_types: ReturnTypesAnnotation,
    pub environment: Environment,
}

/// Annotations of the program statements.
/// See StatementAnnotations.
pub struct ProgramAnnotations {
    // A vector of return types vectors that appear in the program.
    // ReturnTypesAnnotation is an index in this vector.
    return_types: Vec<Vec<ConcreteTypeId>>,
    // Optional per statement annotation.
    per_statement_annotations: Vec<Option<StatementAnnotations>>,
}
impl ProgramAnnotations {
    fn new(n_statements: usize) -> Self {
        ProgramAnnotations {
            return_types: vec![],
            per_statement_annotations: iter::repeat_with(|| None).take(n_statements).collect(),
        }
    }

    // Creates a ProgramAnnotations object based on 'n_statements' and a given functions list.
    pub fn create(n_statements: usize, functions: &[Function]) -> Result<Self, AnnotationError> {
        let mut annotations = ProgramAnnotations::new(n_statements);
        let mut return_annotations: HashMap<&Vec<ConcreteTypeId>, ReturnTypesAnnotation> =
            HashMap::new();
        for func in functions {
            let return_annotation = match return_annotations.entry(&func.ret_types) {
                Entry::Occupied(entry) => entry.get().clone(),
                Entry::Vacant(entry) => {
                    let new_type_annotations =
                        ReturnTypesAnnotation(annotations.return_types.len());
                    annotations.return_types.push(func.ret_types.clone());
                    entry.insert(new_type_annotations.clone());
                    new_type_annotations
                }
            };

            annotations.set_or_assert(
                func.entry,
                StatementAnnotations {
                    refs: build_function_parameter_refs(func)?,
                    return_types: return_annotation,
                    environment: Environment { frame_state: FrameState::Allocating { used: 0 } },
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
                if expected_annotations.return_types != annotations.return_types {
                    return Err(AnnotationError::InconsistentReturnTypesAnnotation(statement_id));
                }

                validate_equal_equality(
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

    // Propagate the annotations from the statement at 'statement_idx' to all the branches
    // from said statement.
    // 'annotations' is the result of calling get_annotations_after_take_args at
    // 'statement_idx' and 'per_branch_ref_changes' are the reference changes at each branch.
    pub fn propagate_annotations(
        &mut self,
        statement_idx: StatementIdx,
        annotations: StatementAnnotations,
        branches: &[BranchInfo],
        per_branch_ref_changes: impl Iterator<Item = BranchRefChanges>,
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
                    return_types: annotations.return_types.clone(),
                    environment: Environment {
                        frame_state: update_frame_state(
                            annotations.environment.frame_state.clone(),
                            branch_result.ap_change,
                        )?,
                    },
                },
            )?;
        }
        Ok(())
    }

    /// Checks that the list of reference contains types matching the given types.
    pub fn validate_return_type(
        &self,
        return_refs: &[ReferenceValue],
        return_types: ReturnTypesAnnotation,
    ) -> Result<(), AnnotationError> {
        check_types_match(return_refs, &self.return_types[return_types.0])?;
        Ok(())
    }
}
