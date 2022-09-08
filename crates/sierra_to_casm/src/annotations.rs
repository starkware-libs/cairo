use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::iter;

use sierra::ids::ConcreteTypeId;
use sierra::program::{Function, StatementIdx};
use thiserror::Error;

use crate::references::{
    build_function_parameter_refs, check_types_match, ReferenceValue, ReferencesError,
    StatementRefs,
};

#[derive(Error, Debug, Eq, PartialEq)]
pub enum AnnotationError {
    #[error("Inconsistent references annotations.")]
    InconsistentReferencesAnnotation(StatementIdx),
    #[error("Inconsistent return type annotation.")]
    InconsistentReturnTypesAnnotation(StatementIdx),
    #[error("InvalidStatementIdx")]
    InvalidStatementIdx,
    #[error("MissingAnnotationsForStatement")]
    MissingAnnotationsForStatement(StatementIdx),
    #[error(transparent)]
    ReferencesError(#[from] ReferencesError),
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
            }
        };
        Ok(())
    }

    /// Gets the annotations for statement_idx.
    pub fn get_annotations(
        &self,
        statement_idx: StatementIdx,
    ) -> Result<&StatementAnnotations, AnnotationError> {
        self.per_statement_annotations[statement_idx.0]
            .as_ref()
            .ok_or(AnnotationError::MissingAnnotationsForStatement(statement_idx))
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
