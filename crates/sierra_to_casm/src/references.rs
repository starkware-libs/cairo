use std::collections::HashMap;
use std::iter;

use casm::ap_change::{ApChange, ApChangeError, ApplyApChange};
use casm::operand::{
    DerefOperand, DerefOrImmediate, DoubleDerefOperand, ImmediateOperand, Register,
};
use itertools::zip_eq;
use sierra::edit_state::{put_results, take_args, EditStateError};
use sierra::extensions::arithmetic::Operator;
use sierra::ids::{ConcreteTypeId, VarId};
use sierra::program::{Function, GenBranchInfo, StatementIdx};
use thiserror::Error;

use crate::invocations::BranchRefChanges;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum ReferencesError {
    #[error("Invalid function declaration.")]
    InvalidFunctionDeclaration(Function),
    #[error("Inconsistent References.")]
    InconsistentReferences,
    #[error("InvalidStatementIdx")]
    InvalidStatementIdx,
    #[error("MissingReferencesForStatement")]
    MissingReferencesForStatement(StatementIdx),
    #[error("DanglingReferences")]
    DanglingReferences(StatementIdx),
    #[error(transparent)]
    EditStateError(#[from] EditStateError),
    #[error(transparent)]
    ApChangeError(#[from] ApChangeError),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BinOpExpression {
    pub op: Operator,
    pub a: DerefOperand,
    pub b: DerefOrImmediate,
}
impl ApplyApChange for BinOpExpression {
    fn apply_ap_change(self, ap_change: ApChange) -> Result<Self, ApChangeError> {
        Ok(BinOpExpression {
            op: self.op,
            a: self.a.apply_ap_change(ap_change)?,
            b: self.b.apply_ap_change(ap_change)?,
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ReferenceExpression {
    Deref(DerefOperand),
    DoubleDeref(DoubleDerefOperand),
    Immediate(ImmediateOperand),
    BinOp(BinOpExpression),
}

impl ApplyApChange for ReferenceExpression {
    fn apply_ap_change(self, ap_change: ApChange) -> Result<Self, ApChangeError> {
        Ok(match self {
            ReferenceExpression::Deref(operand) => {
                ReferenceExpression::Deref(operand.apply_ap_change(ap_change)?)
            }
            ReferenceExpression::DoubleDeref(operand) => {
                ReferenceExpression::DoubleDeref(operand.apply_ap_change(ap_change)?)
            }
            ReferenceExpression::Immediate(operand) => {
                ReferenceExpression::Immediate(operand.apply_ap_change(ap_change)?)
            }
            ReferenceExpression::BinOp(operand) => {
                ReferenceExpression::BinOp(operand.apply_ap_change(ap_change)?)
            }
        })
    }
}

// TODO(ilya, 10/10/2022): Move annotation related code to annotations.rs.

/// A reference to a value.
/// Corresponds to an argument or return value of a sierra statement.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ReferenceValue {
    pub expression: ReferenceExpression,
    pub ty: ConcreteTypeId,
}

type StatementRefs = HashMap<VarId, ReferenceValue>;

/// Annotation that represent the state at each program statement.
/// Currently this only includes the live references at each statement.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StatementAnnotations {
    pub refs: StatementRefs,
}

/// Annotations of the program statements.
/// See StatementAnnotations.
pub struct ProgramAnnotations {
    // Optional per statement annotation.
    per_statement_annotations: Vec<Option<StatementAnnotations>>,
}
impl ProgramAnnotations {
    fn new(n_statements: usize) -> Self {
        ProgramAnnotations {
            per_statement_annotations: iter::repeat_with(|| None).take(n_statements).collect(),
        }
    }

    // Creates a ProgramAnnotations object based on 'n_statements' and a given functions list.
    pub fn create(n_statements: usize, functions: &[Function]) -> Result<Self, ReferencesError> {
        let mut annotations = ProgramAnnotations::new(n_statements);
        for func in functions {
            annotations.set_or_assert(
                func.entry,
                StatementAnnotations { refs: build_function_parameter_refs(func)? },
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
    ) -> Result<(), ReferencesError> {
        let idx = statement_id.0;
        match self.per_statement_annotations.get(idx).ok_or(ReferencesError::InvalidStatementIdx)? {
            None => self.per_statement_annotations[idx] = Some(annotations),
            Some(curr_refs) => {
                if *curr_refs != annotations {
                    return Err(ReferencesError::InconsistentReferences);
                }
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
    ) -> Result<(StatementAnnotations, Vec<ReferenceValue>), ReferencesError> {
        let statement_annotations = &self.per_statement_annotations[statement_idx.0]
            .as_ref()
            .ok_or(ReferencesError::MissingReferencesForStatement(statement_idx))?;

        let (statement_refs, taken_refs) = take_args(statement_annotations.refs.clone(), ref_ids)?;
        Ok((StatementAnnotations { refs: statement_refs }, taken_refs))
    }

    // Propagate the annotations from the statement at 'statement_idx' to all the branches
    // from said statement.
    // 'annotations' is the result of calling get_annotations_after_take_args at
    // 'statement_idx' and 'per_branch_ref_changes' are the reference changes at each branch.
    pub fn propagate_annotations(
        &mut self,
        statement_idx: StatementIdx,
        annotations: StatementAnnotations,
        branches: &[GenBranchInfo<StatementIdx>],
        per_branch_ref_changes: impl Iterator<Item = BranchRefChanges>,
    ) -> Result<(), ReferencesError> {
        for (branch_info, branch_result) in zip_eq(branches, per_branch_ref_changes) {
            let mut new_refs: StatementRefs =
                HashMap::with_capacity(annotations.refs.len() + branch_result.refs.len());
            for (var_id, ref_value) in &annotations.refs {
                new_refs.insert(
                    var_id.clone(),
                    ReferenceValue {
                        expression: ref_value
                            .expression
                            .clone()
                            .apply_ap_change(branch_result.ap_change)?,
                        ty: ref_value.ty.clone(),
                    },
                );
            }

            self.set_or_assert(
                statement_idx.next(&branch_info.target),
                StatementAnnotations {
                    refs: put_results(new_refs, zip_eq(&branch_info.results, branch_result.refs))?,
                },
            )?;
        }
        Ok(())
    }
}

/// Builds the HashMap of references to the parameters of a function.
pub fn build_function_parameter_refs(func: &Function) -> Result<StatementRefs, ReferencesError> {
    let mut refs = HashMap::with_capacity(func.params.len());

    let mut offset = -2;
    for param in func.params.iter().rev() {
        if refs
            .insert(
                param.id.clone(),
                ReferenceValue {
                    expression: ReferenceExpression::Deref(DerefOperand {
                        register: Register::FP,
                        offset,
                    }),
                    ty: param.ty.clone(),
                },
            )
            .is_some()
        {
            return Err(ReferencesError::InvalidFunctionDeclaration(func.clone()));
        }
        // TODO(ilya, 10/10/2022): Get size from type.
        let size = 1;
        offset -= size;
    }

    Ok(refs)
}
