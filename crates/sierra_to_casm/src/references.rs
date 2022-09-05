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
use sierra::program::{Function, GenBranchInfo, Param, StatementIdx};
use thiserror::Error;

use crate::invocations::BranchRefChanges;

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

/// A reference to a value.
/// Corresponds to an argument or return value of a sierra statement.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ReferenceValue {
    pub expression: ReferenceExpression,
    pub ty: ConcreteTypeId,
}

type StatementRefs = HashMap<VarId, ReferenceValue>;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum ReferencesError {
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

pub struct ProgramReferences {
    // Per statement optional VarId => SierraReference mapping.
    per_statement_refs: Vec<Option<StatementRefs>>,
}
impl ProgramReferences {
    pub fn new(n_statements: usize) -> Self {
        ProgramReferences {
            per_statement_refs: iter::repeat_with(|| None).take(n_statements).collect(),
        }
    }

    /// Sets the references at 'statement_id' to 'statement_refs'
    /// If the reference for this statement were set previously, assert that the previous
    /// assignment is consistent with the new assignment.
    pub fn set_or_assert(
        &mut self,
        statement_id: StatementIdx,
        statement_refs: StatementRefs,
    ) -> Result<(), ReferencesError> {
        let idx = statement_id.0;
        match self.per_statement_refs.get(idx).ok_or(ReferencesError::InvalidStatementIdx)? {
            None => self.per_statement_refs[idx] = Some(statement_refs),
            Some(curr_refs) => {
                if curr_refs != &statement_refs {
                    return Err(ReferencesError::InconsistentReferences);
                }
            }
        };
        Ok(())
    }

    /// Applies take_args to the StatementRefs mapping at statement_idx.
    /// Assumes statement_idx is a valid index.
    pub fn take_references<'a>(
        &self,
        statement_idx: StatementIdx,
        ref_ids: impl Iterator<Item = &'a VarId>,
    ) -> Result<(StatementRefs, Vec<ReferenceValue>), ReferencesError> {
        let statement_refs = self.per_statement_refs[statement_idx.0]
            .as_ref()
            .ok_or(ReferencesError::MissingReferencesForStatement(statement_idx))?;
        Ok(take_args(statement_refs.clone(), ref_ids)?)
    }

    pub fn update_references(
        &mut self,
        statement_idx: StatementIdx,
        statement_refs: StatementRefs,
        branches: &[GenBranchInfo<StatementIdx>],
        per_branch_ref_changes: impl Iterator<Item = BranchRefChanges>,
    ) -> Result<(), ReferencesError> {
        for (branch_info, branch_result) in zip_eq(branches, per_branch_ref_changes) {
            let mut new_refs: StatementRefs =
                HashMap::with_capacity(statement_refs.len() + branch_result.refs.len());
            for (var_id, ref_value) in &statement_refs {
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
                put_results(new_refs, zip_eq(&branch_info.results, branch_result.refs))?,
            )?;
        }
        Ok(())
    }
}

/// Builds the HashMap of references to the parameters of a function.
pub fn build_function_parameter_refs(params: &[Param]) -> Result<StatementRefs, ReferencesError> {
    let mut refs = HashMap::with_capacity(params.len());

    let mut offset = -2;
    for param in params.iter().rev() {
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
            return Err(ReferencesError::InconsistentReferences);
        }
        // TODO(ilya, 10/10/2022): Get size from type.
        let size = 1;
        offset -= size;
    }

    Ok(refs)
}

// Creates a ProgramReferences object based on the given functions list.
pub fn init_reference(
    n_statements: usize,
    functions: &[Function],
) -> Result<ProgramReferences, ReferencesError> {
    let mut refs = ProgramReferences::new(n_statements);
    for func in functions {
        refs.set_or_assert(func.entry, build_function_parameter_refs(&func.params)?)?;
    }

    Ok(refs)
}
