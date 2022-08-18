use std::collections::HashMap;
use std::iter;

use casm::operand::{DerefOperand, Register, ResOperand};
use sierra::ids::VarId;
use sierra::program::{Function, Param, StatementIdx};
use thiserror::Error;

type StatementRefs = HashMap<VarId, ResOperand>;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum ReferencesError {
    #[error("Inconsistent References.")]
    InconsistentReferences,
    #[error("InvalidStatementIdx")]
    InvalidStatementIdx,
}

pub struct ProgramReferences {
    // Per statement optional VarId => SierraReference mapping.
    pub per_statement_refs: Vec<Option<StatementRefs>>,
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
}

/// Builds the HashMap of references to the parameters of a function.
pub fn build_function_parameter_refs(params: &[Param]) -> Result<StatementRefs, ReferencesError> {
    let mut refs = HashMap::with_capacity(params.len());

    let mut offset = -2;
    for param in params.iter().rev() {
        if refs
            .insert(
                param.id.clone(),
                ResOperand::Deref(DerefOperand { register: Register::FP, offset }),
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
