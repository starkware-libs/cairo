use std::collections::HashMap;
use std::iter;

use casm::operand::{DerefOperand, Register, ResOperand};

use crate::compiler::CompilationError;
use crate::ids::VarId;
use crate::program::{Function, Param, StatementId};

type SierraReference = ResOperand;
type StatementRefs = HashMap<VarId, SierraReference>;

pub struct ProgramReferences {
    // Per statement optional VarId => SierraReference mapping.
    pub refs: Vec<Option<HashMap<VarId, SierraReference>>>,
}
impl ProgramReferences {
    pub fn new(n_statements: usize) -> Self {
        ProgramReferences { refs: iter::repeat_with(|| None).take(n_statements).collect() }
    }

    /// Sets the references at 'stmt_id' to 'refs'
    /// If the reference for this statement were set previously, assert that the previous
    /// assignment is consistent with the new assignment.
    pub fn set_or_assert(
        &mut self,
        stmt_id: StatementId,
        refs: StatementRefs,
    ) -> Result<(), CompilationError> {
        let idx = stmt_id.0;
        if idx > refs.len() {
            return Err(CompilationError::InvalidEntryPoint);
        }

        match &self.refs[idx] {
            Some(curr_refs) => {
                if curr_refs != &refs {
                    return Err(CompilationError::InconsistentReferences);
                }
            }
            None => self.refs[idx] = Some(refs),
        }
        Ok(())
    }
}

/// Builds the HashMap of references for the parameter of a function.
pub fn build_function_parameter_refs(
    params: &[Param],
) -> Result<HashMap<VarId, SierraReference>, CompilationError> {
    let mut refs = HashMap::with_capacity(params.len());

    let mut offset = -2;
    for param in params.iter().rev() {
        if refs
            .insert(
                param.id.clone(),
                SierraReference::Deref(DerefOperand { register: Register::FP, offset }),
            )
            .is_some()
        {
            return Err(CompilationError::InconsistentReferences);
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
) -> Result<ProgramReferences, CompilationError> {
    let mut refs = ProgramReferences::new(n_statements);
    for func in functions {
        refs.set_or_assert(func.entry, build_function_parameter_refs(&func.params)?)?;
    }

    Ok(refs)
}
