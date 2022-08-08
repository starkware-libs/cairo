use std::collections::HashMap;

use thiserror::Error;

use crate::program::VarId;

#[cfg(test)]
#[path = "edit_state_test.rs"]
mod tests;

#[derive(Error, Debug, PartialEq)]
pub enum EditError {
    #[error("Missing reference")]
    MissingReference(VarId),
    #[error("Overriden variable")]
    VariableOverride(VarId),
}

/// Given a map with var ids as keys, extracts out the given ids, failing if some id is missing.
pub fn take_args<'a, V: 'a + std::cmp::PartialEq>(
    mut state: HashMap<VarId, V>,
    ids: impl Iterator<Item = &'a VarId>,
) -> Result<(HashMap<VarId, V>, Vec<V>), EditError> {
    let mut vals = vec![];
    for id in ids {
        match state.remove(id) {
            None => {
                return Err(EditError::MissingReference(id.clone()));
            }
            Some(v) => {
                vals.push(v);
            }
        }
    }
    Ok((state, vals))
}

/// Adds the given pairs to map with var ids as keys, failing if some variable is overriden.
pub fn put_results<'a, V>(
    mut state: HashMap<VarId, V>,
    results: impl Iterator<Item = (&'a VarId, V)>,
) -> Result<HashMap<VarId, V>, EditError> {
    for (id, v) in results {
        match state.insert(id.clone(), v) {
            Some(_) => return Err(EditError::VariableOverride(id.clone())),
            None => {}
        }
    }
    Ok(state)
}
