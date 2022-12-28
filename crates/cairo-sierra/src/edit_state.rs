use std::collections::HashMap;

use thiserror::Error;

use crate::ids::VarId;

#[cfg(test)]
#[path = "edit_state_test.rs"]
mod test;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum EditStateError {
    #[error("Missing reference")]
    MissingReference(VarId),
    #[error("Overridden variable")]
    VariableOverride(VarId),
}
impl EditStateError {
    pub fn var_id(self) -> VarId {
        match self {
            EditStateError::MissingReference(var_id) => var_id,
            EditStateError::VariableOverride(var_id) => var_id,
        }
    }
}

/// Given a map with var ids as keys, extracts out the given ids, failing if some id is missing.
pub fn take_args<'a, V: 'a + std::cmp::PartialEq>(
    mut state: HashMap<VarId, V>,
    ids: impl Iterator<Item = &'a VarId>,
) -> Result<(HashMap<VarId, V>, Vec<V>), EditStateError> {
    let mut vals = vec![];
    for id in ids {
        match state.remove(id) {
            None => {
                return Err(EditStateError::MissingReference(id.clone()));
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
) -> Result<HashMap<VarId, V>, EditStateError> {
    for (id, v) in results {
        if state.insert(id.clone(), v).is_some() {
            return Err(EditStateError::VariableOverride(id.clone()));
        }
    }
    Ok(state)
}
