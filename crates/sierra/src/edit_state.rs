use std::collections::HashMap;

use thiserror::Error;

use crate::program::Identifier;

#[cfg(test)]
#[path = "edit_state_test.rs"]
mod tests;

#[derive(Error, Debug, PartialEq)]
pub enum EditError {
    #[error("Missing reference")]
    MissingReference(Identifier),
    #[error("Overriden variable")]
    VariableOverride(Identifier),
}

// Given a map with identifiers as key, extracts out the given identifiers, failing if some
// identifier is missing.
pub fn take_args<'a, V: 'a + std::cmp::PartialEq>(
    mut state: HashMap<Identifier, V>,
    ids: impl Iterator<Item = &'a Identifier>,
) -> Result<(HashMap<Identifier, V>, Vec<V>), EditError> {
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

// Adds the given pairs to map with identifiers as key, failing if some variable is overriden.
pub fn put_results<'a, V>(
    mut state: HashMap<Identifier, V>,
    results: impl Iterator<Item = (&'a Identifier, V)>,
) -> Result<HashMap<Identifier, V>, EditError> {
    for (id, v) in results {
        match state.insert(id.clone(), v) {
            Some(_) => return Err(EditError::VariableOverride(id.clone())),
            None => {}
        }
    }
    Ok(state)
}
