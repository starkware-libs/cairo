use std::collections::HashMap;

use thiserror::Error;

use crate::program::Identifier;

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

#[cfg(test)]
mod tests {
    use super::*;
    pub type State = HashMap<Identifier, i64>;

    fn as_id(name: &str) -> Identifier {
        Identifier(name.to_string())
    }

    #[test]
    fn empty() {
        assert_eq!(take_args(State::new(), vec![].into_iter()), Ok((State::new(), vec![])));
        assert_eq!(put_results(State::new(), vec![].into_iter()), Ok(State::new()));
    }

    #[test]
    fn basic_mapping() {
        assert_eq!(
            take_args(State::from([(as_id("arg"), 0)]), vec![&as_id("arg")].into_iter(),),
            Ok((State::new(), vec![0]))
        );
        assert_eq!(
            put_results(State::new(), vec![(&as_id("res"), 1)].into_iter(),),
            Ok(State::from([(as_id("res"), 1)]))
        );
        assert_eq!(
            take_args(State::new(), vec![&as_id("arg")].into_iter(),),
            Err(EditError::MissingReference(as_id("arg")))
        );
        assert_eq!(
            put_results(State::from([(as_id("res"), 1)]), vec![(&as_id("res"), 1)].into_iter(),),
            Err(EditError::VariableOverride(as_id("res")))
        );
    }
}
