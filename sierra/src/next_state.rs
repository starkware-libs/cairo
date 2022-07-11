use crate::{error::Error, graph::Identifier};
use std::collections::HashMap;
use Result::*;

pub fn next_state<'a, V: 'a + std::cmp::PartialEq + std::clone::Clone>(
    mut state: HashMap<Identifier, V>,
    args: impl Iterator<Item = (&'a Identifier, &'a V)>,
    results: impl Iterator<Item = (&'a Identifier, &'a V)>,
) -> Result<HashMap<Identifier, V>, Error> {
    for (id, v) in args {
        match state.remove(id) {
            None => {
                return Err(Error::MissingReference(id.clone()));
            }
            Some(prev_v) => {
                if prev_v != *v {
                    return Err(Error::TypeMismatch(id.clone()));
                }
            }
        }
    }
    for (id, v) in results {
        match state.insert(id.clone(), v.clone()) {
            Some(_) => return Err(Error::VariableOverride(id.clone())),
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
        assert_eq!(
            next_state(State::new(), vec![].into_iter(), vec![].into_iter(),),
            Ok(State::new())
        );
    }

    #[test]
    fn basic_mapping() {
        assert_eq!(
            next_state(
                State::from([(as_id("arg"), 0)]),
                vec![(&as_id("arg"), &0)].into_iter(),
                vec![(&as_id("res"), &1)].into_iter(),
            ),
            Ok(State::from([(as_id("res"), 1)]))
        );
        assert_eq!(
            next_state(
                State::new(),
                vec![(&as_id("arg"), &0)].into_iter(),
                vec![(&as_id("res"), &1)].into_iter(),
            ),
            Err(Error::MissingReference(as_id("arg")))
        );
        assert_eq!(
            next_state(
                State::from([(as_id("arg"), 0)]),
                vec![(&as_id("arg"), &1)].into_iter(),
                vec![(&as_id("res"), &1)].into_iter(),
            ),
            Err(Error::TypeMismatch(as_id("arg"),))
        );
        assert_eq!(
            next_state(
                State::from([(as_id("arg"), 0), (as_id("res"), 1)]),
                vec![(&as_id("arg"), &0)].into_iter(),
                vec![(&as_id("res"), &1)].into_iter(),
            ),
            Err(Error::VariableOverride(as_id("res")))
        );
    }
}
