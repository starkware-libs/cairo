use std::collections::HashMap;

use crate::edit_state::{put_results, take_args, EditError};
use crate::program::Identifier;

pub type State = HashMap<Identifier, i64>;

fn as_id(name: &str) -> Identifier {
    Identifier::Name(name.into())
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
