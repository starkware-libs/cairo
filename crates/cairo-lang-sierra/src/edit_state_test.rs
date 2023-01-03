use std::collections::HashMap;

use test_log::test;

use crate::edit_state::{put_results, take_args, EditStateError};
use crate::ids::VarId;

pub type State = HashMap<VarId, i64>;

#[test]
fn empty() {
    assert_eq!(take_args(State::new(), vec![].into_iter()), Ok((State::new(), vec![])));
    assert_eq!(put_results(State::new(), vec![].into_iter()), Ok(State::new()));
}

#[test]
fn basic_mapping() {
    assert_eq!(
        take_args(State::from([("arg".into(), 0)]), vec![&"arg".into()].into_iter(),),
        Ok((State::new(), vec![0]))
    );
    assert_eq!(
        put_results(State::new(), vec![(&"res".into(), 1)].into_iter(),),
        Ok(State::from([("res".into(), 1)]))
    );
    assert_eq!(
        take_args(State::new(), vec![&"arg".into()].into_iter(),),
        Err(EditStateError::MissingReference("arg".into()))
    );
    assert_eq!(
        put_results(State::from([("res".into(), 1)]), vec![(&"res".into(), 1)].into_iter(),),
        Err(EditStateError::VariableOverride("res".into()))
    );
}
