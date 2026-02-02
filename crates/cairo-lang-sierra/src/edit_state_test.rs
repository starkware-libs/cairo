use cairo_lang_test_utils::test;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::edit_state::{EditState, EditStateError};
use crate::ids::VarId;

#[test]
fn empty() {
    assert_eq!(take_args([], vec![]), Ok((State::default(), vec![])));
    assert_eq!(put_results([], vec![]), Ok(State::default()));
}

#[test]
fn basic_mapping() {
    assert_eq!(take_args([("arg".into(), 0)], vec!["arg".into()]), Ok((State::default(), vec![0])));
    assert_eq!(put_results([], vec![("res".into(), 1)]), Ok(State::from([("res".into(), 1)])));
    assert_eq!(
        take_args([], vec!["arg".into()]),
        Err(EditStateError::MissingReference("arg".into()))
    );
    assert_eq!(
        put_results([("res".into(), 1)], vec![("res".into(), 1)]),
        Err(EditStateError::VariableOverride("res".into()))
    );
}

/// The type for the state in the testing.
pub type State = OrderedHashMap<VarId, i64>;

/// Test helper wrapper for `VarManagement::take_vars`.
fn take_args(
    state: impl Into<State>,
    ids: Vec<VarId>,
) -> Result<(State, Vec<i64>), EditStateError> {
    let mut state: State = state.into();
    let taken = state.take_vars(ids.iter())?;
    Ok((state, taken))
}

/// Test helper wrapper for `VarManagement::put_vars`.
fn put_results(
    state: impl Into<State>,
    results: Vec<(VarId, i64)>,
) -> Result<State, EditStateError> {
    let mut state: State = state.into();
    state.put_vars(results.iter().map(|(k, v)| (k, *v)))?;
    Ok(state)
}
