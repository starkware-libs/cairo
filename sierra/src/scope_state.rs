use crate::{
    error::Error,
    graph::{Identifier, Type},
};
use std::collections::HashMap;
use Result::*;

pub type ScopeState = HashMap<Identifier, Type>;

pub struct ScopeChange<'a> {
    pub arg_ids: &'a Vec<Identifier>,
    pub arg_types: &'a Vec<Type>,
    pub res_ids: &'a Vec<Identifier>,
    pub res_types: &'a Vec<Type>,
}

pub fn next_state(mut state: ScopeState, change: ScopeChange<'_>) -> Result<ScopeState, Error> {
    if change.arg_ids.len() != change.arg_types.len() {
        return Err(Error::ArgumentSizeMismatch);
    }
    if change.res_ids.len() != change.res_types.len() {
        return Err(Error::ResultSizeMismatch);
    }
    for (name, ty) in change.arg_ids.iter().zip(change.arg_types.iter()) {
        match state.remove(name) {
            None => {
                return Err(Error::MissingReference(name.clone(), ty.clone()));
            }
            Some(prev_ty) => {
                if prev_ty != *ty {
                    return Err(Error::TypeMismatch(
                        name.clone(),
                        prev_ty.clone(),
                        ty.clone(),
                    ));
                }
            }
        }
    }
    for (name, ty) in change.res_ids.iter().zip(change.res_types.iter()) {
        match state.insert(name.clone(), ty.clone()) {
            Some(_) => return Err(Error::VariableOverride(name.clone())),
            None => {}
        }
    }
    Ok(state)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn as_id(name: &str) -> Identifier {
        Identifier(name.to_string())
    }

    #[test]
    fn empty() {
        assert_eq!(
            next_state(
                ScopeState::new(),
                ScopeChange {
                    arg_ids: &vec![],
                    arg_types: &vec![],
                    res_ids: &vec![],
                    res_types: &vec![],
                }
            ),
            Ok(ScopeState::new())
        );
    }

    #[test]
    fn basic_mapping() {
        let as_type = |name: &str| Type {
            name: name.to_string(),
            args: vec![],
        };
        assert_eq!(
            next_state(
                ScopeState::from([(as_id("arg"), as_type("Arg"))]),
                ScopeChange {
                    arg_ids: &vec![as_id("arg")],
                    arg_types: &vec![as_type("Arg")],
                    res_ids: &vec![as_id("res")],
                    res_types: &vec![as_type("Res")],
                }
            ),
            Ok(ScopeState::from([(as_id("res"), as_type("Res"))]))
        );
        assert_eq!(
            next_state(
                ScopeState::new(),
                ScopeChange {
                    arg_ids: &vec![as_id("arg")],
                    arg_types: &vec![as_type("Arg")],
                    res_ids: &vec![as_id("res")],
                    res_types: &vec![as_type("Res")],
                }
            ),
            Err(Error::MissingReference(as_id("arg"), as_type("Arg")))
        );
        assert_eq!(
            next_state(
                ScopeState::from([(as_id("arg"), as_type("ArgWrong"))]),
                ScopeChange {
                    arg_ids: &vec![as_id("arg")],
                    arg_types: &vec![as_type("Arg")],
                    res_ids: &vec![as_id("res")],
                    res_types: &vec![as_type("Res")],
                }
            ),
            Err(Error::TypeMismatch(
                as_id("arg"),
                as_type("ArgWrong"),
                as_type("Arg")
            ))
        );
        assert_eq!(
            next_state(
                ScopeState::from([
                    (as_id("arg"), as_type("Arg")),
                    (as_id("res"), as_type("ResWrong"))
                ]),
                ScopeChange {
                    arg_ids: &vec![as_id("arg")],
                    arg_types: &vec![as_type("Arg")],
                    res_ids: &vec![as_id("res")],
                    res_types: &vec![as_type("Res")],
                }
            ),
            Err(Error::VariableOverride(as_id("res")))
        );
    }
}
