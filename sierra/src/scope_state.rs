use crate::{
    error::Error,
    graph::{Identifier, Type},
    utils::IdentifierState,
};
use std::collections::HashMap;
use Result::*;

pub type ScopeState = HashMap<Identifier, IdentifierState>;

pub struct ScopeChange<'a> {
    pub arg_ids: &'a Vec<Identifier>,
    pub arg_types: &'a Vec<Type>,
    pub res_ids: &'a Vec<Identifier>,
    pub res_types: &'a Vec<(Type, Vec<usize>)>,
}

pub fn next_state(mut state: ScopeState, change: ScopeChange<'_>) -> Result<ScopeState, Error> {
    if change.arg_ids.len() != change.arg_types.len() {
        return Err(Error::ArgumentSizeMismatch);
    }
    if change.res_ids.len() != change.res_types.len() {
        return Err(Error::ResultSizeMismatch);
    }
    let mut arg_temps = vec![];
    for (name, ty) in change.arg_ids.iter().zip(change.arg_types.iter()) {
        match state.remove(name) {
            None => {
                return Err(Error::MissingReference(name.clone(), ty.clone()));
            }
            Some(prev) => {
                if prev.ty != *ty {
                    return Err(Error::TypeMismatch(
                        name.clone(),
                        prev.ty.clone(),
                        ty.clone(),
                    ));
                }
                arg_temps.push(prev.is_temp);
            }
        }
    }
    for (name, (ty, deps)) in change.res_ids.iter().zip(change.res_types.iter()) {
        match state.insert(
            name.clone(),
            IdentifierState {
                ty: ty.clone(),
                is_temp: deps.iter().any(|d| arg_temps[*d]),
            },
        ) {
            Some(_) => return Err(Error::VariableOverride(name.clone())),
            None => {}
        }
    }
    Ok(state)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::as_local;

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
        let as_id_state = |name| as_local(as_type(name));
        assert_eq!(
            next_state(
                ScopeState::from([(as_id("arg"), as_id_state("Arg"))]),
                ScopeChange {
                    arg_ids: &vec![as_id("arg")],
                    arg_types: &vec![as_type("Arg")],
                    res_ids: &vec![as_id("res")],
                    res_types: &vec![(as_type("Res"), vec![])],
                }
            ),
            Ok(ScopeState::from([(as_id("res"), as_id_state("Res"))]))
        );
        assert_eq!(
            next_state(
                ScopeState::new(),
                ScopeChange {
                    arg_ids: &vec![as_id("arg")],
                    arg_types: &vec![as_type("Arg")],
                    res_ids: &vec![as_id("res")],
                    res_types: &vec![(as_type("Res"), vec![])],
                }
            ),
            Err(Error::MissingReference(as_id("arg"), as_type("Arg")))
        );
        assert_eq!(
            next_state(
                ScopeState::from([(as_id("arg"), as_id_state("ArgWrong"))]),
                ScopeChange {
                    arg_ids: &vec![as_id("arg")],
                    arg_types: &vec![as_type("Arg")],
                    res_ids: &vec![as_id("res")],
                    res_types: &vec![(as_type("Res"), vec![])],
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
                    (as_id("arg"), as_id_state("Arg")),
                    (as_id("res"), as_id_state("ResWrong"))
                ]),
                ScopeChange {
                    arg_ids: &vec![as_id("arg")],
                    arg_types: &vec![as_type("Arg")],
                    res_ids: &vec![as_id("res")],
                    res_types: &vec![(as_type("Res"), vec![])],
                }
            ),
            Err(Error::VariableOverride(as_id("res")))
        );
    }
}
