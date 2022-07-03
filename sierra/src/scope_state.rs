use crate::error::Error;
use crate::graph::*;
use std::collections::HashMap;
use std::collections::HashSet;
use Result::*;

type ScopeState = HashMap<String, Type>;
type TypeSet = HashSet<Type>;

#[derive(Clone, Debug, PartialEq)]
pub struct TypedMapping {
    pub args: Vec<TypedVar>,
    pub results: Vec<TypedVar>,
}

pub fn prev_state(
    unowned_types: &TypeSet,
    mapping: &TypedMapping,
    mut state: ScopeState,
) -> Result<ScopeState, Error> {
    for res in &mapping.results {
        match state.remove(&res.name) {
            None => {
                return Err(Error::MissingReference);
            }
            Some(ty) => {
                if ty != res.ty {
                    return Err(Error::TypeMismatch);
                }
            }
        }
    }
    for arg in &mapping.args {
        match state.insert(arg.name.clone(), arg.ty.clone()) {
            Some(ty) => {
                if arg.ty != ty {
                    return Err(Error::TypeMismatch);
                }
                if !unowned_types.contains(&ty) {
                    return Err(Error::UnconsumedOwnedType);
                }
            }
            None => {}
        }
    }
    Ok(state)
}

fn next_state(
    unowned_types: &TypeSet,
    mapping: &TypedMapping,
    mut state: ScopeState,
) -> Result<ScopeState, Error> {
    for arg in &mapping.args {
        match state.get(&arg.name) {
            None => {
                return Err(Error::MissingReference);
            }
            Some(ty) => {
                if *ty != arg.ty {
                    return Err(Error::TypeMismatch);
                }
            }
        }
        if !unowned_types.contains(&arg.ty) {
            state.remove(&arg.name);
        }
    }
    for res in &mapping.results {
        match state.insert(res.name.clone(), res.ty.clone()) {
            Some(_) => return Err(Error::VariableOverride),
            None => {}
        }
    }
    Ok(state)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        let unowned_types = TypeSet::new();
        let mapping = TypedMapping {
            args: vec![],
            results: vec![],
        };
        assert_eq!(
            prev_state(&unowned_types, &mapping, ScopeState::new()),
            Ok(ScopeState::new())
        );
        assert_eq!(
            next_state(&unowned_types, &mapping, ScopeState::new()),
            Ok(ScopeState::new())
        );
    }

    #[test]
    fn basic_mapping() {
        let state = |types: Vec<&TypedVar>| {
            types
                .iter()
                .map(|v| (v.name.clone(), v.ty.clone()))
                .collect()
        };
        let types = |types: Vec<&TypedVar>| types.iter().map(|v| v.ty.clone()).collect();
        let typed = |var_name: &str, type_name: &str| TypedVar {
            name: var_name.to_string(),
            ty: Type::Basic(type_name.to_string()),
        };
        let arg = typed("arg", "Arg");
        let res = typed("res", "Res");
        let mapping = TypedMapping {
            args: vec![arg.clone()],
            results: vec![res.clone()],
        };
        assert_eq!(
            prev_state(&types(vec![]), &mapping, state(vec![&res])),
            Ok(state(vec![&arg]))
        );
        assert_eq!(
            next_state(&types(vec![]), &mapping, state(vec![&arg])),
            Ok(state(vec![&res]))
        );
        assert_eq!(
            next_state(&types(vec![&arg]), &mapping, state(vec![&arg])),
            Ok(state(vec![&arg, &res]))
        );
        assert_eq!(
            prev_state(&types(vec![&arg]), &mapping, state(vec![&res, &arg])),
            Ok(state(vec![&arg]))
        );
        assert_eq!(
            prev_state(&types(vec![]), &mapping, state(vec![&res, &arg])),
            Err(Error::UnconsumedOwnedType)
        );
        assert_eq!(
            prev_state(&types(vec![]), &mapping, state(vec![])),
            Err(Error::MissingReference)
        );
        assert_eq!(
            next_state(&types(vec![]), &mapping, state(vec![])),
            Err(Error::MissingReference)
        );
        assert_eq!(
            next_state(&types(vec![&arg]), &mapping, state(vec![])),
            Err(Error::MissingReference)
        );
        assert_eq!(
            prev_state(
                &types(vec![]),
                &mapping,
                state(vec![&typed("res", "ResWrong")])
            ),
            Err(Error::TypeMismatch)
        );
        let arg_wrong_type = typed("arg", "ArgWrong");
        assert_eq!(
            prev_state(&types(vec![]), &mapping, state(vec![&res, &arg_wrong_type])),
            Err(Error::TypeMismatch)
        );
        assert_eq!(
            next_state(&types(vec![]), &mapping, state(vec![&arg_wrong_type])),
            Err(Error::TypeMismatch)
        );
        assert_eq!(
            next_state(&types(vec![&arg]), &mapping, state(vec![&arg_wrong_type])),
            Err(Error::TypeMismatch)
        );
        assert_eq!(
            next_state(&types(vec![&arg]), &mapping, state(vec![&arg, &res])),
            Err(Error::VariableOverride)
        );
    }
}
