use crate::error::Error;
use crate::graph::*;
use std::collections::HashMap;
use Result::*;

pub type ScopeState = HashMap<String, Type>;

#[derive(Clone, Debug, PartialEq)]
pub struct ScopeChange {
    pub args: Vec<TypedVar>,
    pub results: Vec<TypedVar>,
}

pub fn next_state(change: &ScopeChange, mut state: ScopeState) -> Result<ScopeState, Error> {
    for arg in &change.args {
        match state.remove(&arg.name) {
            None => {
                return Err(Error::MissingReference);
            }
            Some(ty) => {
                if ty != arg.ty {
                    return Err(Error::TypeMismatch);
                }
            }
        }
    }
    for res in &change.results {
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
        let change = ScopeChange {
            args: vec![],
            results: vec![],
        };
        assert_eq!(
            next_state(&change, ScopeState::new()),
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
        let typed = |var_name: &str, type_name: &str| TypedVar {
            name: var_name.to_string(),
            ty: Type::Basic(type_name.to_string()),
        };
        let arg = typed("arg", "Arg");
        let res = typed("res", "Res");
        let change = ScopeChange {
            args: vec![arg.clone()],
            results: vec![res.clone()],
        };
        assert_eq!(
            next_state(&change, state(vec![&arg])),
            Ok(state(vec![&res]))
        );
        assert_eq!(
            next_state(&change, state(vec![])),
            Err(Error::MissingReference)
        );
        assert_eq!(
            next_state(&change, state(vec![&typed("arg", "ArgWrong")])),
            Err(Error::TypeMismatch)
        );
        assert_eq!(
            next_state(&change, state(vec![&arg, &res])),
            Err(Error::VariableOverride)
        );
    }
}
