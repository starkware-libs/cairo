use crate::error::Error;
use crate::graph::Type;
use std::collections::HashMap;
use Result::*;

pub type ScopeState = HashMap<String, Type>;

pub struct ScopeChange<'a> {
    pub arg_names: &'a Vec<String>,
    pub arg_types: &'a Vec<Type>,
    pub res_names: &'a Vec<String>,
    pub res_types: &'a Vec<Type>,
}

pub fn next_state(mut state: ScopeState, change: ScopeChange<'_>) -> Result<ScopeState, Error> {
    if change.arg_names.len() != change.arg_types.len() {
        return Err(Error::ArgumentSizeMismatch);
    }
    if change.res_names.len() != change.res_types.len() {
        return Err(Error::ResultSizeMismatch);
    }
    for (name, ty) in change.arg_names.iter().zip(change.arg_types.iter()) {
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
    for (name, ty) in change.res_names.iter().zip(change.res_types.iter()) {
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

    #[test]
    fn empty() {
        assert_eq!(
            next_state(
                ScopeState::new(),
                ScopeChange {
                    arg_names: &vec![],
                    arg_types: &vec![],
                    res_names: &vec![],
                    res_types: &vec![],
                }
            ),
            Ok(ScopeState::new())
        );
    }

    #[test]
    fn basic_mapping() {
        let as_type = |name: &str| Type::Basic(name.to_string());
        assert_eq!(
            next_state(
                ScopeState::from([("arg".to_string(), as_type("Arg"))]),
                ScopeChange {
                    arg_names: &vec!["arg".to_string()],
                    arg_types: &vec![as_type("Arg")],
                    res_names: &vec!["res".to_string()],
                    res_types: &vec![as_type("Res")],
                }
            ),
            Ok(ScopeState::from([("res".to_string(), as_type("Res"))]))
        );
        assert_eq!(
            next_state(
                ScopeState::new(),
                ScopeChange {
                    arg_names: &vec!["arg".to_string()],
                    arg_types: &vec![as_type("Arg")],
                    res_names: &vec!["res".to_string()],
                    res_types: &vec![as_type("Res")],
                }
            ),
            Err(Error::MissingReference("arg".to_string(), as_type("Arg")))
        );
        assert_eq!(
            next_state(
                ScopeState::from([("arg".to_string(), as_type("ArgWrong"))]),
                ScopeChange {
                    arg_names: &vec!["arg".to_string()],
                    arg_types: &vec![as_type("Arg")],
                    res_names: &vec!["res".to_string()],
                    res_types: &vec![as_type("Res")],
                }
            ),
            Err(Error::TypeMismatch(
                "arg".to_string(),
                as_type("ArgWrong"),
                as_type("Arg")
            ))
        );
        assert_eq!(
            next_state(
                ScopeState::from([
                    ("arg".to_string(), as_type("Arg")),
                    ("res".to_string(), as_type("ResWrong"))
                ]),
                ScopeChange {
                    arg_names: &vec!["arg".to_string()],
                    arg_types: &vec![as_type("Arg")],
                    res_names: &vec!["res".to_string()],
                    res_types: &vec![as_type("Res")],
                }
            ),
            Err(Error::VariableOverride("res".to_string()))
        );
    }
}
