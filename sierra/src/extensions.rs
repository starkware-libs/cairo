use crate::error::Error;
use crate::graph::*;
use crate::scope_state::*;
use std::collections::HashMap;
use Result::*;

pub fn get_invoke_effects(invc: &Invocation) -> Result<ScopeChange, Error> {
    match invc.libcall.name.as_str() {
        "add" | "sub" | "mul" | "div" => {
            if invc.libcall.tmpl_args.len() != 1 {
                return Err(Error::WrongNumberOfTypeArgs(invc.to_string()));
            }
            if invc.args.len() != 3 {
                return Err(Error::WrongNumberOfArgs(invc.to_string()));
            }
            if invc.results.len() != 1 {
                return Err(Error::WrongNumberOfResults(invc.to_string()));
            }
            let numeric_type = match &invc.libcall.tmpl_args[0] {
                TemplateArg::Type(t) => Ok(t),
                TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
            }?;
            Ok(ScopeChange {
                args: vec![
                    TypedVar {
                        name: invc.args[0].clone(),
                        ty: numeric_type.clone(),
                    },
                    TypedVar {
                        name: invc.args[1].clone(),
                        ty: numeric_type.clone(),
                    },
                    TypedVar {
                        name: invc.args[2].clone(),
                        ty: Type::Template("Gas".to_string(), vec![TemplateArg::Value(1)]),
                    },
                ],
                results: vec![TypedVar {
                    name: invc.results[0].clone(),
                    ty: numeric_type.clone(),
                }],
            })
        }
        _ => Err(Error::UnsupportedLibCallName),
    }
}

pub fn get_jump_effects(jump: &JumpInfo) -> Result<HashMap<usize, ScopeChange>, Error> {
    match jump.libcall.name.as_str() {
        "jump" => {
            if !jump.libcall.tmpl_args.is_empty() {
                return Err(Error::WrongNumberOfTypeArgs(jump.to_string()));
            }
            if jump.args.len() != 1 {
                return Err(Error::WrongNumberOfArgs(jump.to_string()));
            }
            if jump.branches.len() != 1 {
                return Err(Error::WrongNumberOfBranches(jump.to_string()));
            }
            if !jump.branches[0].exports.is_empty() {
                return Err(Error::WrongNumberOfResults(jump.to_string()));
            }
            Ok(HashMap::<usize, ScopeChange>::from([(
                jump.branches[0].block,
                ScopeChange {
                    args: vec![TypedVar {
                        name: jump.args[0].clone(),
                        ty: Type::Template("Gas".to_string(), vec![TemplateArg::Value(1)]),
                    }],
                    results: vec![],
                },
            )]))
        }
        "jump_nz" => {
            if jump.libcall.tmpl_args.len() != 1 {
                return Err(Error::WrongNumberOfTypeArgs(jump.to_string()));
            }
            if jump.args.len() != 2 {
                return Err(Error::WrongNumberOfArgs(jump.to_string()));
            }
            if jump.branches.len() != 2 {
                return Err(Error::WrongNumberOfBranches(jump.to_string()));
            }
            let success = &jump.branches[0];
            let failure = &jump.branches[1];
            if !success.exports.is_empty() {
                return Err(Error::WrongNumberOfResults(jump.to_string()));
            }
            if !failure.exports.is_empty() {
                return Err(Error::WrongNumberOfResults(jump.to_string()));
            }
            let numeric_type = match &jump.libcall.tmpl_args[0] {
                TemplateArg::Type(t) => Ok(t),
                TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
            }?;
            Ok(HashMap::<usize, ScopeChange>::from([
                (
                    success.block,
                    ScopeChange {
                        args: vec![
                            TypedVar {
                                name: jump.args[0].clone(),
                                ty: numeric_type.clone(),
                            },
                            TypedVar {
                                name: jump.args[1].clone(),
                                ty: Type::Template("Gas".to_string(), vec![TemplateArg::Value(1)]),
                            },
                        ],
                        results: vec![],
                    },
                ),
                (
                    failure.block,
                    ScopeChange {
                        args: vec![TypedVar {
                            name: jump.args[0].clone(),
                            ty: numeric_type.clone(),
                        }],
                        results: vec![],
                    },
                ),
            ]))
        }
        "get_gas" => {
            if jump.libcall.tmpl_args.is_empty() {
                return Err(Error::WrongNumberOfTypeArgs(jump.to_string()));
            }
            if jump.args.len() != 2 {
                return Err(Error::WrongNumberOfArgs(jump.to_string()));
            }
            if jump.branches.len() != 2 {
                return Err(Error::WrongNumberOfBranches(jump.to_string()));
            }
            let success = &jump.branches[0];
            let failure = &jump.branches[1];
            if success.exports.len() != 1 + jump.libcall.tmpl_args.len() {
                return Err(Error::WrongNumberOfResults(jump.to_string()));
            }
            if failure.exports.len() != 1 {
                return Err(Error::WrongNumberOfResults(jump.to_string()));
            }
            let gas_type = Type::Basic("GasBuiltin".to_string());
            let mut success_results = vec![TypedVar {
                name: success.exports[0].clone(),
                ty: gas_type.clone(),
            }];
            jump.libcall
                .tmpl_args
                .iter()
                .zip(success.exports.iter().skip(1))
                .try_for_each(|(tmpl_arg, name)| match tmpl_arg {
                    TemplateArg::Value(v) => {
                        success_results.push(TypedVar {
                            name: name.to_string(),
                            ty: Type::Template("Gas".to_string(), vec![TemplateArg::Value(*v)]),
                        });
                        Ok(())
                    }
                    TemplateArg::Type(_) => Err(Error::UnsupportedTypeArg),
                })?;
            Ok(HashMap::<usize, ScopeChange>::from([
                (
                    success.block,
                    ScopeChange {
                        args: vec![
                            TypedVar {
                                name: jump.args[0].clone(),
                                ty: gas_type.clone(),
                            },
                            TypedVar {
                                name: jump.args[1].clone(),
                                ty: Type::Template("Gas".to_string(), vec![TemplateArg::Value(1)]),
                            },
                        ],
                        results: success_results,
                    },
                ),
                (
                    failure.block,
                    ScopeChange {
                        args: vec![
                            TypedVar {
                                name: jump.args[0].clone(),
                                ty: gas_type.clone(),
                            },
                            TypedVar {
                                name: jump.args[1].clone(),
                                ty: Type::Template("Gas".to_string(), vec![TemplateArg::Value(1)]),
                            },
                        ],
                        results: vec![TypedVar {
                            name: failure.exports[0].clone(),
                            ty: gas_type.clone(),
                        }],
                    },
                ),
            ]))
        }
        "match_nullable" => {
            if jump.libcall.tmpl_args.len() != 1 {
                return Err(Error::WrongNumberOfTypeArgs(jump.to_string()));
            }
            if jump.args.len() != 2 {
                return Err(Error::WrongNumberOfArgs(jump.to_string()));
            }
            if jump.branches.len() != 2 {
                return Err(Error::WrongNumberOfBranches(jump.to_string()));
            }
            let success = &jump.branches[0];
            let failure = &jump.branches[1];
            if success.exports.len() == 1 {
                return Err(Error::WrongNumberOfResults(jump.to_string()));
            }
            if !failure.exports.is_empty() {
                return Err(Error::WrongNumberOfResults(jump.to_string()));
            }
            let inner_type = match &jump.libcall.tmpl_args[0] {
                TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
                TemplateArg::Type(t) => Ok(t),
            }?;
            Ok(HashMap::<usize, ScopeChange>::from([
                (
                    success.block,
                    ScopeChange {
                        args: vec![
                            TypedVar {
                                name: jump.args[0].clone(),
                                ty: Type::Template(
                                    "Nullable".to_string(),
                                    vec![TemplateArg::Type(inner_type.clone())],
                                ),
                            },
                            TypedVar {
                                name: jump.args[1].clone(),
                                ty: Type::Template("Gas".to_string(), vec![TemplateArg::Value(1)]),
                            },
                        ],
                        results: vec![TypedVar {
                            name: success.exports[0].clone(),
                            ty: inner_type.clone(),
                        }],
                    },
                ),
                (
                    failure.block,
                    ScopeChange {
                        args: vec![TypedVar {
                            name: jump.args[0].clone(),
                            ty: Type::Template(
                                "Nullable".to_string(),
                                vec![TemplateArg::Type(inner_type.clone())],
                            ),
                        }],
                        results: vec![],
                    },
                ),
            ]))
        }
        _ => Err(Error::UnsupportedLibCallName),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_add_mapping() {
        let typed = |var_name: &str, type_name: &str| TypedVar {
            name: var_name.to_string(),
            ty: Type::Basic(type_name.to_string()),
        };
        let a = typed("a", "int");
        let b = typed("b", "int");
        let cost = TypedVar {
            name: "cost".to_string(),
            ty: Type::Template("Gas".to_string(), vec![TemplateArg::Value(1)]),
        };
        let c = typed("c", "int");
        assert_eq!(
            get_invoke_effects(&Invocation {
                libcall: LibCall {
                    name: "add".to_string(),
                    tmpl_args: vec![TemplateArg::Type(a.ty.clone())]
                },
                args: vec![a.name.clone(), b.name.clone(), cost.name.clone()],
                results: vec![c.name.clone()],
            }),
            Ok(ScopeChange {
                args: vec![a, b, cost],
                results: vec![c],
            })
        );
    }

    #[test]
    fn get_jump_mapping() {
        let cost = TypedVar {
            name: "cost".to_string(),
            ty: Type::Template("Gas".to_string(), vec![TemplateArg::Value(1)]),
        };
        assert_eq!(
            get_jump_effects(&JumpInfo {
                libcall: LibCall {
                    name: "jump".to_string(),
                    tmpl_args: vec![]
                },
                args: vec![cost.name.clone()],
                branches: vec![BranchInfo {
                    block: 1,
                    exports: vec![]
                }],
            }),
            Ok(HashMap::<usize, ScopeChange>::from([(
                1,
                ScopeChange {
                    args: vec![cost],
                    results: vec![],
                },
            )]))
        );
    }
}
