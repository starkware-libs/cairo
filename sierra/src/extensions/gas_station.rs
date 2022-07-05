use crate::extensions::*;

struct GetGasExtension {}

fn gas_builtin_type() -> Type {
    Type::Basic("GasBuiltin".to_string())
}

fn gas_type(v: i64) -> Type {
    Type::Template("Gas".to_string(), vec![TemplateArg::Value(v)])
}

impl JumpExtension for GetGasExtension {
    fn get_effects(self: &Self, jump: &JumpInfo) -> Result<HashMap<usize, ScopeChange>, Error> {
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
        let args = vec![
            TypedVar {
                name: jump.args[0].clone(),
                ty: gas_builtin_type(),
            },
            TypedVar {
                name: jump.args[1].clone(),
                ty: gas_type(1),
            },
        ];
        let mut success_results = vec![TypedVar {
            name: success.exports[0].clone(),
            ty: gas_builtin_type(),
        }];
        jump.libcall
            .tmpl_args
            .iter()
            .zip(success.exports.iter().skip(1))
            .try_for_each(|(tmpl_arg, name)| match tmpl_arg {
                TemplateArg::Value(v) => {
                    success_results.push(TypedVar {
                        name: name.to_string(),
                        ty: gas_type(*v),
                    });
                    Ok(())
                }
                TemplateArg::Type(_) => Err(Error::UnsupportedTypeArg),
            })?;
        Ok(HashMap::<usize, ScopeChange>::from([
            (
                success.block,
                ScopeChange {
                    args: args.clone(),
                    results: success_results,
                },
            ),
            (
                failure.block,
                ScopeChange {
                    args: args,
                    results: vec![TypedVar {
                        name: failure.exports[0].clone(),
                        ty: gas_builtin_type(),
                    }],
                },
            ),
        ]))
    }
}

struct SplitGasExtension {}

impl InvokeExtension for SplitGasExtension {
    fn get_effects(self: &Self, invc: &Invocation) -> Result<ScopeChange, Error> {
        if invc.libcall.tmpl_args.len() <= 1 {
            return Err(Error::WrongNumberOfTypeArgs(invc.to_string()));
        }
        if invc.args.len() != 1 {
            return Err(Error::WrongNumberOfArgs(invc.to_string()));
        }
        if invc.results.len() != invc.libcall.tmpl_args.len() {
            return Err(Error::WrongNumberOfResults(invc.to_string()));
        }
        let mut results = vec![];
        let mut total = 0;
        invc.libcall
            .tmpl_args
            .iter()
            .zip(invc.results.iter())
            .try_for_each(|(tmpl_arg, name)| match tmpl_arg {
                TemplateArg::Value(v) => {
                    results.push(TypedVar {
                        name: name.to_string(),
                        ty: gas_type(*v),
                    });
                    total += v;
                    Ok(())
                }
                TemplateArg::Type(_) => Err(Error::UnsupportedTypeArg),
            })?;
        Ok(ScopeChange {
            args: vec![TypedVar {
                name: invc.args[0].clone(),
                ty: gas_type(total),
            }],
            results: results,
        })
    }
}

pub(super) fn register(registry: &mut ExtensionRegistry) {
    registry
        .jump_libcalls
        .insert("get_gas".to_string(), Box::new(GetGasExtension {}));
    registry
        .invoke_libcalls
        .insert("split_gas".to_string(), Box::new(SplitGasExtension {}));
}

#[test]
fn get_gas_mapping() {
    let gb = TypedVar {
        name: "gb".to_string(),
        ty: gas_builtin_type(),
    };
    let cost = TypedVar {
        name: "cost".to_string(),
        ty: gas_type(1),
    };
    let new_cost = TypedVar {
        name: "new_cost".to_string(),
        ty: gas_type(6),
    };
    assert_eq!(
        GetGasExtension {}.get_effects(&JumpInfo {
            libcall: LibCall {
                name: "".to_string(),
                tmpl_args: vec![TemplateArg::Value(6)]
            },
            args: vec![gb.name.clone(), cost.name.clone()],
            branches: vec![
                BranchInfo {
                    block: 0,
                    exports: vec![gb.name.clone(), new_cost.name.clone()]
                },
                BranchInfo {
                    block: 1,
                    exports: vec![gb.name.clone()]
                }
            ],
        }),
        Ok(HashMap::<usize, ScopeChange>::from([
            (
                0,
                ScopeChange {
                    args: vec![gb.clone(), cost.clone()],
                    results: vec![gb.clone(), new_cost.clone()],
                },
            ),
            (
                1,
                ScopeChange {
                    args: vec![gb.clone(), cost],
                    results: vec![gb],
                },
            )
        ]))
    );
}

#[test]
fn split_gas_mapping() {
    let c100 = TypedVar {
        name: "c100".to_string(),
        ty: gas_type(100),
    };
    let c12 = TypedVar {
        name: "c12".to_string(),
        ty: gas_type(12),
    };
    let c38 = TypedVar {
        name: "c38".to_string(),
        ty: gas_type(38),
    };
    let c50 = TypedVar {
        name: "c50".to_string(),
        ty: gas_type(50),
    };
    assert_eq!(
        SplitGasExtension {}.get_effects(&Invocation {
            libcall: LibCall {
                name: "".to_string(),
                tmpl_args: vec![
                    TemplateArg::Value(12),
                    TemplateArg::Value(38),
                    TemplateArg::Value(50)
                ]
            },
            args: vec![c100.name.clone()],
            results: vec![c12.name.clone(), c38.name.clone(), c50.name.clone()],
        }),
        Ok(ScopeChange {
            args: vec![c100],
            results: vec![c12, c38, c50],
        },)
    );
}
