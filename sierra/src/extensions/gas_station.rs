use crate::extensions::*;

struct GetGasExtension {}

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
        let gas_type = Type::Basic("GasBuiltin".to_string());
        let args = vec![
            TypedVar {
                name: jump.args[0].clone(),
                ty: gas_type.clone(),
            },
            TypedVar {
                name: jump.args[1].clone(),
                ty: Type::Template("Gas".to_string(), vec![TemplateArg::Value(1)]),
            },
        ];
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
                        ty: gas_type.clone(),
                    }],
                },
            ),
        ]))
    }
}

pub(super) fn register(registry: &mut ExtensionRegistry) {
    registry
        .jump_libcalls
        .insert("get_gas".to_string(), Box::new(GetGasExtension {}));
}

#[test]
fn mapping() {
    let gb = TypedVar {
        name: "gb".to_string(),
        ty: Type::Basic("GasBuiltin".to_string()),
    };
    let cost = TypedVar {
        name: "cost".to_string(),
        ty: Type::Template("Gas".to_string(), vec![TemplateArg::Value(1)]),
    };
    let new_cost = TypedVar {
        name: "new_cost".to_string(),
        ty: Type::Template("Gas".to_string(), vec![TemplateArg::Value(6)]),
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
