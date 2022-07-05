use crate::error::Error;
use crate::extensions::*;
use crate::graph::*;
use crate::scope_state::*;
use Result::*;

pub fn validate(f: &Function) -> Result<(), Error> {
    validate_helper(
        f,
        BlockId(0),
        f.args
            .iter()
            .map(|v| (v.name.clone(), v.ty.clone()))
            .collect(),
        &mut vec![None; f.blocks.len()],
    )
}

fn validate_helper(
    f: &Function,
    b: BlockId,
    start_state: ScopeState,
    block_start_states: &mut Vec<Option<ScopeState>>,
) -> Result<(), Error> {
    if b.0 >= block_start_states.len() {
        return Err(Error::FunctionBlockOutOfBounds);
    }
    match &block_start_states[b.0] {
        None => {
            block_start_states[b.0] = Some(start_state.clone());
        }
        Some(s) => {
            return if *s != start_state {
                Err(Error::FunctionBlockMismatch)
            } else {
                Ok(())
            };
        }
    }
    let mut state = start_state;
    for invc in &f.blocks[b.0].invocations {
        let (arg_types, res_types) = get_invoke_signature(&invc.ext)?;
        state = next_state(
            state,
            ScopeChange {
                arg_names: &invc.args,
                arg_types: &arg_types,
                res_names: &invc.results,
                res_types: &res_types,
            },
        )?;
    }

    match &f.blocks[b.0].exit {
        BlockExit::Return(ref_ids) => {
            state = next_state(
                state,
                ScopeChange {
                    arg_names: &ref_ids,
                    arg_types: &f.res_types,
                    res_names: &vec![],
                    res_types: &vec![],
                },
            )?;
            if state.is_empty() {
                Ok(())
            } else {
                return Err(Error::FunctionRemainingOwnedObjects);
            }
        }
        BlockExit::Continue => validate_helper(f, BlockId(b.0 + 1), state, block_start_states),
        BlockExit::Jump(j) => {
            let (arg_types, res_types_opts) = get_jump_signature(&j.ext)?;
            res_types_opts
                .iter()
                .zip(j.branches.iter())
                .try_for_each(|(res_types, branch)| {
                    let next_state = next_state(
                        state.clone(),
                        ScopeChange {
                            arg_names: &j.args,
                            arg_types: &arg_types,
                            res_names: &branch.exports,
                            res_types: &res_types,
                        },
                    )?;
                    validate_helper(f, branch.block, next_state, block_start_states)
                })
        }
    }
}

#[cfg(test)]
mod function {
    use super::*;

    fn typed(var_name: &str, ty: Type) -> TypedVar {
        TypedVar {
            name: var_name.to_string(),
            ty: ty,
        }
    }

    fn gas_builtin_type() -> Type {
        Type {
            name: "GasBuiltin".to_string(),
            args: vec![],
        }
    }

    fn felt_type() -> Type {
        Type {
            name: "Felt".to_string(),
            args: vec![],
        }
    }

    fn int_type() -> Type {
        Type {
            name: "int".to_string(),
            args: vec![],
        }
    }

    fn gas_type(c: i64) -> Type {
        Type {
            name: "Gas".to_string(),
            args: vec![TemplateArg::Value(c)],
        }
    }

    #[test]
    fn empty() {
        assert_eq!(
            validate(&Function {
                name: "Some".to_string(),
                args: vec![typed("gb", gas_builtin_type()), typed("a", felt_type())],
                res_types: vec![gas_builtin_type(), felt_type()],
                blocks: vec![],
            }),
            Err(Error::FunctionBlockOutOfBounds)
        );
    }

    #[test]
    fn basic_return() {
        assert_eq!(
            validate(&Function {
                name: "Other".to_string(),
                args: vec![
                    typed("a", int_type()),
                    typed("b", int_type()),
                    typed("c", int_type()),
                    typed("d", int_type()),
                    typed("cost", gas_type(3)),
                ],
                res_types: vec![int_type()],
                blocks: vec![Block {
                    invocations: vec![
                        Invocation {
                            ext: Extension {
                                name: "split_gas".to_string(),
                                tmpl_args: vec![TemplateArg::Value(1), TemplateArg::Value(2)],
                            },
                            args: vec!["cost".to_string()],
                            results: vec!["cost_for_next".to_string(), "cost".to_string()],
                        },
                        Invocation {
                            ext: Extension {
                                name: "add".to_string(),
                                tmpl_args: vec![TemplateArg::Type(int_type())],
                            },
                            args: vec![
                                "a".to_string(),
                                "b".to_string(),
                                "cost_for_next".to_string()
                            ],
                            results: vec!["a_plus_b".to_string()],
                        },
                        Invocation {
                            ext: Extension {
                                name: "split_gas".to_string(),
                                tmpl_args: vec![TemplateArg::Value(1), TemplateArg::Value(1)],
                            },
                            args: vec!["cost".to_string()],
                            results: vec!["cost_for_next".to_string(), "cost_for_last".to_string()],
                        },
                        Invocation {
                            ext: Extension {
                                name: "sub".to_string(),
                                tmpl_args: vec![TemplateArg::Type(int_type())],
                            },
                            args: vec![
                                "c".to_string(),
                                "d".to_string(),
                                "cost_for_next".to_string()
                            ],
                            results: vec!["c_minus_d".to_string()],
                        },
                        Invocation {
                            ext: Extension {
                                name: "mul".to_string(),
                                tmpl_args: vec![TemplateArg::Type(int_type())],
                            },
                            args: vec![
                                "a_plus_b".to_string(),
                                "c_minus_d".to_string(),
                                "cost_for_last".to_string()
                            ],
                            results: vec!["a_plus_b_mul_c_minus_d".to_string()],
                        }
                    ],
                    exit: BlockExit::Return(vec!["a_plus_b_mul_c_minus_d".to_string()]),
                },],
            }),
            Ok(())
        );
    }

    #[test]
    fn inifinite_gas_take_or_return() {
        assert_eq!(
            validate(&Function {
                name: "Some".to_string(),
                args: vec![typed("gb", gas_builtin_type()), typed("cost", gas_type(1)),],
                res_types: vec![gas_builtin_type()],
                blocks: vec![
                    Block {
                        invocations: vec![],
                        exit: BlockExit::Jump(JumpInfo {
                            ext: Extension {
                                name: "get_gas".to_string(),
                                tmpl_args: vec![TemplateArg::Value(1)]
                            },
                            args: vec!["gb".to_string(), "cost".to_string()],
                            branches: vec![
                                BranchInfo {
                                    block: BlockId(0),
                                    exports: vec!["gb".to_string(), "cost".to_string()]
                                },
                                BranchInfo {
                                    block: BlockId(1),
                                    exports: vec!["gb".to_string()]
                                }
                            ],
                        }),
                    },
                    Block {
                        invocations: vec![],
                        exit: BlockExit::Return(vec!["gb".to_string()]),
                    },
                ],
            }),
            Ok(())
        );
    }

    #[test]
    fn gas_mismatch() {
        assert_eq!(
            validate(&Function {
                name: "Some".to_string(),
                args: vec![typed("gb", gas_builtin_type()), typed("cost", gas_type(1)),],
                res_types: vec![gas_builtin_type()],
                blocks: vec![
                    Block {
                        invocations: vec![],
                        exit: BlockExit::Jump(JumpInfo {
                            ext: Extension {
                                name: "get_gas".to_string(),
                                tmpl_args: vec![TemplateArg::Value(2)]
                            },
                            args: vec!["gb".to_string(), "cost".to_string()],
                            branches: vec![
                                BranchInfo {
                                    block: BlockId(0),
                                    exports: vec!["gb".to_string(), "cost".to_string()]
                                },
                                BranchInfo {
                                    block: BlockId(1),
                                    exports: vec!["gb".to_string()]
                                }
                            ],
                        }),
                    },
                    Block {
                        invocations: vec![],
                        exit: BlockExit::Return(vec!["gb".to_string()]),
                    },
                ],
            }),
            Err(Error::FunctionBlockMismatch)
        );
    }
}
