use crate::error::Error;
use crate::extensions::*;
use crate::graph::*;
use crate::scope_state::*;
use Result::*;

pub fn validate(f: &Function) -> Result<(), Error> {
    validate_helper(
        f,
        0,
        f.args
            .iter()
            .map(|v| (v.name.clone(), v.ty.clone()))
            .collect(),
        &mut vec![None; f.blocks.len()],
    )
}

fn validate_helper(
    f: &Function,
    b: usize,
    start_state: ScopeState,
    block_start_states: &mut Vec<Option<ScopeState>>,
) -> Result<(), Error> {
    if b >= block_start_states.len() {
        return Err(Error::FunctionBlockOutOfBounds);
    }
    match &block_start_states[b] {
        None => {
            block_start_states[b] = Some(start_state.clone());
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
    for invc in &f.blocks[b].invocations {
        let change = get_invoke_effects(invc)?;
        state = next_state(&change, state)?;
    }

    match &f.blocks[b].exit {
        BlockExit::Return(ref_ids) => {
            if ref_ids.len() != f.res_types.len() {
                return Err(Error::FunctionTypeMismatch);
            }
            if ref_ids.len() != state.len() {
                return Err(Error::FunctionRemainingOwnedObjects);
            }
            ref_ids
                .iter()
                .zip(f.res_types.iter())
                .try_for_each(|(n, ty)| match state.get(n) {
                    None => Err(Error::FunctionTypeMismatch),
                    Some(other) => {
                        if ty != other {
                            Err(Error::FunctionTypeMismatch)
                        } else {
                            Ok(())
                        }
                    }
                })
        }
        BlockExit::Continue => validate_helper(f, b + 1, state, block_start_states),
        BlockExit::Jump(j) => {
            let changes = get_jump_effects(j)?;
            changes.iter().try_for_each(|(next_block, change)| {
                let next_state = next_state(&change, state.clone())?;
                validate_helper(f, *next_block, next_state, block_start_states)
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
        Type::Basic("GasBuiltin".to_string())
    }

    fn felt_type() -> Type {
        Type::Basic("Felt".to_string())
    }

    fn int_type() -> Type {
        Type::Basic("Int".to_string())
    }

    fn gas_type(c: i64) -> Type {
        Type::Template("Gas".to_string(), vec![TemplateArg::Value(c)])
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
                            libcall: LibCall {
                                name: "split_gas".to_string(),
                                tmpl_args: vec![TemplateArg::Value(1), TemplateArg::Value(2)],
                            },
                            args: vec!["cost".to_string()],
                            results: vec!["cost_for_next".to_string(), "cost".to_string()],
                        },
                        Invocation {
                            libcall: LibCall {
                                name: "add".to_string(),
                                tmpl_args: vec![TemplateArg::Type(int_type())],
                            },
                            args: vec!["a".to_string(), "b".to_string(), "cost_for_next".to_string()],
                            results: vec!["a_plus_b".to_string()],
                        },
                        Invocation {
                            libcall: LibCall {
                                name: "split_gas".to_string(),
                                tmpl_args: vec![TemplateArg::Value(1), TemplateArg::Value(1)],
                            },
                            args: vec!["cost".to_string()],
                            results: vec!["cost_for_next".to_string(), "cost_for_last".to_string()],
                        },
                        Invocation {
                            libcall: LibCall {
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
                            libcall: LibCall {
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
                            libcall: LibCall {
                                name: "get_gas".to_string(),
                                tmpl_args: vec![TemplateArg::Value(1)]
                            },
                            args: vec!["gb".to_string(), "cost".to_string()],
                            branches: vec![
                                BranchInfo {
                                    block: 0,
                                    exports: vec!["gb".to_string(), "cost".to_string()]
                                },
                                BranchInfo {
                                    block: 1,
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
                            libcall: LibCall {
                                name: "get_gas".to_string(),
                                tmpl_args: vec![TemplateArg::Value(2)]
                            },
                            args: vec!["gb".to_string(), "cost".to_string()],
                            branches: vec![
                                BranchInfo {
                                    block: 0,
                                    exports: vec!["gb".to_string(), "cost".to_string()]
                                },
                                BranchInfo {
                                    block: 1,
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
