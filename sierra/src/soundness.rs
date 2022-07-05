use crate::error::Error;
use crate::extensions::*;
use crate::graph::*;
use crate::scope_state::*;
use Result::*;

pub fn validate(prog: &Program) -> Result<(), Error> {
    let mut block_start_states = vec![None; prog.blocks.len()];
    prog.funcs.iter().try_for_each(|f| {
        validate_helper(
            &prog.blocks,
            &f.res_types,
            f.entry,
            f.args
                .iter()
                .map(|v| (v.id.clone(), v.ty.clone()))
                .collect(),
            &mut block_start_states,
        )
    })
}

fn validate_helper(
    blocks: &Vec<Block>,
    res_types: &Vec<Type>,
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
    for invc in &blocks[b.0].invocations {
        let (arg_types, res_types) = get_invoke_signature(&invc.ext)?;
        state = next_state(
            state,
            ScopeChange {
                arg_ids: &invc.args,
                arg_types: &arg_types,
                res_ids: &invc.results,
                res_types: &res_types,
            },
        )?;
    }

    match &blocks[b.0].exit {
        BlockExit::Return(ref_ids) => {
            state = next_state(
                state,
                ScopeChange {
                    arg_ids: &ref_ids,
                    arg_types: &res_types,
                    res_ids: &vec![],
                    res_types: &vec![],
                },
            )?;
            if state.is_empty() {
                Ok(())
            } else {
                return Err(Error::FunctionRemainingOwnedObjects);
            }
        }
        BlockExit::Continue => validate_helper(
            blocks,
            res_types,
            BlockId(b.0 + 1),
            state,
            block_start_states,
        ),
        BlockExit::Jump(j) => {
            let (arg_types, res_types_opts) = get_jump_signature(&j.ext)?;
            res_types_opts
                .iter()
                .zip(j.branches.iter())
                .try_for_each(|(res_types, branch)| {
                    let next_state = next_state(
                        state.clone(),
                        ScopeChange {
                            arg_ids: &j.args,
                            arg_types: &arg_types,
                            res_ids: &branch.exports,
                            res_types: &res_types,
                        },
                    )?;
                    validate_helper(
                        blocks,
                        res_types,
                        branch.block,
                        next_state,
                        block_start_states,
                    )
                })
        }
    }
}

#[cfg(test)]
mod function {
    use super::*;

    fn as_id(name: &str) -> Identifier {
        Identifier(name.to_string())
    }

    fn typed(id: Identifier, ty: Type) -> TypedVar {
        TypedVar { id: id, ty: ty }
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
            validate(&Program {
                blocks: vec![],
                funcs: vec![Function {
                    name: "Some".to_string(),
                    args: vec![
                        typed(as_id("gb"), gas_builtin_type()),
                        typed(as_id("a"), felt_type())
                    ],
                    res_types: vec![gas_builtin_type(), felt_type()],
                    entry: BlockId(0),
                }]
            }),
            Err(Error::FunctionBlockOutOfBounds)
        );
    }

    #[test]
    fn basic_return() {
        assert_eq!(
            validate(&Program {
                blocks: vec![Block {
                    invocations: vec![
                        Invocation {
                            ext: Extension {
                                name: "split_gas".to_string(),
                                tmpl_args: vec![TemplateArg::Value(1), TemplateArg::Value(2)],
                            },
                            args: vec![as_id("cost")],
                            results: vec![as_id("cost_for_next"), as_id("cost")],
                        },
                        Invocation {
                            ext: Extension {
                                name: "add".to_string(),
                                tmpl_args: vec![TemplateArg::Type(int_type())],
                            },
                            args: vec![as_id("a"), as_id("b"), as_id("cost_for_next")],
                            results: vec![as_id("a_plus_b")],
                        },
                        Invocation {
                            ext: Extension {
                                name: "split_gas".to_string(),
                                tmpl_args: vec![TemplateArg::Value(1), TemplateArg::Value(1)],
                            },
                            args: vec![as_id("cost")],
                            results: vec![as_id("cost_for_next"), as_id("cost_for_last")],
                        },
                        Invocation {
                            ext: Extension {
                                name: "sub".to_string(),
                                tmpl_args: vec![TemplateArg::Type(int_type())],
                            },
                            args: vec![as_id("c"), as_id("d"), as_id("cost_for_next")],
                            results: vec![as_id("c_minus_d")],
                        },
                        Invocation {
                            ext: Extension {
                                name: "mul".to_string(),
                                tmpl_args: vec![TemplateArg::Type(int_type())],
                            },
                            args: vec![
                                as_id("a_plus_b"),
                                as_id("c_minus_d"),
                                as_id("cost_for_last")
                            ],
                            results: vec![as_id("a_plus_b_mul_c_minus_d")],
                        }
                    ],
                    exit: BlockExit::Return(vec![as_id("a_plus_b_mul_c_minus_d")]),
                },],

                funcs: vec![Function {
                    name: "Other".to_string(),
                    args: vec![
                        typed(as_id("a"), int_type()),
                        typed(as_id("b"), int_type()),
                        typed(as_id("c"), int_type()),
                        typed(as_id("d"), int_type()),
                        typed(as_id("cost"), gas_type(3)),
                    ],
                    res_types: vec![int_type()],
                    entry: BlockId(0),
                }]
            }),
            Ok(())
        );
    }

    #[test]
    fn inifinite_gas_take_or_return() {
        assert_eq!(
            validate(&Program {
                blocks: vec![
                    Block {
                        invocations: vec![],
                        exit: BlockExit::Jump(JumpInfo {
                            ext: Extension {
                                name: "get_gas".to_string(),
                                tmpl_args: vec![TemplateArg::Value(1)]
                            },
                            args: vec![as_id("gb"), as_id("cost")],
                            branches: vec![
                                BranchInfo {
                                    block: BlockId(0),
                                    exports: vec![as_id("gb"), as_id("cost")]
                                },
                                BranchInfo {
                                    block: BlockId(1),
                                    exports: vec![as_id("gb")]
                                }
                            ],
                        }),
                    },
                    Block {
                        invocations: vec![],
                        exit: BlockExit::Return(vec![as_id("gb")]),
                    },
                ],
                funcs: vec![Function {
                    name: "Some".to_string(),
                    args: vec![
                        typed(as_id("gb"), gas_builtin_type()),
                        typed(as_id("cost"), gas_type(1)),
                    ],
                    res_types: vec![gas_builtin_type()],
                    entry: BlockId(0),
                }]
            }),
            Ok(())
        );
    }

    #[test]
    fn gas_mismatch() {
        assert_eq!(
            validate(&Program {
                blocks: vec![
                    Block {
                        invocations: vec![],
                        exit: BlockExit::Jump(JumpInfo {
                            ext: Extension {
                                name: "get_gas".to_string(),
                                tmpl_args: vec![TemplateArg::Value(2)]
                            },
                            args: vec![as_id("gb"), as_id("cost")],
                            branches: vec![
                                BranchInfo {
                                    block: BlockId(0),
                                    exports: vec![as_id("gb"), as_id("cost")]
                                },
                                BranchInfo {
                                    block: BlockId(1),
                                    exports: vec![as_id("gb")]
                                }
                            ],
                        }),
                    },
                    Block {
                        invocations: vec![],
                        exit: BlockExit::Return(vec![as_id("gb")]),
                    },
                ],
                funcs: vec![Function {
                    name: "Some".to_string(),
                    args: vec![
                        typed(as_id("gb"), gas_builtin_type()),
                        typed(as_id("cost"), gas_type(1)),
                    ],
                    res_types: vec![gas_builtin_type()],
                    entry: BlockId(0),
                }]
            }),
            Err(Error::FunctionBlockMismatch)
        );
    }
}
