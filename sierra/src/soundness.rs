use crate::{error::Error, extensions::*, graph::*, mem_state::*, next_state::next_state};
use std::collections::HashMap;
use Result::*;

pub fn validate(prog: &Program) -> Result<(), Error> {
    let mut block_start_states = vec![None; prog.blocks.len()];
    let ext_registry = get_ext_registry(prog);
    let type_registry = get_type_registry();
    prog.funcs.iter().try_for_each(|f| {
        //let mut last = -2;
        let mut vars = VarStates::new();
        f.args.iter().try_for_each(|v| {
            //let ti = get_info(&type_registry, &v.ty)?;
            //last -= ti.size as i64;
            //vars.insert(v.id.clone(), VarState { ty:v.ty, loc:Location::Local(last)});
            vars.insert(v.id.clone(), v.ty.clone());
            Ok(())
        })?;
        Helper {
            ext_registry: &ext_registry,
            type_registry: &type_registry,
            blocks: &prog.blocks,
            res_types: &f.res_types,
        }
        .validate(
            f.entry,
            State {
                vars: vars,
                mem: MemState {
                    temp_offset: 0,
                    local_offset: 0,
                    ap_change: ApChange::Known(0),
                },
            },
            &mut block_start_states,
        )
    })
}

type VarState = Type;

//#[derive(Debug, Clone, PartialEq)]
//struct VarState {
//    ty: Type,
//    loc: Location,
//}

type VarStates = HashMap<Identifier, VarState>;

#[derive(Debug, Clone, PartialEq)]
struct State {
    vars: VarStates,
    mem: MemState,
}

struct Helper<'a> {
    pub ext_registry: &'a ExtensionRegistry,
    pub type_registry: &'a TypeRegistry,
    pub blocks: &'a Vec<Block>,
    pub res_types: &'a Vec<Type>,
}

impl Helper<'_> {
    fn validate(
        self: &Self,
        block: BlockId,
        start_state: State,
        block_start_states: &mut Vec<Option<State>>,
    ) -> Result<(), Error> {
        if block.0 >= block_start_states.len() {
            return Err(Error::FunctionBlockOutOfBounds);
        }
        match &block_start_states[block.0] {
            None => {
                block_start_states[block.0] = Some(start_state.clone());
            }
            Some(s) => {
                if s.vars.len() != start_state.vars.len() {
                    return Err(Error::FunctionBlockIdentifiersMismatch(
                        block,
                        s.vars.keys().map(|x| x.clone()).collect(),
                        start_state.vars.keys().map(|x| x.clone()).collect(),
                    ));
                }
                for (id, expected_var_state) in &s.vars {
                    match start_state.vars.get(id) {
                        None => Err(Error::FunctionBlockIdentifiersMismatch(
                            block,
                            s.vars.keys().map(|x| x.clone()).collect(),
                            start_state.vars.keys().map(|x| x.clone()).collect(),
                        )),
                        Some(var_state) => {
                            if var_state != expected_var_state {
                                Err(Error::FunctionBlockIdentifierTypeMismatch(
                                    block,
                                    id.clone(),
                                    expected_var_state.clone(),
                                    var_state.clone(),
                                ))
                            } else {
                                Ok(())
                            }
                        }
                    }?;
                }
            }
        }
        let State { mut vars, mem } = start_state;
        for invc in &self.blocks[block.0].invocations {
            let sign = get_signature(self.ext_registry, &invc.ext)?;
            if sign.results.len() != 1 {
                return Err(Error::FunctionInvocationMismatch(invc.to_string()));
            }
            match sign.fallthrough {
                Some(0) => {}
                _ => {
                    return Err(Error::FunctionInvocationMismatch(invc.to_string()));
                }
            }
            vars = next_state(
                vars,
                invc.args.iter().zip(sign.args.iter()),
                invc.results.iter().zip(sign.results[0].iter()),
            )?;
        }

        match &self.blocks[block.0].exit {
            BlockExit::Return(ref_ids) => {
                vars = next_state(
                    vars,
                    ref_ids.iter().zip(self.res_types.iter()),
                    vec![].into_iter(),
                )?;
                if vars.is_empty() {
                    Ok(())
                } else {
                    return Err(Error::FunctionRemainingOwnedObjects(
                        vars.into_keys().collect(),
                    ));
                }
            }
            BlockExit::Jump(j) => {
                let sign = get_signature(self.ext_registry, &j.ext)?;
                if sign.results.len() != j.branches.len() {
                    return Err(Error::FunctionJumpMismatch(j.to_string()));
                }
                match sign.fallthrough {
                    None => {}
                    Some(i) => {
                        if j.branches[i].target != BranchTarget::Fallthrough {
                            return Err(Error::FunctionJumpMismatch(j.to_string()));
                        }
                    }
                }
                sign.results
                    .iter()
                    .zip(j.branches.iter())
                    .try_for_each(|(res_types, branch)| {
                        let next_vars = next_state(
                            vars.clone(),
                            j.args.iter().zip(sign.args.iter()),
                            branch.exports.iter().zip(res_types.iter()),
                        )?;
                        self.validate(
                            match branch.target {
                                BranchTarget::Fallthrough => BlockId(block.0 + 1),
                                BranchTarget::Block(b) => b,
                            },
                            State {
                                vars: next_vars,
                                mem: mem.clone(),
                            },
                            block_start_states,
                        )
                    })
            }
        }
    }
}

#[cfg(test)]
mod function {
    use super::*;
    use crate::utils::{as_type, gas_builtin_type, gas_type, type_arg, val_arg};

    fn as_id(name: &str) -> Identifier {
        Identifier(name.to_string())
    }

    fn typed(id: Identifier, ty: Type) -> TypedVar {
        TypedVar { id: id, ty: ty }
    }

    fn int_type() -> Type {
        as_type("int")
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
                        typed(as_id("a"), as_type("felt"))
                    ],
                    res_types: vec![gas_builtin_type(), as_type("felt")],
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
                                tmpl_args: vec![val_arg(1), val_arg(2)],
                            },
                            args: vec![as_id("cost")],
                            results: vec![as_id("cost_for_next"), as_id("cost")],
                        },
                        Invocation {
                            ext: Extension {
                                name: "add".to_string(),
                                tmpl_args: vec![type_arg(int_type())],
                            },
                            args: vec![as_id("a"), as_id("b")],
                            results: vec![as_id("a_plus_b_deferred")],
                        },
                        Invocation {
                            ext: Extension {
                                name: "enact_calc".to_string(),
                                tmpl_args: vec![type_arg(int_type()), val_arg(0)],
                            },
                            args: vec![as_id("a_plus_b_deferred"), as_id("cost_for_next")],
                            results: vec![as_id("a_plus_b")],
                        },
                        Invocation {
                            ext: Extension {
                                name: "split_gas".to_string(),
                                tmpl_args: vec![val_arg(1), val_arg(1)],
                            },
                            args: vec![as_id("cost")],
                            results: vec![as_id("cost_for_next"), as_id("cost_for_last")],
                        },
                        Invocation {
                            ext: Extension {
                                name: "sub".to_string(),
                                tmpl_args: vec![type_arg(int_type())],
                            },
                            args: vec![as_id("c"), as_id("d")],
                            results: vec![as_id("c_minus_d_deferred")],
                        },
                        Invocation {
                            ext: Extension {
                                name: "enact_calc".to_string(),
                                tmpl_args: vec![type_arg(int_type()), val_arg(1)],
                            },
                            args: vec![as_id("c_minus_d_deferred"), as_id("cost_for_next")],
                            results: vec![as_id("c_minus_d")],
                        },
                        Invocation {
                            ext: Extension {
                                name: "mul".to_string(),
                                tmpl_args: vec![type_arg(int_type())],
                            },
                            args: vec![as_id("a_plus_b"), as_id("c_minus_d"),],
                            results: vec![as_id("a_plus_b_mul_c_minus_d_deferred")],
                        },
                        Invocation {
                            ext: Extension {
                                name: "enact_calc".to_string(),
                                tmpl_args: vec![type_arg(int_type()), val_arg(0)],
                            },
                            args: vec![
                                as_id("a_plus_b_mul_c_minus_d_deferred"),
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
                                tmpl_args: vec![val_arg(1)]
                            },
                            args: vec![as_id("gb"), as_id("cost")],
                            branches: vec![
                                BranchInfo {
                                    target: BranchTarget::Block(BlockId(0)),
                                    exports: vec![as_id("gb"), as_id("cost")]
                                },
                                BranchInfo {
                                    target: BranchTarget::Fallthrough,
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
    fn inifinite_gas_take_or_return_bad_fallthrough() {
        let ji = JumpInfo {
            ext: Extension {
                name: "get_gas".to_string(),
                tmpl_args: vec![val_arg(1)],
            },
            args: vec![as_id("gb"), as_id("cost")],
            branches: vec![
                BranchInfo {
                    target: BranchTarget::Fallthrough,
                    exports: vec![as_id("gb"), as_id("cost")],
                },
                BranchInfo {
                    target: BranchTarget::Block(BlockId(0)),
                    exports: vec![as_id("gb")],
                },
            ],
        };
        assert_eq!(
            validate(&Program {
                blocks: vec![
                    Block {
                        invocations: vec![],
                        exit: BlockExit::Return(vec![as_id("gb")]),
                    },
                    Block {
                        invocations: vec![],
                        exit: BlockExit::Jump(ji.clone()),
                    },
                ],
                funcs: vec![Function {
                    name: "Some".to_string(),
                    args: vec![
                        typed(as_id("gb"), gas_builtin_type()),
                        typed(as_id("cost"), gas_type(1)),
                    ],
                    res_types: vec![gas_builtin_type()],
                    entry: BlockId(1),
                }]
            }),
            Err(Error::FunctionJumpMismatch(ji.to_string()))
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
                                tmpl_args: vec![val_arg(2)]
                            },
                            args: vec![as_id("gb"), as_id("cost")],
                            branches: vec![
                                BranchInfo {
                                    target: BranchTarget::Block(BlockId(0)),
                                    exports: vec![as_id("gb"), as_id("cost")]
                                },
                                BranchInfo {
                                    target: BranchTarget::Fallthrough,
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
            Err(Error::FunctionBlockIdentifierTypeMismatch(
                BlockId(0),
                as_id("cost"),
                gas_type(1),
                gas_type(2)
            ))
        );
    }

    #[test]
    fn fibonacci_using_jump() {
        let dup = |old, new1, new2| Invocation {
            ext: Extension {
                name: "duplicate_num".to_string(),
                tmpl_args: vec![type_arg(int_type())],
            },
            args: vec![as_id(old)],
            results: vec![as_id(new1), as_id(new2)],
        };
        let gas_use = |curr, taken| Invocation {
            ext: Extension {
                name: "split_gas".to_string(),
                tmpl_args: vec![val_arg(taken), val_arg(curr - taken)],
            },
            args: vec![as_id("cost")],
            results: vec![as_id("use_cost"), as_id("cost")],
        };
        let refund = |count| Invocation {
            ext: Extension {
                name: "refund_gas".to_string(),
                tmpl_args: vec![val_arg(count)],
            },
            args: vec![as_id("gb"), as_id("cost")],
            results: vec![as_id("gb")],
        };
        let ignore = |name| Invocation {
            ext: Extension {
                name: "ignore_num".to_string(),
                tmpl_args: vec![type_arg(int_type())],
            },
            args: vec![as_id(name)],
            results: vec![],
        };
        let as_exit = |invc: Invocation| {
            BlockExit::Jump(JumpInfo {
                ext: invc.ext,
                args: invc.args,
                branches: vec![BranchInfo {
                    target: BranchTarget::Fallthrough,
                    exports: invc.results,
                }],
            })
        };

        let test_n = |b_success| {
            BlockExit::Jump(JumpInfo {
                ext: Extension {
                    name: "jump_nz".to_string(),
                    tmpl_args: vec![type_arg(int_type())],
                },
                args: vec![as_id("use"), as_id("use_cost")],
                branches: vec![
                    BranchInfo {
                        target: BranchTarget::Block(BlockId(b_success)),
                        exports: vec![],
                    },
                    BranchInfo {
                        target: BranchTarget::Fallthrough,
                        exports: vec![],
                    },
                ],
            })
        };

        let dec_n = Invocation {
            ext: Extension {
                name: "add".to_string(),
                tmpl_args: vec![type_arg(int_type()), val_arg(-1)],
            },
            args: vec![as_id("n")],
            results: vec![as_id("n")],
        };

        let enact = |name| Invocation {
            ext: Extension {
                name: "enact_calc".to_string(),
                tmpl_args: vec![type_arg(int_type()), val_arg(0)],
            },
            args: vec![as_id(name), as_id("use_cost")],
            results: vec![as_id(name)],
        };

        assert_eq!(
            validate(&Program {
                blocks: vec![
                    Block {
                        // 0
                        invocations: vec![
                            Invocation {
                                ext: Extension {
                                    name: "constant_num".to_string(),
                                    tmpl_args: vec![type_arg(int_type()), val_arg(1)],
                                },
                                args: vec![],
                                results: vec![as_id("one")],
                            },
                            gas_use(6, 1),
                            enact("one"),
                            dup("n", "n", "use"),
                            gas_use(5, 1),
                        ],
                        exit: test_n(2),
                    },
                    Block {
                        // 1
                        invocations: vec![refund(4), ignore("n")],
                        exit: BlockExit::Return(vec![as_id("gb"), as_id("one")]),
                    },
                    Block {
                        // 2
                        invocations: vec![
                            dec_n.clone(),
                            gas_use(4, 1),
                            enact("n"),
                            dup("n", "n", "use"),
                            gas_use(3, 1),
                        ],
                        exit: test_n(4),
                    },
                    Block {
                        // 3
                        invocations: vec![refund(2), ignore("n")],
                        exit: BlockExit::Return(vec![as_id("gb"), as_id("one")]),
                    },
                    Block {
                        // 4
                        invocations: vec![],
                        exit: as_exit(dup("one", "a", "b")),
                    },
                    Block {
                        // 5
                        invocations: vec![Invocation {
                            ext: Extension {
                                name: "split_gas".to_string(),
                                tmpl_args: vec![val_arg(1), val_arg(1)],
                            },
                            args: vec![as_id("cost")],
                            results: vec![as_id("split_gas_cost"), as_id("use_cost")],
                        }],
                        exit: BlockExit::Jump(JumpInfo {
                            ext: Extension {
                                name: "get_gas".to_string(),
                                tmpl_args: vec![val_arg(4)]
                            },
                            args: vec![as_id("gb"), as_id("split_gas_cost")],
                            branches: vec![
                                BranchInfo {
                                    target: BranchTarget::Block(BlockId(7)),
                                    exports: vec![as_id("gb"), as_id("cost")]
                                },
                                BranchInfo {
                                    target: BranchTarget::Fallthrough,
                                    exports: vec![as_id("gb")]
                                }
                            ],
                        }),
                    },
                    Block {
                        // 6
                        invocations: vec![
                            Invocation {
                                ext: Extension {
                                    name: "constant_num".to_string(),
                                    tmpl_args: vec![type_arg(int_type()), val_arg(-1)],
                                },
                                args: vec![],
                                results: vec![as_id("minus")],
                            },
                            enact("minus"),
                            ignore("a"),
                            ignore("b"),
                            ignore("n"),
                        ],
                        exit: BlockExit::Return(vec![as_id("gb"), as_id("minus")]),
                    },
                    Block {
                        // 7
                        invocations: vec![
                            dup("a", "a", "tmp"),
                            Invocation {
                                ext: Extension {
                                    name: "add".to_string(),
                                    tmpl_args: vec![type_arg(int_type())],
                                },
                                args: vec![as_id("a"), as_id("b")],
                                results: vec![as_id("a")],
                            },
                            enact("a"),
                            dup("tmp", "tmp", "b"),
                            ignore("tmp"),
                            dec_n.clone(),
                            gas_use(4, 1),
                            enact("n"),
                            dup("n", "n", "use"),
                            gas_use(3, 1),
                        ],
                        exit: test_n(5),
                    },
                    Block {
                        // 8
                        invocations: vec![refund(2), ignore("n"), ignore("b")],
                        exit: BlockExit::Return(vec![as_id("gb"), as_id("a")]),
                    },
                ],
                funcs: vec![Function {
                    name: "Fibonacci".to_string(),
                    args: vec![
                        typed(as_id("gb"), gas_builtin_type()),
                        typed(as_id("n"), int_type()),
                        typed(as_id("cost"), gas_type(6)),
                    ],
                    res_types: vec![gas_builtin_type(), int_type()],
                    entry: BlockId(0),
                }]
            }),
            Ok(())
        );
    }

    #[test]
    fn fibonacci_using_recursion() {
        let dup = |name, other| Invocation {
            ext: Extension {
                name: "duplicate_num".to_string(),
                tmpl_args: vec![type_arg(int_type())],
            },
            args: vec![as_id(name)],
            results: vec![as_id(name), as_id(other)],
        };
        let gas_use = |curr, taken| Invocation {
            ext: Extension {
                name: "split_gas".to_string(),
                tmpl_args: vec![val_arg(taken), val_arg(curr - taken)],
            },
            args: vec![as_id("cost")],
            results: vec![as_id("use_cost"), as_id("cost")],
        };
        let refund = |count| Invocation {
            ext: Extension {
                name: "refund_gas".to_string(),
                tmpl_args: vec![val_arg(count)],
            },
            args: vec![as_id("gb"), as_id("cost")],
            results: vec![as_id("gb")],
        };
        let ignore = |name| Invocation {
            ext: Extension {
                name: "ignore_num".to_string(),
                tmpl_args: vec![type_arg(int_type())],
            },
            args: vec![as_id(name)],
            results: vec![],
        };

        let test_n = |b_success| {
            BlockExit::Jump(JumpInfo {
                ext: Extension {
                    name: "jump_nz".to_string(),
                    tmpl_args: vec![type_arg(int_type())],
                },
                args: vec![as_id("use"), as_id("use_cost")],
                branches: vec![
                    BranchInfo {
                        target: BranchTarget::Block(BlockId(b_success)),
                        exports: vec![],
                    },
                    BranchInfo {
                        target: BranchTarget::Fallthrough,
                        exports: vec![],
                    },
                ],
            })
        };

        let enact = |name| Invocation {
            ext: Extension {
                name: "enact_calc".to_string(),
                tmpl_args: vec![type_arg(int_type()), val_arg(0)],
            },
            args: vec![as_id(name), as_id("use_cost")],
            results: vec![as_id(name)],
        };

        assert_eq!(
            validate(&Program {
                blocks: vec![
                    Block {
                        // 0
                        invocations: vec![
                            Invocation {
                                ext: Extension {
                                    name: "constant_num".to_string(),
                                    tmpl_args: vec![type_arg(int_type()), val_arg(1)],
                                },
                                args: vec![],
                                results: vec![as_id("one")],
                            },
                            gas_use(6, 1),
                            enact("one"),
                            dup("n", "use"),
                            gas_use(5, 1),
                        ],
                        exit: test_n(2),
                    },
                    Block {
                        // 1
                        invocations: vec![refund(4), ignore("n")],
                        exit: BlockExit::Return(vec![as_id("gb"), as_id("one")]),
                    },
                    Block {
                        // 2
                        invocations: vec![
                            Invocation {
                                ext: Extension {
                                    name: "add".to_string(),
                                    tmpl_args: vec![type_arg(int_type()), val_arg(-1)],
                                },
                                args: vec![as_id("n")],
                                results: vec![as_id("n_1")],
                            },
                            gas_use(4, 1),
                            enact("n_1"),
                            dup("n_1", "use"),
                            gas_use(3, 1),
                        ],
                        exit: test_n(4),
                    },
                    Block {
                        // 3
                        invocations: vec![refund(2), ignore("n_1")],
                        exit: BlockExit::Return(vec![as_id("gb"), as_id("one")]),
                    },
                    Block {
                        // 4
                        invocations: vec![
                            ignore("one"),
                            Invocation {
                                ext: Extension {
                                    name: "split_gas".to_string(),
                                    tmpl_args: vec![val_arg(1), val_arg(1)],
                                },
                                args: vec![as_id("cost")],
                                results: vec![as_id("split_gas_cost"), as_id("use_cost")],
                            }
                        ],
                        exit: BlockExit::Jump(JumpInfo {
                            ext: Extension {
                                name: "get_gas".to_string(),
                                tmpl_args: vec![
                                    val_arg(1),
                                    val_arg(6),
                                    val_arg(2),
                                    val_arg(6),
                                    val_arg(2)
                                ]
                            },
                            args: vec![as_id("gb"), as_id("split_gas_cost")],
                            branches: vec![
                                BranchInfo {
                                    target: BranchTarget::Block(BlockId(6)),
                                    exports: vec![
                                        as_id("gb"),
                                        as_id("dec_cost"),
                                        as_id("call1_inner_cost"),
                                        as_id("call1_outer_cost"),
                                        as_id("call2_inner_cost"),
                                        as_id("call2_outer_cost"),
                                    ]
                                },
                                BranchInfo {
                                    target: BranchTarget::Fallthrough,
                                    exports: vec![as_id("gb")]
                                }
                            ],
                        }),
                    },
                    Block {
                        // 5
                        invocations: vec![
                            Invocation {
                                ext: Extension {
                                    name: "constant_num".to_string(),
                                    tmpl_args: vec![type_arg(int_type()), val_arg(-10000)],
                                },
                                args: vec![],
                                results: vec![as_id("minus")],
                            },
                            enact("minus"),
                            ignore("n_1"),
                        ],
                        exit: BlockExit::Return(vec![as_id("gb"), as_id("minus")]),
                    },
                    Block {
                        // 6
                        invocations: vec![
                            dup("n_1", "n_2"),
                            Invocation {
                                ext: Extension {
                                    name: "add".to_string(),
                                    tmpl_args: vec![type_arg(int_type()), val_arg(-1)],
                                },
                                args: vec![as_id("n_2")],
                                results: vec![as_id("n_2")],
                            },
                            Invocation {
                                ext: Extension {
                                    name: "enact_calc".to_string(),
                                    tmpl_args: vec![type_arg(int_type()), val_arg(1)],
                                },
                                args: vec![as_id("n_2"), as_id("dec_cost")],
                                results: vec![as_id("n_2")],
                            },
                            Invocation {
                                ext: Extension {
                                    name: "tuple_pack".to_string(),
                                    tmpl_args: vec![
                                        type_arg(gas_builtin_type()),
                                        type_arg(int_type()),
                                        type_arg(gas_type(6))
                                    ],
                                },
                                args: vec![as_id("gb"), as_id("n_1"), as_id("call1_inner_cost")],
                                results: vec![as_id("input")],
                            },
                            Invocation {
                                ext: Extension {
                                    name: "Fibonacci".to_string(),
                                    tmpl_args: vec![],
                                },
                                args: vec![as_id("input"), as_id("call1_outer_cost")],
                                results: vec![as_id("output")],
                            },
                            Invocation {
                                ext: Extension {
                                    name: "tuple_unpack".to_string(),
                                    tmpl_args: vec![
                                        type_arg(gas_builtin_type()),
                                        type_arg(int_type())
                                    ],
                                },
                                args: vec![as_id("output")],
                                results: vec![as_id("gb"), as_id("r1")],
                            },
                            Invocation {
                                ext: Extension {
                                    name: "tuple_pack".to_string(),
                                    tmpl_args: vec![
                                        type_arg(gas_builtin_type()),
                                        type_arg(int_type()),
                                        type_arg(gas_type(6))
                                    ],
                                },
                                args: vec![as_id("gb"), as_id("n_2"), as_id("call2_inner_cost")],
                                results: vec![as_id("input")],
                            },
                            Invocation {
                                ext: Extension {
                                    name: "Fibonacci".to_string(),
                                    tmpl_args: vec![],
                                },
                                args: vec![as_id("input"), as_id("call2_outer_cost")],
                                results: vec![as_id("output")],
                            },
                            Invocation {
                                ext: Extension {
                                    name: "tuple_unpack".to_string(),
                                    tmpl_args: vec![
                                        type_arg(gas_builtin_type()),
                                        type_arg(int_type())
                                    ],
                                },
                                args: vec![as_id("output")],
                                results: vec![as_id("gb"), as_id("r2")],
                            },
                            Invocation {
                                ext: Extension {
                                    name: "add".to_string(),
                                    tmpl_args: vec![type_arg(int_type())],
                                },
                                args: vec![as_id("r1"), as_id("r2")],
                                results: vec![as_id("r")],
                            },
                            enact("r"),
                        ],
                        exit: BlockExit::Return(vec![as_id("gb"), as_id("r")]),
                    },
                ],
                funcs: vec![Function {
                    name: "Fibonacci".to_string(),
                    args: vec![
                        typed(as_id("gb"), gas_builtin_type()),
                        typed(as_id("n"), int_type()),
                        typed(as_id("cost"), gas_type(6)),
                    ],
                    res_types: vec![gas_builtin_type(), int_type()],
                    entry: BlockId(0),
                }]
            }),
            Ok(())
        );
    }
}
