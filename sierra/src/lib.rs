mod error;
mod extensions;
mod graph;
mod scope_state;

use crate::error::Error;
use crate::extensions::*;
use crate::graph::*;
use crate::scope_state::*;
use Result::*;

pub fn test_function(f: &Function) -> Result<(), Error> {
    let found_ret = test_function_helper(
        f,
        0,
        f.args
            .iter()
            .map(|v| (v.name.clone(), v.ty.clone()))
            .collect(),
        &mut vec![None; f.blocks.len()],
    )?;
    if !found_ret {
        return Err(Error::FunctionNoExit);
    }
    Ok(())
}

fn test_function_helper(
    f: &Function,
    b: usize,
    start_state: ScopeState,
    block_start_states: &mut Vec<Option<ScopeState>>,
) -> Result<bool, Error> {
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
                Ok(false)
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
                })?;
            Ok(true)
        }
        BlockExit::Continue => test_function_helper(f, b + 1, state, block_start_states),
        BlockExit::Jump(j) => {
            let changes = get_jump_effects(j)?;
            let found_ret = changes.iter().try_fold(false, |_, (next_block, change)| {
                let next_state = next_state(&change, state.clone())?;
                test_function_helper(f, *next_block, next_state, block_start_states)
            })?;
            Ok(found_ret)
        }
    }
}

#[cfg(test)]
mod full_function {
    use super::*;

    #[test]
    fn empty() {
        let typed = |var_name: &str, type_name: &str| TypedVar {
            name: var_name.to_string(),
            ty: Type::Basic(type_name.to_string()),
        };
        assert_eq!(
            test_function(&Function {
                name: "Some".to_string(),
                args: vec![typed("gb", "GasBuiltin"), typed("a", "felt")],
                res_types: vec![
                    Type::Basic("GasBuiltin".to_string()),
                    Type::Basic("felt".to_string())
                ],
                blocks: vec![],
            }),
            Err(Error::FunctionBlockOutOfBounds)
        );
    }

    #[test]
    fn inifinite_gas_take_or_return() {
        let typed = |var_name: &str, type_name: &str| TypedVar {
            name: var_name.to_string(),
            ty: Type::Basic(type_name.to_string()),
        };
        assert_eq!(
            test_function(&Function {
                name: "Some".to_string(),
                args: vec![
                    typed("gb", "GasBuiltin"),
                    TypedVar {
                        name: "cost".to_string(),
                        ty: Type::Template("Cost".to_string(), vec![TemplateArg::Value(1)]),
                    }
                ],
                res_types: vec![Type::Basic("GasBuiltin".to_string())],
                blocks: vec![
                    Block {
                        invocations: vec![],
                        exit: BlockExit::Jump(JumpInfo {
                            libcall: LibCall {
                                name: "deduct_gas".to_string(),
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
            Err(Error::FunctionNoExit)
        );
    }
}
