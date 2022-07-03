use crate::error::Error;
use crate::graph::*;
use crate::scope_state::*;
use std::collections::HashMap;
use Result::*;

#[derive(Clone, Debug, PartialEq)]
pub struct CodeEffects {
    pub mapping: TypedMapping,
    pub cost: i64,
}

pub fn get_invoke_effects(invc: &Invocation) -> Result<CodeEffects, Error> {
    match invc.libcall.name.as_str() {
        "add" | "sub" | "mul" | "div" => {
            if invc.libcall.tmpl_args.len() != 1 {
                return Err(Error::WrongNumberOfTypeArgs(invc.to_string()));
            }
            if invc.args.len() != 2 {
                return Err(Error::WrongNumberOfArgs(invc.to_string()));
            }
            if invc.results.len() != 1 {
                return Err(Error::WrongNumberOfResults(invc.to_string()));
            }
            let numeric_type = match &invc.libcall.tmpl_args[0] {
                TemplateArg::Type(t) => Ok(t),
                TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
            }?;
            Ok(CodeEffects {
                mapping: TypedMapping {
                    args: vec![
                        TypedVar {
                            name: invc.args[0].clone(),
                            ty: numeric_type.clone(),
                        },
                        TypedVar {
                            name: invc.args[1].clone(),
                            ty: numeric_type.clone(),
                        },
                    ],
                    results: vec![TypedVar {
                        name: invc.results[0].clone(),
                        ty: numeric_type.clone(),
                    }],
                },
                cost: 1,
            })
        }
        _ => Err(Error::UnsupportedLibCallName),
    }
}

pub fn get_jump_effects(jump: &JumpInfo) -> Result<HashMap<u64, CodeEffects>, Error> {
    match jump.libcall.name.as_str() {
        "jump" => {
            if !jump.libcall.tmpl_args.is_empty() {
                return Err(Error::WrongNumberOfTypeArgs(jump.to_string()));
            }
            if !jump.args.is_empty() {
                return Err(Error::WrongNumberOfArgs(jump.to_string()));
            }
            if jump.branches.len() != 1 {
                return Err(Error::WrongNumberOfBranches(jump.to_string()));
            }
            if !jump.branches[0].exports.is_empty() {
                return Err(Error::WrongNumberOfResults(jump.to_string()));
            }
            Ok(HashMap::<u64, CodeEffects>::from([(
                jump.branches[0].block,
                CodeEffects {
                    mapping: TypedMapping {
                        args: vec![],
                        results: vec![],
                    },
                    cost: 1,
                },
            )]))
        }
        "jump_nz" => {
            if jump.libcall.tmpl_args.len() != 1 {
                return Err(Error::WrongNumberOfTypeArgs(jump.to_string()));
            }
            if jump.args.len() != 1 {
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
            Ok(HashMap::<u64, CodeEffects>::from([
                (
                    success.block,
                    CodeEffects {
                        mapping: TypedMapping {
                            args: vec![TypedVar {
                                name: jump.args[0].clone(),
                                ty: numeric_type.clone(),
                            }],
                            results: vec![],
                        },
                        cost: 1,
                    },
                ),
                (
                    failure.block,
                    CodeEffects {
                        mapping: TypedMapping {
                            args: vec![TypedVar {
                                name: jump.args[0].clone(),
                                ty: numeric_type.clone(),
                            }],
                            results: vec![],
                        },
                        cost: 1,
                    },
                ),
            ]))
        }
        "deduct_gas" => {
            if jump.libcall.tmpl_args.len() != 1 {
                return Err(Error::WrongNumberOfTypeArgs(jump.to_string()));
            }
            if jump.args.len() != 1 {
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
            if failure.exports.len() == 1 {
                return Err(Error::WrongNumberOfResults(jump.to_string()));
            }
            let usage = match &jump.libcall.tmpl_args[0] {
                TemplateArg::Value(v) => Ok(v),
                TemplateArg::Type(_) => Err(Error::UnsupportedTypeArg),
            }?;
            let gas_type = Type::Basic("GasBuiltin".to_string());
            Ok(HashMap::<u64, CodeEffects>::from([
                (
                    success.block,
                    CodeEffects {
                        mapping: TypedMapping {
                            args: vec![TypedVar {
                                name: jump.args[0].clone(),
                                ty: gas_type.clone(),
                            }],
                            results: vec![TypedVar {
                                name: success.exports[0].clone(),
                                ty: gas_type.clone(),
                            }],
                        },
                        cost: 1 - usage,
                    },
                ),
                (
                    failure.block,
                    CodeEffects {
                        mapping: TypedMapping {
                            args: vec![TypedVar {
                                name: jump.args[0].clone(),
                                ty: gas_type.clone(),
                            }],
                            results: vec![TypedVar {
                                name: failure.exports[0].clone(),
                                ty: gas_type.clone(),
                            }],
                        },
                        cost: 1,
                    },
                ),
            ]))
        }
        "match_nullable" => {
            if jump.libcall.tmpl_args.len() != 1 {
                return Err(Error::WrongNumberOfTypeArgs(jump.to_string()));
            }
            if jump.args.len() != 1 {
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
            Ok(HashMap::<u64, CodeEffects>::from([
                (
                    success.block,
                    CodeEffects {
                        mapping: TypedMapping {
                            args: vec![TypedVar {
                                name: jump.args[0].clone(),
                                ty: Type::Template(
                                    "Nullable".to_string(),
                                    vec![TemplateArg::Type(inner_type.clone())],
                                ),
                            }],
                            results: vec![TypedVar {
                                name: success.exports[0].clone(),
                                ty: inner_type.clone(),
                            }],
                        },
                        cost: 1,
                    },
                ),
                (
                    failure.block,
                    CodeEffects {
                        mapping: TypedMapping {
                            args: vec![TypedVar {
                                name: jump.args[0].clone(),
                                ty: Type::Template(
                                    "Nullable".to_string(),
                                    vec![TemplateArg::Type(inner_type.clone())],
                                ),
                            }],
                            results: vec![],
                        },
                        cost: 1,
                    },
                ),
            ]))
        }
        _ => {
            return Err(Error::UnsupportedLibCallName);
        }
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
        let c = typed("c", "int");
        assert_eq!(
            get_invoke_effects(&Invocation {
                libcall: LibCall {
                    name: "add".to_string(),
                    tmpl_args: vec![TemplateArg::Type(a.ty.clone())]
                },
                args: vec![a.name.clone(), b.name.clone()],
                results: vec![c.name.clone()],
            }),
            Ok(CodeEffects {
                mapping: TypedMapping {
                    args: vec![a, b],
                    results: vec![c],
                },
                cost: 1
            })
        );
    }

    #[test]
    fn get_jump_mapping() {
        assert_eq!(
            get_jump_effects(&JumpInfo {
                libcall: LibCall {
                    name: "jump".to_string(),
                    tmpl_args: vec![]
                },
                args: vec![],
                branches: vec![BranchInfo {
                    block: 1,
                    exports: vec![]
                }],
            }),
            Ok(HashMap::<u64, CodeEffects>::from([(
                1,
                CodeEffects {
                    mapping: TypedMapping {
                        args: vec![],
                        results: vec![],
                    },
                    cost: 1,
                },
            )]))
        );
    }
}
