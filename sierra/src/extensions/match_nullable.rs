use crate::extensions::*;

struct MatchNullableExtension {}

impl JumpExtension for MatchNullableExtension {
    fn get_effects(self: &Self, jump: &JumpInfo) -> Result<HashMap<BlockId, ScopeChange>, Error> {
        if jump.ext.tmpl_args.len() != 1 {
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
        if success.exports.len() != 1 {
            return Err(Error::WrongNumberOfResults(jump.to_string()));
        }
        if !failure.exports.is_empty() {
            return Err(Error::WrongNumberOfResults(jump.to_string()));
        }
        let inner_type = match &jump.ext.tmpl_args[0] {
            TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
            TemplateArg::Type(t) => Ok(t),
        }?;
        let args = vec![
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
        ];
        Ok(HashMap::<BlockId, ScopeChange>::from([
            (
                success.block,
                ScopeChange {
                    args: args.clone(),
                    results: vec![TypedVar {
                        name: success.exports[0].clone(),
                        ty: inner_type.clone(),
                    }],
                },
            ),
            (
                failure.block,
                ScopeChange {
                    args: args,
                    results: vec![],
                },
            ),
        ]))
    }
}

pub(super) fn register(registry: &mut ExtensionRegistry) {
    registry.jump_exts.insert(
        "match_nullable".to_string(),
        Box::new(MatchNullableExtension {}),
    );
}

#[test]
fn mapping() {
    let cost = TypedVar {
        name: "cost".to_string(),
        ty: Type::Template("Gas".to_string(), vec![TemplateArg::Value(1)]),
    };
    let a = TypedVar {
        name: "a".to_string(),
        ty: Type::Basic("int".to_string()),
    };
    let a_wrap = TypedVar {
        name: "a_wrap".to_string(),
        ty: Type::Template(
            "Nullable".to_string(),
            vec![TemplateArg::Type(a.ty.clone())],
        ),
    };
    assert_eq!(
        MatchNullableExtension {}.get_effects(&JumpInfo {
            ext: Extension {
                name: "".to_string(),
                tmpl_args: vec![TemplateArg::Type(a.ty.clone())]
            },
            args: vec![a_wrap.name.clone(), cost.name.clone()],
            branches: vec![
                BranchInfo {
                    block: BlockId(0),
                    exports: vec![a.name.clone()]
                },
                BranchInfo {
                    block: BlockId(1),
                    exports: vec![]
                }
            ],
        }),
        Ok(HashMap::<BlockId, ScopeChange>::from([
            (
                BlockId(0),
                ScopeChange {
                    args: vec![a_wrap.clone(), cost.clone()],
                    results: vec![a],
                },
            ),
            (
                BlockId(1),
                ScopeChange {
                    args: vec![a_wrap, cost],
                    results: vec![],
                },
            )
        ]))
    );
}
