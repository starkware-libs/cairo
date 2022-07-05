use crate::extensions::*;

struct JumpNzExtension {}

impl JumpExtension for JumpNzExtension {
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
        if !success.exports.is_empty() {
            return Err(Error::WrongNumberOfResults(jump.to_string()));
        }
        if !failure.exports.is_empty() {
            return Err(Error::WrongNumberOfResults(jump.to_string()));
        }
        let numeric_type = match &jump.ext.tmpl_args[0] {
            TemplateArg::Type(t) => Ok(t),
            TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
        }?;
        let change = ScopeChange {
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
        };
        Ok(HashMap::<BlockId, ScopeChange>::from([
            (success.block, change.clone()),
            (failure.block, change),
        ]))
    }
}

pub(super) fn register(registry: &mut ExtensionRegistry) {
    registry
        .jump_exts
        .insert("jump_nz".to_string(), Box::new(JumpNzExtension {}));
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
    assert_eq!(
        JumpNzExtension {}.get_effects(&JumpInfo {
            ext: Extension {
                name: "".to_string(),
                tmpl_args: vec![TemplateArg::Type(a.ty.clone())]
            },
            args: vec![a.name.clone(), cost.name.clone()],
            branches: vec![
                BranchInfo {
                    block: BlockId(0),
                    exports: vec![]
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
                    args: vec![a.clone(), cost.clone()],
                    results: vec![],
                },
            ),
            (
                BlockId(1),
                ScopeChange {
                    args: vec![a, cost],
                    results: vec![],
                },
            )
        ]))
    );
}
