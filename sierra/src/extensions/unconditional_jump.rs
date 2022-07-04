use crate::extensions::*;

struct UnconditionalJumpExtension {}

impl JumpExtension for UnconditionalJumpExtension {
    fn get_effects(self: &Self, jump: &JumpInfo) -> Result<HashMap<usize, ScopeChange>, Error> {
        if !jump.libcall.tmpl_args.is_empty() {
            return Err(Error::WrongNumberOfTypeArgs(jump.to_string()));
        }
        if jump.args.len() != 1 {
            return Err(Error::WrongNumberOfArgs(jump.to_string()));
        }
        if jump.branches.len() != 1 {
            return Err(Error::WrongNumberOfBranches(jump.to_string()));
        }
        if !jump.branches[0].exports.is_empty() {
            return Err(Error::WrongNumberOfResults(jump.to_string()));
        }
        Ok(HashMap::<usize, ScopeChange>::from([(
            jump.branches[0].block,
            ScopeChange {
                args: vec![TypedVar {
                    name: jump.args[0].clone(),
                    ty: Type::Template("Gas".to_string(), vec![TemplateArg::Value(1)]),
                }],
                results: vec![],
            },
        )]))
    }
}

pub(super) fn register(registry: &mut ExtensionRegistry) {
    registry
        .jump_libcalls
        .insert("jump".to_string(), Box::new(UnconditionalJumpExtension {}));
}

#[test]
fn mapping() {
    let cost = TypedVar {
        name: "cost".to_string(),
        ty: Type::Template("Gas".to_string(), vec![TemplateArg::Value(1)]),
    };
    assert_eq!(
        UnconditionalJumpExtension {}.get_effects(&JumpInfo {
            libcall: LibCall {
                name: "".to_string(),
                tmpl_args: vec![]
            },
            args: vec![cost.name.clone()],
            branches: vec![BranchInfo {
                block: 1,
                exports: vec![]
            }],
        }),
        Ok(HashMap::<usize, ScopeChange>::from([(
            1,
            ScopeChange {
                args: vec![cost],
                results: vec![],
            },
        )]))
    );
}
