use crate::extensions::*;

struct ArithmeticExtension {}

impl InvokeExtension for ArithmeticExtension {
    fn get_effects(self: &Self, invc: &Invocation) -> Result<ScopeChange, Error> {
        if invc.libcall.tmpl_args.len() != 1 {
            return Err(Error::WrongNumberOfTypeArgs(invc.to_string()));
        }
        if invc.args.len() != 3 {
            return Err(Error::WrongNumberOfArgs(invc.to_string()));
        }
        if invc.results.len() != 1 {
            return Err(Error::WrongNumberOfResults(invc.to_string()));
        }
        let numeric_type = match &invc.libcall.tmpl_args[0] {
            TemplateArg::Type(t) => Ok(t),
            TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
        }?;
        Ok(ScopeChange {
            args: vec![
                TypedVar {
                    name: invc.args[0].clone(),
                    ty: numeric_type.clone(),
                },
                TypedVar {
                    name: invc.args[1].clone(),
                    ty: numeric_type.clone(),
                },
                TypedVar {
                    name: invc.args[2].clone(),
                    ty: Type::Template("Gas".to_string(), vec![TemplateArg::Value(1)]),
                },
            ],
            results: vec![TypedVar {
                name: invc.results[0].clone(),
                ty: numeric_type.clone(),
            }],
        })
    }
}

pub(super) fn register(registry: &mut ExtensionRegistry) {
    for op in ["add", "sub", "mul", "div"] {
        registry
            .invoke_libcalls
            .insert(op.to_string(), Box::new(ArithmeticExtension {}));
    }
}

#[test]
fn mapping() {
    let typed = |var_name: &str, type_name: &str| TypedVar {
        name: var_name.to_string(),
        ty: Type::Basic(type_name.to_string()),
    };
    let a = typed("a", "int");
    let b = typed("b", "int");
    let cost = TypedVar {
        name: "cost".to_string(),
        ty: Type::Template("Gas".to_string(), vec![TemplateArg::Value(1)]),
    };
    let c = typed("c", "int");
    assert_eq!(
        ArithmeticExtension {}.get_effects(&Invocation {
            libcall: LibCall {
                name: "".to_string(),
                tmpl_args: vec![TemplateArg::Type(a.ty.clone())]
            },
            args: vec![a.name.clone(), b.name.clone(), cost.name.clone()],
            results: vec![c.name.clone()],
        }),
        Ok(ScopeChange {
            args: vec![a, b, cost],
            results: vec![c],
        })
    );
}
