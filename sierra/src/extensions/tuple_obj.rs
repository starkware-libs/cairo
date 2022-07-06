use crate::extensions::*;

struct TuplePackExtension {}

impl ExtensionImplementation for TuplePackExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        let arg_types = types(tmpl_args)?;
        Ok(simple_invoke_ext_sign(
            arg_types,
            vec![Type {
                name: "Tuple".to_string(),
                args: tmpl_args.clone(),
            }],
        ))
    }
}

struct TupleUnpackExtension {}

impl ExtensionImplementation for TupleUnpackExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        let arg_types = types(tmpl_args)?;
        Ok(simple_invoke_ext_sign(
            vec![Type {
                name: "Tuple".to_string(),
                args: tmpl_args.clone(),
            }],
            arg_types,
        ))
    }
}

fn types(tmpl_args: &Vec<TemplateArg>) -> Result<Vec<Type>, Error> {
    let mut result = vec![];
    tmpl_args.iter().try_for_each(|tmpl_arg| match tmpl_arg {
        TemplateArg::Type(t) => {
            result.push(t.clone());
            Ok(())
        }
        TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
    })?;
    Ok(result)
}

pub(super) fn extensions() -> [(String, ExtensionBox); 2] {
    [
        ("tuple_pack".to_string(), Box::new(TuplePackExtension {})),
        (
            "tuple_unpack".to_string(),
            Box::new(TupleUnpackExtension {}),
        ),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ty(name: &str) -> Type {
        Type {
            name: name.to_string(),
            args: vec![],
        }
    }

    #[test]
    fn pack_legal_usage() {
        assert_eq!(
            TuplePackExtension {}.get_signature(&vec![
                TemplateArg::Type(ty("1")),
                TemplateArg::Type(ty("2"))
            ]),
            Ok(simple_invoke_ext_sign(
                vec![ty("1"), ty("2")],
                vec![Type {
                    name: "Tuple".to_string(),
                    args: vec![TemplateArg::Type(ty("1")), TemplateArg::Type(ty("2"))]
                }],
            ))
        );
    }

    #[test]
    fn pack_wrong_arg_type() {
        assert_eq!(
            TuplePackExtension {}.get_signature(&vec![TemplateArg::Value(1)]),
            Err(Error::UnsupportedTypeArg)
        );
    }

    #[test]
    fn unpack_legal_usage() {
        assert_eq!(
            TupleUnpackExtension {}.get_signature(&vec![
                TemplateArg::Type(ty("1")),
                TemplateArg::Type(ty("2"))
            ]),
            Ok(simple_invoke_ext_sign(
                vec![Type {
                    name: "Tuple".to_string(),
                    args: vec![TemplateArg::Type(ty("1")), TemplateArg::Type(ty("2"))]
                }],
                vec![ty("1"), ty("2")],
            ))
        );
    }

    #[test]
    fn unpack_wrong_arg_type() {
        assert_eq!(
            TupleUnpackExtension {}.get_signature(&vec![TemplateArg::Value(1)]),
            Err(Error::UnsupportedTypeArg)
        );
    }
}
