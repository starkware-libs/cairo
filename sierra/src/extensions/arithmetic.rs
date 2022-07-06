use crate::extensions::*;

struct ArithmeticExtension {}

impl ExtensionImplementation for ArithmeticExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        if tmpl_args.len() != 1 {
            return Err(Error::WrongNumberOfTypeArgs);
        }
        let numeric_type = match &tmpl_args[0] {
            TemplateArg::Type(t) => Ok(t),
            TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
        }?;
        Ok(simple_invoke_ext_sign(
            vec![
                numeric_type.clone(),
                numeric_type.clone(),
                Type {
                    name: "Gas".to_string(),
                    args: vec![TemplateArg::Value(1)],
                },
            ],
            vec![numeric_type.clone()],
        ))
    }
}

pub(super) fn extensions() -> [(String, ExtensionBox); 4] {
    [
        ("add".to_string(), Box::new(ArithmeticExtension {})),
        ("sub".to_string(), Box::new(ArithmeticExtension {})),
        ("mul".to_string(), Box::new(ArithmeticExtension {})),
        ("div".to_string(), Box::new(ArithmeticExtension {})),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn legal_usage() {
        let ty = Type {
            name: "int".to_string(),
            args: vec![],
        };
        assert_eq!(
            ArithmeticExtension {}.get_signature(&vec![TemplateArg::Type(ty.clone())]),
            Ok(simple_invoke_ext_sign(
                vec![
                    ty.clone(),
                    ty.clone(),
                    Type {
                        name: "Gas".to_string(),
                        args: vec![TemplateArg::Value(1)]
                    },
                ],
                vec![ty],
            ))
        );
    }

    #[test]
    fn wrong_num_of_args() {
        assert_eq!(
            ArithmeticExtension {}.get_signature(&vec![]),
            Err(Error::WrongNumberOfTypeArgs)
        );
    }

    #[test]
    fn wrong_arg_type() {
        assert_eq!(
            ArithmeticExtension {}.get_signature(&vec![TemplateArg::Value(1)]),
            Err(Error::UnsupportedTypeArg)
        );
    }
}
