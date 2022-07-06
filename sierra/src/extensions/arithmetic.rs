use crate::extensions::*;

struct ArithmeticExtension {}

impl ExtensionImplementation for ArithmeticExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        let numeric_type = get_type(tmpl_args)?;
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

struct DuplicateExtension {}

impl ExtensionImplementation for DuplicateExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        let numeric_type = get_type(tmpl_args)?;
        Ok(simple_invoke_ext_sign(
            vec![numeric_type.clone()],
            vec![numeric_type.clone(), numeric_type.clone()],
        ))
    }
}

struct ConstantExtension {}

impl ExtensionImplementation for ConstantExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        if tmpl_args.len() != 2 {
            return Err(Error::WrongNumberOfTypeArgs);
        }
        let numeric_type = match &tmpl_args[0] {
            TemplateArg::Type(t) if matches!(t.to_string().as_str(), "int" | "felt") => Ok(t),
            _ => Err(Error::UnsupportedTypeArg),
        }?;
        match &tmpl_args[1] {
            TemplateArg::Value(_) => {}
            _ => {
                return Err(Error::UnsupportedTypeArg);
            }
        }
        Ok(simple_invoke_ext_sign(
            vec![Type {
                name: "Gas".to_string(),
                args: vec![TemplateArg::Value(1)],
            }],
            vec![numeric_type.clone()],
        ))
    }
}

struct IgnoreExtension {}

impl ExtensionImplementation for IgnoreExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        let numeric_type = get_type(tmpl_args)?;
        Ok(simple_invoke_ext_sign(vec![numeric_type.clone()], vec![]))
    }
}

fn get_type<'a>(tmpl_args: &'a Vec<TemplateArg>) -> Result<&'a Type, Error> {
    if tmpl_args.len() != 1 {
        return Err(Error::WrongNumberOfTypeArgs);
    }
    match &tmpl_args[0] {
        TemplateArg::Type(t) if matches!(t.to_string().as_str(), "int" | "felt") => Ok(t),
        _ => Err(Error::UnsupportedTypeArg),
    }
}

pub(super) fn extensions() -> [(String, ExtensionBox); 7] {
    [
        ("add".to_string(), Box::new(ArithmeticExtension {})),
        ("sub".to_string(), Box::new(ArithmeticExtension {})),
        ("mul".to_string(), Box::new(ArithmeticExtension {})),
        ("div".to_string(), Box::new(ArithmeticExtension {})),
        ("duplicate_num".to_string(), Box::new(DuplicateExtension {})),
        ("constant_num".to_string(), Box::new(ConstantExtension {})),
        ("ignore_num".to_string(), Box::new(IgnoreExtension {})),
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
                vec![ty.clone()],
            ))
        );
        assert_eq!(
            DuplicateExtension {}.get_signature(&vec![TemplateArg::Type(ty.clone())]),
            Ok(simple_invoke_ext_sign(
                vec![ty.clone()],
                vec![ty.clone(), ty],
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
