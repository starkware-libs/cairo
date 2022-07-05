use crate::extensions::*;

struct ArithmeticExtension {}

impl InvokeExtension for ArithmeticExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Type>), Error> {
        if tmpl_args.len() != 1 {
            return Err(Error::WrongNumberOfTypeArgs);
        }
        let numeric_type = match &tmpl_args[0] {
            TemplateArg::Type(t) => Ok(t),
            TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
        }?;
        Ok((
            vec![
                numeric_type.clone(),
                numeric_type.clone(),
                Type::Template("Gas".to_string(), vec![TemplateArg::Value(1)]),
            ],
            vec![numeric_type.clone()],
        ))
    }
}

pub(super) fn register(registry: &mut ExtensionRegistry) {
    for op in ["add", "sub", "mul", "div"] {
        registry
            .invoke_exts
            .insert(op.to_string(), Box::new(ArithmeticExtension {}));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn legal_usage() {
        let ty = Type::Basic("int".to_string());
        assert_eq!(
            ArithmeticExtension {}.get_signature(&vec![TemplateArg::Type(ty.clone())]),
            Ok((
                vec![
                    ty.clone(),
                    ty.clone(),
                    Type::Template("Gas".to_string(), vec![TemplateArg::Value(1)]),
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
