use crate::extensions::*;

struct JumpNzExtension {}

impl JumpExtension for JumpNzExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Vec<Type>>), Error> {
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
                Type {
                    name: "Gas".to_string(),
                    args: vec![TemplateArg::Value(1)],
                },
            ],
            vec![vec![], vec![]],
        ))
    }
}

pub(super) fn register(registry: &mut ExtensionRegistry) {
    registry
        .jump_exts
        .insert("jump_nz".to_string(), Box::new(JumpNzExtension {}));
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
            JumpNzExtension {}.get_signature(&vec![TemplateArg::Type(ty.clone())]),
            Ok((
                vec![
                    ty,
                    Type {
                        name: "Gas".to_string(),
                        args: vec![TemplateArg::Value(1)]
                    },
                ],
                vec![vec![], vec![]],
            ))
        );
    }

    #[test]
    fn wrong_num_of_args() {
        assert_eq!(
            JumpNzExtension {}.get_signature(&vec![]),
            Err(Error::WrongNumberOfTypeArgs)
        );
    }

    #[test]
    fn wrong_arg_type() {
        assert_eq!(
            JumpNzExtension {}.get_signature(&vec![TemplateArg::Value(1)]),
            Err(Error::UnsupportedTypeArg)
        );
    }
}
