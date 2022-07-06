use crate::extensions::*;

struct JumpNzExtension {}

impl ExtensionImplementation for JumpNzExtension {
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
        Ok(ExtensionSignature {
            args: vec![
                numeric_type.clone(),
                Type {
                    name: "Gas".to_string(),
                    args: vec![TemplateArg::Value(1)],
                },
            ],
            results: vec![vec![], vec![]],
            fallthrough: Some(1),
        })
    }
}

pub(super) fn register(registry: &mut ExtensionRegistry) {
    registry.insert("jump_nz".to_string(), Box::new(JumpNzExtension {}));
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
            Ok(ExtensionSignature {
                args: vec![
                    ty,
                    Type {
                        name: "Gas".to_string(),
                        args: vec![TemplateArg::Value(1)]
                    },
                ],
                results: vec![vec![], vec![]],
                fallthrough: Some(1),
            })
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
