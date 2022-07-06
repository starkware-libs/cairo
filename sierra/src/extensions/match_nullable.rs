use crate::extensions::*;

struct MatchNullableExtension {}

impl ExtensionImplementation for MatchNullableExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        if tmpl_args.len() != 1 {
            return Err(Error::WrongNumberOfTypeArgs);
        }
        let inner_type = match &tmpl_args[0] {
            TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
            TemplateArg::Type(t) => Ok(t),
        }?;
        Ok(ExtensionSignature {
            args: vec![
                Type {
                    name: "Nullable".to_string(),
                    args: vec![TemplateArg::Type(inner_type.clone())],
                },
                Type {
                    name: "Gas".to_string(),
                    args: vec![TemplateArg::Value(1)],
                },
            ],
            results: vec![vec![inner_type.clone()], vec![]],
            fallthrough: Some(1),
        })
    }
}

pub(super) fn register(registry: &mut ExtensionRegistry) {
    registry.insert(
        "match_nullable".to_string(),
        Box::new(MatchNullableExtension {}),
    );
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
            MatchNullableExtension {}.get_signature(&vec![TemplateArg::Type(ty.clone())]),
            Ok(ExtensionSignature {
                args: vec![
                    Type {
                        name: "Nullable".to_string(),
                        args: vec![TemplateArg::Type(ty.clone())]
                    },
                    Type {
                        name: "Gas".to_string(),
                        args: vec![TemplateArg::Value(1)]
                    },
                ],
                results: vec![vec![ty], vec![]],
                fallthrough: Some(1),
            })
        );
    }

    #[test]
    fn wrong_num_of_args() {
        assert_eq!(
            MatchNullableExtension {}.get_signature(&vec![]),
            Err(Error::WrongNumberOfTypeArgs)
        );
    }

    #[test]
    fn wrong_arg_type() {
        assert_eq!(
            MatchNullableExtension {}.get_signature(&vec![TemplateArg::Value(1)]),
            Err(Error::UnsupportedTypeArg)
        );
    }
}
