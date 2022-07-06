use crate::extensions::*;

struct UnconditionalJumpExtension {}

impl ExtensionImplementation for UnconditionalJumpExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        if !tmpl_args.is_empty() {
            return Err(Error::WrongNumberOfTypeArgs);
        }
        Ok(ExtensionSignature {
            args: vec![Type {
                name: "Gas".to_string(),
                args: vec![TemplateArg::Value(1)],
            }],
            results: vec![vec![]],
            fallthrough: None,
        })
    }
}

pub(super) fn register(registry: &mut ExtensionRegistry) {
    registry.insert("jump".to_string(), Box::new(UnconditionalJumpExtension {}));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn legal_usage() {
        assert_eq!(
            UnconditionalJumpExtension {}.get_signature(&vec![]),
            Ok(ExtensionSignature {
                args: vec![Type {
                    name: "Gas".to_string(),
                    args: vec![TemplateArg::Value(1)]
                }],
                results: vec![vec![]],
                fallthrough: None,
            })
        );
    }

    #[test]
    fn wrong_num_of_args() {
        assert_eq!(
            UnconditionalJumpExtension {}.get_signature(&vec![TemplateArg::Value(1)]),
            Err(Error::WrongNumberOfTypeArgs)
        );
    }
}
