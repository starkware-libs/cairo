use crate::extensions::*;

struct UnconditionalJumpExtension {}

impl JumpExtension for UnconditionalJumpExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Vec<Type>>), Error> {
        if !tmpl_args.is_empty() {
            return Err(Error::WrongNumberOfTypeArgs);
        }
        Ok((
            vec![Type::Template(
                "Gas".to_string(),
                vec![TemplateArg::Value(1)],
            )],
            vec![vec![]],
        ))
    }
}

pub(super) fn register(registry: &mut ExtensionRegistry) {
    registry
        .jump_exts
        .insert("jump".to_string(), Box::new(UnconditionalJumpExtension {}));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn legal_usage() {
        assert_eq!(
            UnconditionalJumpExtension {}.get_signature(&vec![]),
            Ok((
                vec![Type::Template(
                    "Gas".to_string(),
                    vec![TemplateArg::Value(1)]
                ),],
                vec![vec![]],
            ))
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
