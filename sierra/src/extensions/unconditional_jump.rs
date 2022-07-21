use crate::extensions::*;

struct UnconditionalJumpExtension {}

impl ExtensionImplementation for UnconditionalJumpExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        validate_size_eq(tmpl_args, 0)?;
        Ok(ExtensionSignature {
            args: vec![],
            results: vec![vec![]],
            fallthrough: None,
        })
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        context: Context,
        _arg_refs: Vec<RefValue>,
    ) -> Result<Vec<(Context, Vec<RefValue>)>, Error> {
        Ok(vec![(update_gas(context, -1), vec![])])
    }

    fn exec(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        inputs: Vec<Vec<i64>>,
    ) -> Result<(Vec<Vec<i64>>, usize), Error> {
        validate_mem_sizes(&inputs, [])?;
        Ok((vec![], 0))
    }
}

pub(super) fn extensions() -> [(String, ExtensionBox); 1] {
    [("jump".to_string(), Box::new(UnconditionalJumpExtension {}))]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::val_arg;

    #[test]
    fn legal_usage() {
        assert_eq!(
            UnconditionalJumpExtension {}.get_signature(&vec![]),
            Ok(ExtensionSignature {
                args: vec![],
                results: vec![vec![]],
                fallthrough: None,
            })
        );
    }

    #[test]
    fn wrong_num_of_args() {
        assert_eq!(
            UnconditionalJumpExtension {}.get_signature(&vec![val_arg(1)]),
            Err(Error::WrongNumberOfTypeArgs)
        );
    }
}
