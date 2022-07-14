use crate::{extensions::*, utils::gas_type};

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
            args: vec![numeric_type.clone(), gas_type(1)],
            results: vec![vec![], vec![]],
            fallthrough: Some(1),
        })
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        mem_state: MemState,
        _arg_locs: Vec<Location>,
    ) -> Result<Vec<(MemState, Vec<Location>)>, Error> {
        Ok(vec![(mem_state.clone(), vec![]), (mem_state, vec![])])
    }
}

pub(super) fn extensions() -> [(String, ExtensionBox); 1] {
    [("jump_nz".to_string(), Box::new(JumpNzExtension {}))]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::{as_type, type_arg, val_arg};

    #[test]
    fn legal_usage() {
        assert_eq!(
            JumpNzExtension {}.get_signature(&vec![type_arg(as_type("int"))]),
            Ok(ExtensionSignature {
                args: vec![as_type("int"), gas_type(1)],
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
            JumpNzExtension {}.get_signature(&vec![val_arg(1)]),
            Err(Error::UnsupportedTypeArg)
        );
    }
}
