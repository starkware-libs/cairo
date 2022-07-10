use crate::{
    extensions::*,
    utils::{as_nullable, gas_type},
};

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
            args: vec![as_nullable(inner_type.clone()), gas_type(1)],
            results: vec![vec![(inner_type.clone(), vec![0])], vec![]],
            fallthrough: Some(1),
        })
    }
}

pub(super) fn extensions() -> [(String, ExtensionBox); 1] {
    [(
        "match_nullable".to_string(),
        Box::new(MatchNullableExtension {}),
    )]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::{as_type, type_arg, val_arg};

    #[test]
    fn legal_usage() {
        assert_eq!(
            MatchNullableExtension {}.get_signature(&vec![type_arg(as_type("int"))]),
            Ok(ExtensionSignature {
                args: vec![as_nullable(as_type("int")), gas_type(1)],
                results: vec![vec![(as_type("int"), vec![0])], vec![]],
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
            MatchNullableExtension {}.get_signature(&vec![val_arg(1)]),
            Err(Error::UnsupportedTypeArg)
        );
    }
}
