use crate::{extensions::*, utils::as_nullable};

struct MatchNullableExtension {}

impl ExtensionImplementation for MatchNullableExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        let inner_type = single_type_arg(tmpl_args)?;
        Ok(ExtensionSignature {
            args: vec![as_nullable(inner_type.clone())],
            results: vec![vec![inner_type.clone()], vec![]],
            fallthrough: Some(1),
        })
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        context: Context,
        arg_refs: Vec<RefValue>,
    ) -> Result<Vec<(Context, Vec<RefValue>)>, Error> {
        let context = update_gas(context, -1);
        Ok(vec![
            (context.clone(), vec![arg_refs[0].clone()]),
            (context, vec![]),
        ])
    }

    fn exec(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        _inputs: Vec<Vec<i64>>,
    ) -> Result<(Vec<Vec<i64>>, usize), Error> {
        todo!("yet to decide on the structure of a general nullable.")
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
                args: vec![as_nullable(as_type("int"))],
                results: vec![vec![as_type("int")], vec![]],
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
