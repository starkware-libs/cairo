use crate::{
    extensions::*,
    utils::{as_nonzero, gas_type},
};

struct JumpNzExtension {}

impl ExtensionImplementation for JumpNzExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        let numeric_type = single_type_arg(tmpl_args)?;
        Ok(ExtensionSignature {
            args: vec![numeric_type.clone(), gas_type(1)],
            results: vec![vec![as_nonzero(numeric_type.clone())], vec![]],
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
        Ok(vec![
            (context.clone(), vec![arg_refs[0].clone()]),
            (context, vec![]),
        ])
    }
}

struct UnwrapNzExtension {}

impl ExtensionImplementation for UnwrapNzExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        let numeric_type = single_type_arg(tmpl_args)?;
        Ok(simple_invoke_ext_sign(
            vec![as_nonzero(numeric_type.clone())],
            vec![numeric_type.clone()],
        ))
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        context: Context,
        arg_refs: Vec<RefValue>,
    ) -> Result<Vec<(Context, Vec<RefValue>)>, Error> {
        Ok(vec![(context, arg_refs)])
    }
}

struct NonZeroTypeInfo {}

impl TypeInfoImplementation for NonZeroTypeInfo {
    fn get_info(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        registry: &TypeRegistry,
    ) -> Result<TypeInfo, Error> {
        let numeric_type = single_type_arg(tmpl_args)?;
        let ti = get_info(registry, numeric_type)?;
        Ok(TypeInfo { size: ti.size })
    }
}

pub(super) fn extensions() -> [(String, ExtensionBox); 2] {
    [
        ("jump_nz".to_string(), Box::new(JumpNzExtension {})),
        ("unwrap_nz".to_string(), Box::new(UnwrapNzExtension {})),
    ]
}

pub(super) fn types() -> [(String, TypeInfoBox); 1] {
    [("NonZero".to_string(), Box::new(NonZeroTypeInfo {}))]
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
                results: vec![vec![as_nonzero(as_type("int"))], vec![]],
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
