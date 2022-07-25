use crate::{extensions::*, utils::as_nonzero};

struct JumpNzExtension {}

impl ExtensionImplementation for JumpNzExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        let numeric_type = single_type_arg(tmpl_args)?;
        Ok(ExtensionSignature {
            args: vec![numeric_type.clone()],
            results: vec![vec![as_nonzero(numeric_type.clone())], vec![]],
            fallthrough: Some(1),
        })
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        _cursors: &Cursors,
        arg_refs: Vec<RefValue>,
    ) -> Result<Vec<(Effects, Vec<RefValue>)>, Error> {
        Ok(vec![
            (gas_usage(1), vec![arg_refs[0].clone()]),
            (gas_usage(1), vec![]),
        ])
    }

    fn exec(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        mut inputs: Vec<Vec<i64>>,
    ) -> Result<(Vec<Vec<i64>>, usize), Error> {
        validate_mem_sizes(&inputs, [1])?;
        if inputs[0][0] != 0 {
            Ok((vec![inputs.remove(0)], 0))
        } else {
            Ok((vec![], 1))
        }
    }
}

struct UnwrapNzExtension {}

impl NonBranchImplementation for UnwrapNzExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Type>), Error> {
        let numeric_type = single_type_arg(tmpl_args)?;
        Ok((
            vec![as_nonzero(numeric_type.clone())],
            vec![numeric_type.clone()],
        ))
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        _cursors: &Cursors,
        arg_refs: Vec<RefValue>,
    ) -> Result<(Effects, Vec<RefValue>), Error> {
        Ok((Effects::none(), arg_refs))
    }

    fn exec(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<Vec<i64>>, Error> {
        validate_mem_sizes(&inputs, [1])?;
        Ok(inputs)
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
        (
            "unwrap_nz".to_string(),
            wrap_non_branch(Box::new(UnwrapNzExtension {})),
        ),
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
                args: vec![as_type("int")],
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
