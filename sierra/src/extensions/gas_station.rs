use crate::{
    extensions::*,
    utils::{as_deferred, gas_builtin_type, gas_type},
};

struct GetGasExtension {}

impl ExtensionImplementation for GetGasExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        validate_size_ge(tmpl_args, 1)?;
        let mut success_types = vec![as_deferred(gas_builtin_type())];
        tmpl_args.iter().try_for_each(|tmpl_arg| {
            success_types.push(gas_type(unwrap_value(tmpl_arg)?));
            Ok(())
        })?;
        Ok(ExtensionSignature {
            args: vec![gas_builtin_type(), gas_type(1)],
            results: vec![success_types, vec![gas_builtin_type()]],
            fallthrough: Some(1),
        })
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        context: Context,
        arg_refs: Vec<RefValue>,
    ) -> Result<Vec<(Context, Vec<RefValue>)>, Error> {
        let mut total_gas = 0;
        let mut success_refs = vec![RefValue::Transient];
        tmpl_args.iter().try_for_each(|tmpl_arg| {
            let v = unwrap_value(tmpl_arg)?;
            if v <= 0 {
                Err(Error::UnsupportedTypeArg)
            } else {
                total_gas += v;
                success_refs.push(RefValue::Transient);
                Ok(())
            }
        })?;
        success_refs[0] = RefValue::OpWithConst(as_final(&arg_refs[0])?, Op::Add, total_gas);
        Ok(vec![
            (context.clone(), success_refs),
            (context, vec![arg_refs[0].clone()]),
        ])
    }
}

struct RefundGasExtension {}

impl NonBranchImplementation for RefundGasExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Type>), Error> {
        Ok((
            vec![gas_builtin_type(), gas_type(single_value_arg(tmpl_args)?)],
            vec![as_deferred(gas_builtin_type())],
        ))
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        context: Context,
        arg_refs: Vec<RefValue>,
    ) -> Result<(Context, Vec<RefValue>), Error> {
        Ok((
            context,
            vec![RefValue::OpWithConst(
                as_final(&arg_refs[0])?,
                Op::Sub,
                single_value_arg(tmpl_args)?,
            )],
        ))
    }
}

struct SplitGasExtension {}

impl NonBranchImplementation for SplitGasExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Type>), Error> {
        validate_size_ge(tmpl_args, 2)?;
        let mut res_types = vec![];
        let mut total = 0;
        tmpl_args.iter().try_for_each(|tmpl_arg| {
            let v = unwrap_value(tmpl_arg)?;
            res_types.push(gas_type(v));
            total += v;
            Ok(())
        })?;
        Ok((vec![gas_type(total)], res_types))
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        context: Context,
        _arg_refs: Vec<RefValue>,
    ) -> Result<(Context, Vec<RefValue>), Error> {
        Ok((
            context,
            tmpl_args.iter().map(|_| RefValue::Transient).collect(),
        ))
    }
}

pub(super) fn extensions() -> [(String, ExtensionBox); 3] {
    [
        ("get_gas".to_string(), Box::new(GetGasExtension {})),
        (
            "refund_gas".to_string(),
            wrap_non_branch(Box::new(RefundGasExtension {})),
        ),
        (
            "split_gas".to_string(),
            wrap_non_branch(Box::new(SplitGasExtension {})),
        ),
    ]
}

struct GasBuiltinTypeInfo {}

impl TypeInfoImplementation for GasBuiltinTypeInfo {
    fn get_info(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        _: &TypeRegistry,
    ) -> Result<TypeInfo, Error> {
        validate_size_eq(tmpl_args, 0)?;
        Ok(TypeInfo { size: 1 })
    }
}

struct GasTypeInfo {}

impl TypeInfoImplementation for GasTypeInfo {
    fn get_info(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        _: &TypeRegistry,
    ) -> Result<TypeInfo, Error> {
        single_value_arg(tmpl_args)?;
        Ok(TypeInfo { size: 0 })
    }
}

pub(super) fn types() -> [(String, TypeInfoBox); 2] {
    [
        (gas_builtin_type().name, Box::new(GasBuiltinTypeInfo {})),
        (gas_type(1).name, Box::new(GasTypeInfo {})),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::{type_arg, val_arg};

    #[test]
    fn legal_usage() {
        assert_eq!(
            GetGasExtension {}.get_signature(&vec![val_arg(1), val_arg(2)]),
            Ok(ExtensionSignature {
                args: vec![gas_builtin_type(), gas_type(1)],
                results: vec![
                    vec![as_deferred(gas_builtin_type()), gas_type(1), gas_type(2)],
                    vec![gas_builtin_type()]
                ],
                fallthrough: Some(1),
            })
        );
        assert_eq!(
            RefundGasExtension {}.get_signature(&vec![val_arg(5)]),
            Ok((
                vec![gas_builtin_type(), gas_type(5)],
                vec![as_deferred(gas_builtin_type())],
            ))
        );
        assert_eq!(
            SplitGasExtension {}.get_signature(&vec![val_arg(1), val_arg(2)]),
            Ok((vec![gas_type(3)], vec![gas_type(1), gas_type(2)],))
        );
    }

    #[test]
    fn wrong_num_of_args() {
        assert_eq!(
            GetGasExtension {}.get_signature(&vec![]),
            Err(Error::WrongNumberOfTypeArgs)
        );
        assert_eq!(
            RefundGasExtension {}.get_signature(&vec![]),
            Err(Error::WrongNumberOfTypeArgs)
        );
        assert_eq!(
            SplitGasExtension {}.get_signature(&vec![]),
            Err(Error::WrongNumberOfTypeArgs)
        );
        assert_eq!(
            SplitGasExtension {}.get_signature(&vec![val_arg(1)]),
            Err(Error::WrongNumberOfTypeArgs)
        );
    }

    #[test]
    fn wrong_arg_type() {
        assert_eq!(
            GetGasExtension {}.get_signature(&vec![type_arg(gas_type(1))]),
            Err(Error::UnsupportedTypeArg)
        );
        assert_eq!(
            RefundGasExtension {}.get_signature(&vec![type_arg(gas_type(1))]),
            Err(Error::UnsupportedTypeArg)
        );
        assert_eq!(
            SplitGasExtension {}.get_signature(&vec![val_arg(1), type_arg(gas_type(1))],),
            Err(Error::UnsupportedTypeArg)
        );
    }
}
