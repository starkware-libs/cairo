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
        if tmpl_args.is_empty() {
            return Err(Error::WrongNumberOfTypeArgs);
        }
        let mut success_types = vec![as_deferred(gas_builtin_type())];
        tmpl_args.iter().try_for_each(|tmpl_arg| match tmpl_arg {
            TemplateArg::Value(v) => {
                success_types.push(gas_type(*v));
                Ok(())
            }
            TemplateArg::Type(_) => Err(Error::UnsupportedTypeArg),
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
        mem_state: MemState,
        arg_locs: Vec<Location>,
    ) -> Result<Vec<(MemState, Vec<Location>)>, Error> {
        let mut total_gas = 0;
        let mut success_locs = vec![Location::Transient];
        tmpl_args.iter().try_for_each(|tmpl_arg| match tmpl_arg {
            TemplateArg::Value(v) if *v > 0 => {
                success_locs.push(Location::Transient);
                total_gas += *v;
                Ok(())
            }
            _ => Err(Error::UnsupportedTypeArg),
        })?;
        success_locs[0] = Location::AddConst(as_final(&arg_locs[0])?, total_gas);
        Ok(vec![
            (mem_state.clone(), success_locs),
            (mem_state, vec![arg_locs[0].clone()]),
        ])
    }
}

struct RefundGasExtension {}

impl ExtensionImplementation for RefundGasExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        if tmpl_args.len() != 1 {
            return Err(Error::WrongNumberOfTypeArgs);
        }
        let value = match &tmpl_args[0] {
            TemplateArg::Value(v) => Ok(*v),
            TemplateArg::Type(_) => Err(Error::UnsupportedTypeArg),
        }?;
        Ok(simple_invoke_ext_sign(
            vec![gas_builtin_type(), gas_type(value)],
            vec![as_deferred(gas_builtin_type())],
        ))
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        mem_state: MemState,
        arg_locs: Vec<Location>,
    ) -> Result<Vec<(MemState, Vec<Location>)>, Error> {
        if tmpl_args.len() != 1 {
            return Err(Error::WrongNumberOfTypeArgs);
        }
        let value = match &tmpl_args[0] {
            TemplateArg::Value(v) => Ok(*v),
            TemplateArg::Type(_) => Err(Error::UnsupportedTypeArg),
        }?;
        Ok(vec![(
            mem_state,
            vec![Location::AddConst(as_final(&arg_locs[0])?, -value)],
        )])
    }
}

struct SplitGasExtension {}

impl ExtensionImplementation for SplitGasExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        if tmpl_args.len() <= 1 {
            return Err(Error::WrongNumberOfTypeArgs);
        }
        let mut res_types = vec![];
        let mut total = 0;
        tmpl_args.iter().try_for_each(|tmpl_arg| match tmpl_arg {
            TemplateArg::Value(v) => {
                res_types.push(gas_type(*v));
                total += v;
                Ok(())
            }
            TemplateArg::Type(_) => Err(Error::UnsupportedTypeArg),
        })?;
        Ok(simple_invoke_ext_sign(vec![gas_type(total)], res_types))
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        mem_state: MemState,
        _arg_locs: Vec<Location>,
    ) -> Result<Vec<(MemState, Vec<Location>)>, Error> {
        Ok(vec![(
            mem_state,
            tmpl_args.iter().map(|_| Location::Transient).collect(),
        )])
    }
}

pub(super) fn extensions() -> [(String, ExtensionBox); 3] {
    [
        ("get_gas".to_string(), Box::new(GetGasExtension {})),
        ("refund_gas".to_string(), Box::new(RefundGasExtension {})),
        ("split_gas".to_string(), Box::new(SplitGasExtension {})),
    ]
}

struct GasBuiltinTypeInfo {}

impl TypeInfoImplementation for GasBuiltinTypeInfo {
    fn get_info(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        _: &TypeRegistry,
    ) -> Result<TypeInfo, Error> {
        if !tmpl_args.is_empty() {
            return Err(Error::WrongNumberOfTypeArgs);
        }
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
        if tmpl_args.len() != 1 {
            return Err(Error::WrongNumberOfTypeArgs);
        }
        match &tmpl_args[0] {
            TemplateArg::Type(_) => {
                return Err(Error::UnsupportedTypeArg);
            }
            TemplateArg::Value(_) => {}
        }
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
            Ok(simple_invoke_ext_sign(
                vec![gas_builtin_type(), gas_type(5)],
                vec![as_deferred(gas_builtin_type())],
            ))
        );
        assert_eq!(
            SplitGasExtension {}.get_signature(&vec![val_arg(1), val_arg(2)]),
            Ok(simple_invoke_ext_sign(
                vec![gas_type(3)],
                vec![gas_type(1), gas_type(2)],
            ))
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
