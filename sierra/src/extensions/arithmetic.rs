use crate::{
    extensions::*,
    utils::{as_deferred, gas_type},
};

struct ArithmeticExtension {}

impl ExtensionImplementation for ArithmeticExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        match tmpl_args.len() {
            1 => {
                let numeric_type = get_numeric_type(&tmpl_args[0])?;
                Ok(simple_invoke_ext_sign(
                    vec![numeric_type.clone(), numeric_type.clone()],
                    vec![as_deferred(numeric_type.clone())],
                ))
            }
            2 => {
                let (numeric_type, _) = get_type_value(tmpl_args)?;
                Ok(simple_invoke_ext_sign(
                    vec![numeric_type.clone()],
                    vec![as_deferred(numeric_type.clone())],
                ))
            }
            _ => Err(Error::WrongNumberOfTypeArgs),
        }
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        mem_state: MemState,
        _args_state: &Vec<Location>,
    ) -> Result<(MemState, Vec<Vec<Location>>), Error> {
        Ok((mem_state, vec![vec![Location::Transient]]))
    }
}

struct DuplicateExtension {}

impl ExtensionImplementation for DuplicateExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        let numeric_type = get_type(tmpl_args)?;
        Ok(simple_invoke_ext_sign(
            vec![numeric_type.clone()],
            vec![numeric_type.clone(), numeric_type.clone()],
        ))
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        mem_state: MemState,
        args_state: &Vec<Location>,
    ) -> Result<(MemState, Vec<Vec<Location>>), Error> {
        Ok((
            mem_state,
            vec![vec![args_state[0].clone(), args_state[0].clone()]],
        ))
    }
}

struct ConstantExtension {}

impl ExtensionImplementation for ConstantExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        if tmpl_args.len() != 2 {
            return Err(Error::WrongNumberOfTypeArgs);
        }
        let (numeric_type, _) = get_type_value(tmpl_args)?;
        Ok(simple_invoke_ext_sign(
            vec![],
            vec![as_deferred(numeric_type.clone())],
        ))
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        mem_state: MemState,
        _args_state: &Vec<Location>,
    ) -> Result<(MemState, Vec<Vec<Location>>), Error> {
        Ok((mem_state, vec![vec![Location::Transient]]))
    }
}

struct IgnoreExtension {}

impl ExtensionImplementation for IgnoreExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        let numeric_type = get_type(tmpl_args)?;
        Ok(simple_invoke_ext_sign(vec![numeric_type.clone()], vec![]))
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        mem_state: MemState,
        _args_state: &Vec<Location>,
    ) -> Result<(MemState, Vec<Vec<Location>>), Error> {
        Ok((mem_state, vec![vec![]]))
    }
}

struct EnactExtension {}

impl ExtensionImplementation for EnactExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        let (numeric_type, v) = get_type_value(tmpl_args)?;
        if v != 0 && v != 1 {
            return Err(Error::UnsupportedTypeArg);
        }
        Ok(simple_invoke_ext_sign(
            vec![as_deferred(numeric_type.clone()), gas_type(1)],
            vec![numeric_type.clone()],
        ))
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        registry: &TypeRegistry,
        mut mem_state: MemState,
        _args_state: &Vec<Location>,
    ) -> Result<(MemState, Vec<Vec<Location>>), Error> {
        let (ty, v) = get_type_value(tmpl_args)?;
        let ti = get_info(registry, ty)?;
        let loc = match v {
            0 => {
                let prev = mem_state.temp_offset;
                mem_state.temp_offset += ti.size;
                Ok(Location::Temp(prev.try_into().unwrap()))
            }
            1 => {
                let prev = mem_state.local_offset;
                mem_state.local_offset += ti.size;
                Ok(Location::Local(prev.try_into().unwrap()))
            }
            _ => Err(Error::UnsupportedTypeArg),
        }?;
        Ok((mem_state, vec![vec![loc]]))
    }
}

fn get_numeric_type<'a>(arg: &'a TemplateArg) -> Result<&'a Type, Error> {
    match arg {
        TemplateArg::Type(t) if matches!(t.to_string().as_str(), "int" | "felt") => Ok(t),
        _ => Err(Error::UnsupportedTypeArg),
    }
}

fn get_type_value<'a>(tmpl_args: &'a Vec<TemplateArg>) -> Result<(&'a Type, i64), Error> {
    let numeric_type = get_numeric_type(&tmpl_args[0])?;
    match &tmpl_args[1] {
        TemplateArg::Value(v) => Ok((numeric_type, *v)),
        _ => Err(Error::UnsupportedTypeArg),
    }
}

fn get_type<'a>(tmpl_args: &'a Vec<TemplateArg>) -> Result<&'a Type, Error> {
    if tmpl_args.len() != 1 {
        return Err(Error::WrongNumberOfTypeArgs);
    }
    get_numeric_type(&tmpl_args[0])
}

struct ArithmeticTypeInfo {}

impl TypeInfoImplementation for ArithmeticTypeInfo {
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

pub(super) fn extensions() -> [(String, ExtensionBox); 8] {
    [
        ("add".to_string(), Box::new(ArithmeticExtension {})),
        ("sub".to_string(), Box::new(ArithmeticExtension {})),
        ("mul".to_string(), Box::new(ArithmeticExtension {})),
        ("div".to_string(), Box::new(ArithmeticExtension {})),
        ("duplicate_num".to_string(), Box::new(DuplicateExtension {})),
        ("constant_num".to_string(), Box::new(ConstantExtension {})),
        ("ignore_num".to_string(), Box::new(IgnoreExtension {})),
        ("enact_calc".to_string(), Box::new(EnactExtension {})),
    ]
}

pub(super) fn types() -> [(String, TypeInfoBox); 2] {
    [
        ("int".to_string(), Box::new(ArithmeticTypeInfo {})),
        ("felt".to_string(), Box::new(ArithmeticTypeInfo {})),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::utils::{as_type, type_arg, val_arg};

    #[test]
    fn legal_usage() {
        let ty = as_type("int");
        assert_eq!(
            ArithmeticExtension {}.get_signature(&vec![type_arg(ty.clone())]),
            Ok(simple_invoke_ext_sign(
                vec![ty.clone(), ty.clone()],
                vec![as_deferred(ty.clone())],
            ))
        );
        assert_eq!(
            ArithmeticExtension {}.get_signature(&vec![type_arg(ty.clone()), val_arg(1)],),
            Ok(simple_invoke_ext_sign(
                vec![ty.clone()],
                vec![as_deferred(ty.clone())],
            ))
        );
        assert_eq!(
            DuplicateExtension {}.get_signature(&vec![type_arg(ty.clone())]),
            Ok(simple_invoke_ext_sign(
                vec![ty.clone()],
                vec![ty.clone(), ty.clone()],
            ))
        );
        assert_eq!(
            ConstantExtension {}.get_signature(&vec![type_arg(ty.clone()), val_arg(1)],),
            Ok(simple_invoke_ext_sign(
                vec![],
                vec![as_deferred(ty.clone())],
            ))
        );
        assert_eq!(
            IgnoreExtension {}.get_signature(&vec![type_arg(ty.clone())]),
            Ok(simple_invoke_ext_sign(vec![ty.clone()], vec![]))
        );
        assert_eq!(
            EnactExtension {}.get_signature(&vec![type_arg(ty.clone()), val_arg(0)]),
            Ok(simple_invoke_ext_sign(
                vec![as_deferred(ty.clone()), gas_type(1)],
                vec![ty.clone()],
            ))
        );
        assert_eq!(
            EnactExtension {}.get_signature(&vec![type_arg(ty.clone()), val_arg(1)]),
            Ok(simple_invoke_ext_sign(
                vec![as_deferred(ty.clone()), gas_type(1)],
                vec![ty],
            ))
        );
    }

    #[test]
    fn wrong_num_of_args() {
        assert_eq!(
            ArithmeticExtension {}.get_signature(&vec![]),
            Err(Error::WrongNumberOfTypeArgs)
        );
        assert_eq!(
            ConstantExtension {}.get_signature(&vec![]),
            Err(Error::WrongNumberOfTypeArgs)
        );
        assert_eq!(
            DuplicateExtension {}.get_signature(&vec![]),
            Err(Error::WrongNumberOfTypeArgs)
        );
        assert_eq!(
            IgnoreExtension {}.get_signature(&vec![]),
            Err(Error::WrongNumberOfTypeArgs)
        );
    }

    #[test]
    fn wrong_arg_type() {
        assert_eq!(
            ArithmeticExtension {}.get_signature(&vec![type_arg(as_type("non-int"))]),
            Err(Error::UnsupportedTypeArg)
        );
        assert_eq!(
            ArithmeticExtension {}.get_signature(&vec![val_arg(1)]),
            Err(Error::UnsupportedTypeArg)
        );
    }
}
