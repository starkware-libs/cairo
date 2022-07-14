use crate::{
    extensions::*,
    utils::{as_deferred, gas_type},
};

enum StoreType {
    Temp,
    Local,
}
fn unpack_args<'a>(tmpl_args: &'a Vec<TemplateArg>) -> Result<(StoreType, &'a Type), Error> {
    if tmpl_args.len() != 2 {
        return Err(Error::WrongNumberOfTypeArgs);
    }
    Ok((
        match &tmpl_args[0] {
            TemplateArg::Type(Type { name: n, args: a })
                if n.as_str() == "Temp" && a.is_empty() =>
            {
                Ok(StoreType::Temp)
            }
            TemplateArg::Type(Type { name: n, args: a })
                if n.as_str() == "Local" && a.is_empty() =>
            {
                Ok(StoreType::Local)
            }
            _ => Err(Error::UnsupportedTypeArg),
        }?,
        match &tmpl_args[1] {
            TemplateArg::Type(ty) => Ok(ty),
            _ => Err(Error::UnsupportedTypeArg),
        }?,
    ))
}

struct StoreExtension {}

impl ExtensionImplementation for StoreExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        let (_, ty) = unpack_args(tmpl_args)?;
        Ok(simple_invoke_ext_sign(
            vec![as_deferred(ty.clone()), gas_type(1)],
            vec![ty.clone()],
        ))
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        registry: &TypeRegistry,
        mut mem_state: MemState,
        _arg_refs: Vec<RefValue>,
    ) -> Result<Vec<(MemState, Vec<RefValue>)>, Error> {
        let (store_ty, ty) = unpack_args(tmpl_args)?;
        let ti = get_info(registry, ty)?;
        let loc = match store_ty {
            StoreType::Temp => {
                let prev = mem_state.temp_cursur as i64;
                mem_state.temp_used = true;
                mem_state.temp_cursur += ti.size;
                Ok(MemLocation::Temp(prev))
            }
            StoreType::Local => {
                let prev = mem_state.local_cursur as i64;
                mem_state.local_cursur += ti.size;
                if mem_state.local_allocated {
                    Ok(MemLocation::Local(prev))
                } else {
                    Err(Error::LocalMemoryNotAllocated)
                }
            }
        }?;
        Ok(vec![(mem_state, vec![RefValue::Final(loc)])])
    }
}

struct RenameExtension {}

impl ExtensionImplementation for RenameExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        if tmpl_args.len() != 1 {
            return Err(Error::WrongNumberOfTypeArgs);
        }
        let ty = match &tmpl_args[0] {
            TemplateArg::Type(ty) => Ok(ty),
            _ => Err(Error::UnsupportedTypeArg),
        }?;
        Ok(simple_invoke_ext_sign(vec![ty.clone()], vec![ty.clone()]))
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        mem_state: MemState,
        arg_refs: Vec<RefValue>,
    ) -> Result<Vec<(MemState, Vec<RefValue>)>, Error> {
        Ok(vec![(mem_state, arg_refs)])
    }
}

struct MoveExtension {}

impl ExtensionImplementation for MoveExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        if tmpl_args.len() != 1 {
            return Err(Error::WrongNumberOfTypeArgs);
        }
        let ty = match &tmpl_args[0] {
            TemplateArg::Type(ty) => Ok(ty),
            _ => Err(Error::UnsupportedTypeArg),
        }?;
        Ok(simple_invoke_ext_sign(
            vec![ty.clone()],
            vec![as_deferred(ty.clone())],
        ))
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        mem_state: MemState,
        arg_refs: Vec<RefValue>,
    ) -> Result<Vec<(MemState, Vec<RefValue>)>, Error> {
        Ok(vec![(
            mem_state,
            vec![RefValue::OpWithConst(as_final(&arg_refs[0])?, Op::Add, 0)],
        )])
    }
}

struct AllocLocalsExtension {}

impl ExtensionImplementation for AllocLocalsExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        if !tmpl_args.is_empty() {
            Err(Error::WrongNumberOfTypeArgs)
        } else {
            Ok(simple_invoke_ext_sign(vec![gas_type(1)], vec![]))
        }
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        mut mem_state: MemState,
        _arg_refs: Vec<RefValue>,
    ) -> Result<Vec<(MemState, Vec<RefValue>)>, Error> {
        if mem_state.local_allocated {
            Err(Error::LocalMemoryAlreadyAllocated)
        } else if mem_state.temp_used {
            Err(Error::LocalMemoryCantBeAllocated)
        } else {
            mem_state.local_allocated = true;
            mem_state.temp_used = true;
            Ok(vec![(mem_state, vec![])])
        }
    }
}

struct AlignTempsExtension {}

fn value_arg(tmpl_args: &Vec<TemplateArg>) -> Result<usize, Error> {
    if tmpl_args.len() != 1 {
        Err(Error::WrongNumberOfTypeArgs)
    } else {
        match &tmpl_args[0] {
            TemplateArg::Value(v) if *v > 0 => Ok(*v as usize),
            _ => Err(Error::UnsupportedTypeArg),
        }
    }
}

impl ExtensionImplementation for AlignTempsExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        value_arg(tmpl_args)?;
        Ok(simple_invoke_ext_sign(vec![gas_type(1)], vec![]))
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        mut mem_state: MemState,
        _arg_refs: Vec<RefValue>,
    ) -> Result<Vec<(MemState, Vec<RefValue>)>, Error> {
        mem_state.temp_cursur += value_arg(tmpl_args)?;
        mem_state.temp_used = true;
        Ok(vec![(mem_state, vec![])])
    }
}

pub(super) fn extensions() -> [(String, ExtensionBox); 5] {
    [
        ("store".to_string(), Box::new(StoreExtension {})),
        ("rename".to_string(), Box::new(RenameExtension {})),
        ("move".to_string(), Box::new(MoveExtension {})),
        (
            "alloc_locals".to_string(),
            Box::new(AllocLocalsExtension {}),
        ),
        ("align_temps".to_string(), Box::new(AlignTempsExtension {})),
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
            StoreExtension {}.get_signature(&vec![type_arg(as_type("Temp")), type_arg(ty.clone())]),
            Ok(simple_invoke_ext_sign(
                vec![as_deferred(ty.clone()), gas_type(1)],
                vec![ty.clone()],
            ))
        );
        assert_eq!(
            StoreExtension {}
                .get_signature(&vec![type_arg(as_type("Local")), type_arg(ty.clone())]),
            Ok(simple_invoke_ext_sign(
                vec![as_deferred(ty.clone()), gas_type(1)],
                vec![ty],
            ))
        );
    }

    #[test]
    fn wrong_num_of_args() {
        assert_eq!(
            StoreExtension {}.get_signature(&vec![]),
            Err(Error::WrongNumberOfTypeArgs)
        );
    }

    #[test]
    fn wrong_arg_type() {
        assert_eq!(
            StoreExtension {}
                .get_signature(&vec![type_arg(as_type("Other")), type_arg(as_type("Some"))]),
            Err(Error::UnsupportedTypeArg)
        );
        assert_eq!(
            StoreExtension {}.get_signature(&vec![val_arg(1), val_arg(1)]),
            Err(Error::UnsupportedTypeArg)
        );
    }
}
