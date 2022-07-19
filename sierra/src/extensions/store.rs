use crate::{
    extensions::*,
    utils::{as_deferred, gas_type},
};

enum StoreType {
    Temp,
    Local,
}
fn unpack_args<'a>(tmpl_args: &'a Vec<TemplateArg>) -> Result<(StoreType, &'a Type), Error> {
    validate_size_eq(tmpl_args, 2)?;
    Ok((
        match unwrap_type(&tmpl_args[0])? {
            Type { name: n, args: a } if n.as_str() == "Temp" && a.is_empty() => {
                Ok(StoreType::Temp)
            }
            Type { name: n, args: a } if n.as_str() == "Local" && a.is_empty() => {
                Ok(StoreType::Local)
            }
            _ => Err(Error::UnsupportedTypeArg),
        }?,
        unwrap_type(&tmpl_args[1])?,
    ))
}

struct StoreExtension {}

impl NonBranchImplementation for StoreExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Type>), Error> {
        let (_, ty) = unpack_args(tmpl_args)?;
        Ok((vec![as_deferred(ty.clone()), gas_type(1)], vec![ty.clone()]))
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        registry: &TypeRegistry,
        mut context: Context,
        _arg_refs: Vec<RefValue>,
    ) -> Result<(Context, Vec<RefValue>), Error> {
        let (store_ty, ty) = unpack_args(tmpl_args)?;
        let ti = get_info(registry, ty)?;
        let loc = match store_ty {
            StoreType::Temp => {
                let prev = context.temp_cursur as i64;
                context.temp_used = true;
                context.temp_cursur += ti.size;
                Ok(MemLocation::Temp(prev))
            }
            StoreType::Local => {
                let prev = context.local_cursur as i64;
                context.local_cursur += ti.size;
                if context.local_allocated {
                    Ok(MemLocation::Local(prev))
                } else {
                    Err(Error::LocalMemoryNotAllocated)
                }
            }
        }?;
        Ok((context, vec![RefValue::Final(loc)]))
    }

    fn exec(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<Vec<i64>>, Error> {
        validate_mem_sizes(&inputs, [1, 0])?;
        Ok(inputs)
    }
}

struct RenameExtension {}

impl NonBranchImplementation for RenameExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Type>), Error> {
        let ty = single_type_arg(tmpl_args)?;
        Ok((vec![ty.clone()], vec![ty.clone()]))
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        context: Context,
        arg_refs: Vec<RefValue>,
    ) -> Result<(Context, Vec<RefValue>), Error> {
        Ok((context, arg_refs))
    }

    fn exec(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<Vec<i64>>, Error> {
        Ok(inputs)
    }
}

struct MoveExtension {}

impl NonBranchImplementation for MoveExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Type>), Error> {
        let ty = single_type_arg(tmpl_args)?;
        Ok((vec![ty.clone()], vec![as_deferred(ty.clone())]))
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        context: Context,
        arg_refs: Vec<RefValue>,
    ) -> Result<(Context, Vec<RefValue>), Error> {
        Ok((
            context,
            vec![RefValue::OpWithConst(as_final(&arg_refs[0])?, Op::Add, 0)],
        ))
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

struct AllocLocalsExtension {}

impl NonBranchImplementation for AllocLocalsExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Type>), Error> {
        validate_size_eq(tmpl_args, 0)?;
        Ok((vec![gas_type(1)], vec![]))
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        mut context: Context,
        _arg_refs: Vec<RefValue>,
    ) -> Result<(Context, Vec<RefValue>), Error> {
        if context.local_allocated {
            Err(Error::LocalMemoryAlreadyAllocated)
        } else if context.temp_used {
            Err(Error::LocalMemoryCantBeAllocated)
        } else {
            context.local_allocated = true;
            context.temp_used = true;
            Ok((context, vec![]))
        }
    }

    fn exec(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<Vec<i64>>, Error> {
        validate_mem_sizes(&inputs, [0])?;
        Ok(vec![])
    }
}

struct AlignTempsExtension {}

fn positive_value_arg(tmpl_args: &Vec<TemplateArg>) -> Result<usize, Error> {
    let v = single_value_arg(tmpl_args)?;
    if v > 0 {
        Ok(v as usize)
    } else {
        Err(Error::UnsupportedTypeArg)
    }
}

impl NonBranchImplementation for AlignTempsExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Type>), Error> {
        positive_value_arg(tmpl_args)?;
        Ok((vec![gas_type(1)], vec![]))
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        mut context: Context,
        _arg_refs: Vec<RefValue>,
    ) -> Result<(Context, Vec<RefValue>), Error> {
        context.temp_cursur += positive_value_arg(tmpl_args)?;
        context.temp_used = true;
        Ok((context, vec![]))
    }

    fn exec(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<Vec<i64>>, Error> {
        validate_mem_sizes(&inputs, [0])?;
        Ok(vec![])
    }
}

pub(super) fn extensions() -> [(String, ExtensionBox); 5] {
    [
        (
            "store".to_string(),
            wrap_non_branch(Box::new(StoreExtension {})),
        ),
        (
            "rename".to_string(),
            wrap_non_branch(Box::new(RenameExtension {})),
        ),
        (
            "move".to_string(),
            wrap_non_branch(Box::new(MoveExtension {})),
        ),
        (
            "alloc_locals".to_string(),
            wrap_non_branch(Box::new(AllocLocalsExtension {})),
        ),
        (
            "align_temps".to_string(),
            wrap_non_branch(Box::new(AlignTempsExtension {})),
        ),
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
            Ok((vec![as_deferred(ty.clone()), gas_type(1)], vec![ty.clone()],))
        );
        assert_eq!(
            StoreExtension {}
                .get_signature(&vec![type_arg(as_type("Local")), type_arg(ty.clone())]),
            Ok((vec![as_deferred(ty.clone()), gas_type(1)], vec![ty],))
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
