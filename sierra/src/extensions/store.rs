use crate::{extensions::*, utils::as_deferred};

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
        Ok((vec![as_deferred(ty.clone())], vec![ty.clone()]))
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        registry: &TypeRegistry,
        cursors: &Cursors,
        _arg_refs: Vec<RefValue>,
    ) -> Result<(Effects, Vec<RefValue>), Error> {
        let mut effects = gas_usage(1);
        let (store_ty, ty) = unpack_args(tmpl_args)?;
        let ti = get_info(registry, ty)?;
        let loc = match store_ty {
            StoreType::Temp => {
                effects = effects
                    .add(&Effects::ap_change(ti.size))
                    .map_err(|e| Error::EffectsAdd(e))?;
                Ok(MemLocation::Temp(cursors.temp as i64))
            }
            StoreType::Local => {
                effects = effects
                    .add(&Effects::local_writes(ti.size))
                    .map_err(|e| Error::EffectsAdd(e))?;
                Ok(MemLocation::Local(cursors.local as i64))
            }
        }?;
        Ok((effects, vec![RefValue::Final(loc)]))
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
        _cursors: &Cursors,
        arg_refs: Vec<RefValue>,
    ) -> Result<(Effects, Vec<RefValue>), Error> {
        Ok((
            Effects::none(),
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
        Ok((vec![], vec![]))
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        _cursors: &Cursors,
        _arg_refs: Vec<RefValue>,
    ) -> Result<(Effects, Vec<RefValue>), Error> {
        Ok((
            Effects::allocate_locals()
                .add(&gas_usage(1))
                .map_err(|e| Error::EffectsAdd(e))?,
            vec![],
        ))
    }

    fn exec(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<Vec<i64>>, Error> {
        validate_mem_sizes(&inputs, [])?;
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
        Ok((vec![], vec![]))
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        _cursors: &Cursors,
        _arg_refs: Vec<RefValue>,
    ) -> Result<(Effects, Vec<RefValue>), Error> {
        Ok((
            gas_usage(1)
                .add(&Effects::ap_change(positive_value_arg(tmpl_args)?))
                .map_err(|e| Error::EffectsAdd(e))?,
            vec![],
        ))
    }

    fn exec(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<Vec<i64>>, Error> {
        validate_mem_sizes(&inputs, [])?;
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
            Ok((vec![as_deferred(ty.clone())], vec![ty.clone()],))
        );
        assert_eq!(
            StoreExtension {}
                .get_signature(&vec![type_arg(as_type("Local")), type_arg(ty.clone())]),
            Ok((vec![as_deferred(ty.clone())], vec![ty],))
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
