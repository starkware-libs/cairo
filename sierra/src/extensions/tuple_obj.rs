use crate::{extensions::*, utils::as_tuple};

struct TuplePackExtension {}

impl ExtensionImplementation for TuplePackExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        _: &TypeRegistry,
    ) -> Result<ExtensionSignature, Error> {
        let arg_types = extract_types(tmpl_args)?;
        Ok(simple_invoke_ext_sign(
            arg_types,
            vec![(as_tuple(tmpl_args.clone()), ResLoc::ArgRef(0))],
        ))
    }
}

struct TupleUnpackExtension {}

impl ExtensionImplementation for TupleUnpackExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        registry: &TypeRegistry,
    ) -> Result<ExtensionSignature, Error> {
        let mut mem_idx = 0;
        let mut results = vec![];
        tmpl_args.iter().try_for_each(|tmpl_arg| match tmpl_arg {
            TemplateArg::Type(t) => {
                results.push((t.clone(), ResLoc::ArgRef(mem_idx)));
                let ti = get_info(registry, t)?;
                mem_idx += ti.size;
                Ok(())
            }
            TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
        })?;
        Ok(simple_invoke_ext_sign(
            vec![as_tuple(tmpl_args.clone())],
            results,
        ))
    }
}

fn extract_types(tmpl_args: &Vec<TemplateArg>) -> Result<Vec<Type>, Error> {
    let mut result = vec![];
    tmpl_args.iter().try_for_each(|tmpl_arg| match tmpl_arg {
        TemplateArg::Type(t) => {
            result.push(t.clone());
            Ok(())
        }
        TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
    })?;
    Ok(result)
}

struct TupleTypeInfo {}

impl TypeInfoImplementation for TupleTypeInfo {
    fn get_info(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        registry: &TypeRegistry,
    ) -> Result<TypeInfo, Error> {
        let mut size = 0;
        tmpl_args.iter().try_for_each(|tmpl_arg| match tmpl_arg {
            TemplateArg::Type(t) => {
                let ti = get_info(registry, t)?;
                size += ti.size;
                Ok(())
            }
            TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
        })?;
        Ok(TypeInfo { size: size })
    }
}

pub(super) fn extensions() -> [(String, ExtensionBox); 2] {
    [
        ("tuple_pack".to_string(), Box::new(TuplePackExtension {})),
        (
            "tuple_unpack".to_string(),
            Box::new(TupleUnpackExtension {}),
        ),
    ]
}

pub(super) fn types() -> [(String, TypeInfoBox); 1] {
    [("Tuple".to_string(), Box::new(TupleTypeInfo {}))]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::{as_type, type_arg, val_arg};

    struct FakeTypeInfo(pub usize);

    impl TypeInfoImplementation for FakeTypeInfo {
        fn get_info(
            self: &Self,
            tmpl_args: &Vec<TemplateArg>,
            _: &TypeRegistry,
        ) -> Result<TypeInfo, Error> {
            if !tmpl_args.is_empty() {
                return Err(Error::WrongNumberOfTypeArgs);
            }
            Ok(TypeInfo { size: self.0 })
        }
    }

    #[test]
    fn legal_usage() {
        assert_eq!(
            TuplePackExtension {}.get_signature(
                &vec![type_arg(as_type("1")), type_arg(as_type("2"))],
                &TypeRegistry::new()
            ),
            Ok(simple_invoke_ext_sign(
                vec![as_type("1"), as_type("2")],
                vec![(
                    as_tuple(vec![type_arg(as_type("1")), type_arg(as_type("2"))]),
                    ResLoc::ArgRef(0)
                )]
            ))
        );
        assert_eq!(
            TupleUnpackExtension {}.get_signature(
                &vec![type_arg(as_type("1")), type_arg(as_type("2"))],
                &TypeRegistry::from([
                    ("1".to_string(), Box::new(FakeTypeInfo(1)) as TypeInfoBox),
                    ("2".to_string(), Box::new(FakeTypeInfo(1)) as TypeInfoBox)
                ])
            ),
            Ok(simple_invoke_ext_sign(
                vec![as_tuple(vec![
                    type_arg(as_type("1")),
                    type_arg(as_type("2"))
                ])],
                vec![
                    (as_type("1"), ResLoc::ArgRef(0)),
                    (as_type("2"), ResLoc::ArgRef(1))
                ]
            ))
        );
    }

    #[test]
    fn wrong_arg_type() {
        assert_eq!(
            TuplePackExtension {}.get_signature(&vec![val_arg(1)], &TypeRegistry::new()),
            Err(Error::UnsupportedTypeArg)
        );
        assert_eq!(
            TupleUnpackExtension {}.get_signature(&vec![val_arg(1)], &TypeRegistry::new()),
            Err(Error::UnsupportedTypeArg)
        );
    }
}
