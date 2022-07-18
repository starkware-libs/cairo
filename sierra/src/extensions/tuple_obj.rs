use crate::{extensions::*, utils::as_tuple};

struct TuplePackExtension {}

impl ExtensionImplementation for TuplePackExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        let mut arg_types = vec![];
        tmpl_args.iter().try_for_each(|tmpl_arg| match tmpl_arg {
            TemplateArg::Type(t) => {
                arg_types.push(t.clone());
                Ok(())
            }
            TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
        })?;
        Ok(simple_invoke_ext_sign(
            arg_types,
            vec![as_tuple(tmpl_args.clone())],
        ))
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        registry: &TypeRegistry,
        context: Context,
        arg_refs: Vec<RefValue>,
    ) -> Result<Vec<(Context, Vec<RefValue>)>, Error> {
        let mut tup_mem: Option<(MemLocation, usize)> = None;
        for (ref_val, tmpl_arg) in arg_refs.iter().zip(tmpl_args.iter()) {
            let size = match tmpl_arg {
                TemplateArg::Type(t) => {
                    let ti = get_info(registry, t)?;
                    Ok(ti.size)
                }
                TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
            }?;
            if size == 0 {
                continue;
            }
            tup_mem = Some(match tup_mem {
                None => Ok((as_final(ref_val)?, size)),
                Some(prev) => mem_reducer(prev, (as_final(ref_val)?, size))
                    .ok_or(Error::LocationsNonCosecutive),
            }?);
        }
        Ok(vec![(
            context,
            vec![match tup_mem {
                None => RefValue::Transient,
                Some((mem, _)) => RefValue::Final(mem),
            }],
        )])
    }
}

struct TupleUnpackExtension {}

impl ExtensionImplementation for TupleUnpackExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        let mut arg_types = vec![];
        tmpl_args.iter().try_for_each(|tmpl_arg| match tmpl_arg {
            TemplateArg::Type(t) => {
                arg_types.push(t.clone());
                Ok(())
            }
            TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
        })?;
        Ok(simple_invoke_ext_sign(
            vec![as_tuple(tmpl_args.clone())],
            arg_types,
        ))
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        registry: &TypeRegistry,
        context: Context,
        arg_refs: Vec<RefValue>,
    ) -> Result<Vec<(Context, Vec<RefValue>)>, Error> {
        let mut refs = vec![];
        let mut offset = 0;
        tmpl_args.iter().try_for_each(|tmpl_arg| match tmpl_arg {
            TemplateArg::Type(t) => {
                let ti = get_info(registry, t)?;
                refs.push(if ti.size == 0 {
                    Ok(RefValue::Transient)
                } else {
                    match &arg_refs[0] {
                        RefValue::Final(MemLocation::Temp(base)) => {
                            Ok(RefValue::Final(MemLocation::Temp(base + offset)))
                        }
                        RefValue::Final(MemLocation::Local(base)) => {
                            Ok(RefValue::Final(MemLocation::Local(base + offset)))
                        }
                        _ => Err(Error::IllegalArgsLocation),
                    }
                }?);
                offset += ti.size as i64;
                Ok(())
            }
            TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
        })?;
        Ok(vec![(context, refs)])
    }
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

    #[test]
    fn legal_usage() {
        assert_eq!(
            TuplePackExtension {}
                .get_signature(&vec![type_arg(as_type("1")), type_arg(as_type("2"))],),
            Ok(simple_invoke_ext_sign(
                vec![as_type("1"), as_type("2")],
                vec![as_tuple(vec![
                    type_arg(as_type("1")),
                    type_arg(as_type("2"))
                ])]
            ))
        );
        assert_eq!(
            TupleUnpackExtension {}
                .get_signature(&vec![type_arg(as_type("1")), type_arg(as_type("2"))]),
            Ok(simple_invoke_ext_sign(
                vec![as_tuple(vec![
                    type_arg(as_type("1")),
                    type_arg(as_type("2"))
                ])],
                vec![as_type("1"), as_type("2")]
            ))
        );
    }

    #[test]
    fn wrong_arg_type() {
        assert_eq!(
            TuplePackExtension {}.get_signature(&vec![val_arg(1)]),
            Err(Error::UnsupportedTypeArg)
        );
        assert_eq!(
            TupleUnpackExtension {}.get_signature(&vec![val_arg(1)]),
            Err(Error::UnsupportedTypeArg)
        );
    }
}
