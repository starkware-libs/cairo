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
        mem_state: MemState,
        arg_locs: Vec<Location>,
    ) -> Result<Vec<(MemState, Vec<Location>)>, Error> {
        let mut result: Option<&Location> = None;
        let mut expected_next: Option<Location> = None;
        tmpl_args
            .iter()
            .zip(arg_locs.iter())
            .try_for_each(|(tmpl_arg, loc)| match tmpl_arg {
                TemplateArg::Type(t) => {
                    let ti = get_info(registry, t)?;
                    if ti.size == 0 {
                        return Ok(());
                    }
                    //match &expected_next {
                    //    Some(exp) if loc != exp => {
                    //        return Err(Error::IllegalExtensionArgsLocation);
                    //    }
                    //    _ => {}
                    //}
                    if result.is_none() {
                        result = Some(loc);
                    }
                    expected_next = match loc {
                        Location::Temp(offset) => Ok(Some(Location::Temp(offset + ti.size as i64))),
                        Location::Local(offset) => {
                            Ok(Some(Location::Local(offset + ti.size as i64)))
                        }
                        // Location::Transient(_) => Err(Error::IllegalExtensionArgsLocation),
                        Location::Transient(_) => Ok(Some(Location::Local(0))),
                    }?;
                    Ok(())
                }
                TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
            })?;
        Ok(vec![(
            mem_state,
            vec![match result {
                None => Location::Transient(vec![]),
                Some(loc) => loc.clone(),
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
        mem_state: MemState,
        arg_locs: Vec<Location>,
    ) -> Result<Vec<(MemState, Vec<Location>)>, Error> {
        let mut locs = vec![];
        let mut offset = 0;
        tmpl_args.iter().try_for_each(|tmpl_arg| match tmpl_arg {
            TemplateArg::Type(t) => {
                let ti = get_info(registry, t)?;
                if ti.size == 0 {
                    return Ok(());
                }
                locs.push(match &arg_locs[0] {
                    Location::Temp(base) => Ok(Location::Temp(base + offset)),
                    Location::Local(base) => Ok(Location::Local(base + offset)),
                    Location::Transient(_) => Err(Error::IllegalExtensionArgsLocation),
                }?);
                offset += ti.size as i64;
                Ok(())
            }
            TemplateArg::Value(_) => Err(Error::UnsupportedTypeArg),
        })?;
        Ok(vec![(mem_state, locs)])
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
