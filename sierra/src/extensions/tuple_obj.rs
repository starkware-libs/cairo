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
        let mut start_loc: Option<&Location> = None;
        let mut combined_size = 0;
        for (loc, tmpl_arg) in arg_locs.iter().zip(tmpl_args.iter()) {
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
            match &start_loc {
                None => {
                    start_loc = Some(loc);
                }
                Some(Location::Final(MemLocation::Temp(start_offset))) => match loc {
                    Location::Final(MemLocation::Temp(next))
                        if *next == (start_offset + combined_size as i64) => {}
                    _ => {
                        return Err(Error::LocationsNonCosecutive);
                    }
                },
                Some(Location::Final(MemLocation::Local(start_offset))) => match loc {
                    Location::Final(MemLocation::Local(next))
                        if *next == (start_offset + combined_size as i64) => {}
                    _ => {
                        return Err(Error::LocationsNonCosecutive);
                    }
                },
                _ => {
                    return Err(Error::LocationsNonCosecutive);
                }
            }
            combined_size += size;
        }
        Ok(vec![(
            mem_state,
            vec![match start_loc {
                None => Location::Transient,
                Some(start_loc) => start_loc.clone(),
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
                locs.push(if ti.size == 0 {
                    Ok(Location::Transient)
                } else {
                    match &arg_locs[0] {
                        Location::Final(MemLocation::Temp(base)) => {
                            Ok(Location::Final(MemLocation::Temp(base + offset)))
                        }
                        Location::Final(MemLocation::Local(base)) => {
                            Ok(Location::Final(MemLocation::Local(base + offset)))
                        }
                        _ => Err(Error::IllegalExtensionArgsLocation),
                    }
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
