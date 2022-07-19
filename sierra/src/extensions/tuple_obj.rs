use crate::{extensions::*, utils::as_tuple};

struct TuplePackExtension {}

impl NonBranchImplementation for TuplePackExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Type>), Error> {
        let mut arg_types = vec![];
        for tmpl_arg in tmpl_args {
            arg_types.push(unwrap_type(tmpl_arg)?.clone());
        }
        Ok((arg_types, vec![as_tuple(tmpl_args.clone())]))
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        registry: &TypeRegistry,
        context: Context,
        arg_refs: Vec<RefValue>,
    ) -> Result<(Context, Vec<RefValue>), Error> {
        let mut tup_mem: Option<(MemLocation, usize)> = None;
        for (ref_val, tmpl_arg) in arg_refs.iter().zip(tmpl_args.iter()) {
            let size = get_info(registry, unwrap_type(tmpl_arg)?)?.size;
            if size == 0 {
                continue;
            }
            tup_mem = Some(match tup_mem {
                None => Ok((as_final(ref_val)?, size)),
                Some(prev) => mem_reducer(prev, (as_final(ref_val)?, size))
                    .ok_or(Error::LocationsNonCosecutive),
            }?);
        }
        Ok((
            context,
            vec![match tup_mem {
                None => RefValue::Transient,
                Some((mem, _)) => RefValue::Final(mem),
            }],
        ))
    }
}

struct TupleUnpackExtension {}

impl NonBranchImplementation for TupleUnpackExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Type>), Error> {
        let mut arg_types = vec![];
        for tmpl_arg in tmpl_args {
            arg_types.push(unwrap_type(tmpl_arg)?.clone());
        }
        Ok((vec![as_tuple(tmpl_args.clone())], arg_types))
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        registry: &TypeRegistry,
        context: Context,
        arg_refs: Vec<RefValue>,
    ) -> Result<(Context, Vec<RefValue>), Error> {
        let mut refs = vec![];
        let mut offset = 0;
        for tmpl_arg in tmpl_args {
            let size = get_info(registry, unwrap_type(tmpl_arg)?)?.size;
            refs.push(if size == 0 {
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
            offset += size as i64;
        }
        Ok((context, refs))
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
        for tmpl_arg in tmpl_args {
            let ti = get_info(registry, unwrap_type(tmpl_arg)?)?;
            size += ti.size;
        }
        Ok(TypeInfo { size: size })
    }
}

pub(super) fn extensions() -> [(String, ExtensionBox); 2] {
    [
        (
            "tuple_pack".to_string(),
            wrap_non_branch(Box::new(TuplePackExtension {})),
        ),
        (
            "tuple_unpack".to_string(),
            wrap_non_branch(Box::new(TupleUnpackExtension {})),
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
            Ok((
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
            Ok((
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
