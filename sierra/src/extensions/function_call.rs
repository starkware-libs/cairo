use crate::{
    extensions::*,
    utils::{as_tuple, gas_type, type_arg},
};

struct FunctionCallExtension {
    pub args: Vec<Type>,
    pub results: Vec<Type>,
}

impl ExtensionImplementation for FunctionCallExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        if !tmpl_args.is_empty() {
            return Err(Error::WrongNumberOfTypeArgs);
        }
        Ok(simple_invoke_ext_sign(
            vec![
                as_tuple(self.args.iter().map(|t| type_arg(t.clone())).collect()),
                gas_type(2),
            ],
            vec![as_tuple(
                self.results.iter().map(|t| type_arg(t.clone())).collect(),
            )],
        ))
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        registry: &TypeRegistry,
        _mem_state: MemState,
        _arg_locs: Vec<Location>,
    ) -> Result<Vec<(MemState, Vec<Location>)>, Error> {
        //let ap_val = match mem_state.ap_change {
        //    ApChange::Unknown => Err(Error::IllegalApChangeValue),
        //    ApChange::Known(val) => Ok(val),
        //}?;
        //match arg_locs[0] {
        //    Location::Temp(offset) if offset == ap_val as i64 => Ok(()),
        //    _ => Err(Error::IllegalExtensionArgsLocation),
        //}?;
        let ti = get_info(registry, &as_tuple(tmpl_args.clone()))?;
        Ok(vec![(
            MemState {
                temp_cursur: ti.size,
                local_cursur: 0,
                ap_change: ApChange::Unknown,
            },
            vec![Location::Temp(-(ti.size as i64))],
        )])
    }
}

pub(super) fn extensions(prog: &Program) -> Vec<(String, ExtensionBox)> {
    prog.funcs
        .iter()
        .map(|f| {
            (
                f.name.clone(),
                Box::new(FunctionCallExtension {
                    args: f.args.iter().map(|v| v.ty.clone()).collect(),
                    results: f.res_types.clone(),
                }) as ExtensionBox,
            )
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::as_type;

    #[test]
    fn legal_usage() {
        assert_eq!(
            FunctionCallExtension {
                args: vec![],
                results: vec![]
            }
            .get_signature(&vec![]),
            Ok(simple_invoke_ext_sign(
                vec![as_tuple(vec![]), gas_type(2)],
                vec![as_tuple(vec![])],
            ))
        );
        assert_eq!(
            FunctionCallExtension {
                args: vec![as_type("1"), as_type("2")],
                results: vec![as_type("3"), as_type("4")]
            }
            .get_signature(&vec![]),
            Ok(simple_invoke_ext_sign(
                vec![
                    as_tuple(vec![type_arg(as_type("1")), type_arg(as_type("2"))]),
                    gas_type(2)
                ],
                vec![as_tuple(vec![
                    type_arg(as_type("3")),
                    type_arg(as_type("4"))
                ])],
            ))
        );
    }

    #[test]
    fn wrong_num_of_args() {
        assert_eq!(
            FunctionCallExtension {
                args: vec![],
                results: vec![]
            }
            .get_signature(&vec![type_arg(as_type("1"))]),
            Err(Error::WrongNumberOfTypeArgs)
        );
    }
}
