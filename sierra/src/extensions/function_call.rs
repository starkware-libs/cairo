use crate::{
    extensions::*,
    utils::{as_tuple, gas_type, type_arg},
};

struct FunctionCallExtension {
    pub args: Vec<Type>,
    pub results: Vec<Type>,
}

fn types_as_tuple(tys: &Vec<Type>) -> Type {
    as_tuple(tys.iter().map(|t| type_arg(t.clone())).collect())
}

impl NonBranchImplementation for FunctionCallExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Type>), Error> {
        validate_size_eq(tmpl_args, 0)?;
        Ok((
            vec![types_as_tuple(&self.args), gas_type(2)],
            vec![types_as_tuple(&self.results)],
        ))
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        registry: &TypeRegistry,
        mut context: Context,
        arg_refs: Vec<RefValue>,
    ) -> Result<(Context, Vec<RefValue>), Error> {
        validate_size_eq(tmpl_args, 0)?;
        let ti = get_info(registry, &types_as_tuple(&self.args))?;
        match &arg_refs[0] {
            RefValue::Final(MemLocation::Temp(offset))
                if offset + ti.size as i64 == context.temp_cursur as i64 => {}
            _ => {
                return Err(Error::IllegalArgsLocation);
            }
        }
        let ti = get_info(registry, &types_as_tuple(&self.results))?;
        context.temp_cursur = 0;
        context.temp_invalidated = true;
        Ok((
            context,
            vec![RefValue::Final(MemLocation::Temp(-(ti.size as i64)))],
        ))
    }
}

pub(super) fn extensions(prog: &Program) -> Vec<(String, ExtensionBox)> {
    prog.funcs
        .iter()
        .map(|f| {
            (
                f.name.clone(),
                wrap_non_branch(Box::new(FunctionCallExtension {
                    args: f.args.iter().map(|v| v.ty.clone()).collect(),
                    results: f.res_types.clone(),
                })),
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
            Ok((vec![as_tuple(vec![]), gas_type(2)], vec![as_tuple(vec![])],))
        );
        assert_eq!(
            FunctionCallExtension {
                args: vec![as_type("1"), as_type("2")],
                results: vec![as_type("3"), as_type("4")]
            }
            .get_signature(&vec![]),
            Ok((
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
