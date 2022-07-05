use crate::extensions::*;

struct GetGasExtension {}

fn gas_builtin_type() -> Type {
    Type::Basic("GasBuiltin".to_string())
}

fn gas_type(v: i64) -> Type {
    Type::Template("Gas".to_string(), vec![TemplateArg::Value(v)])
}

impl JumpExtension for GetGasExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Vec<Type>>), Error> {
        if tmpl_args.is_empty() {
            return Err(Error::WrongNumberOfTypeArgs);
        }
        let mut success_types = vec![gas_builtin_type()];
        tmpl_args.iter().try_for_each(|tmpl_arg| match tmpl_arg {
            TemplateArg::Value(v) => {
                success_types.push(gas_type(*v));
                Ok(())
            }
            TemplateArg::Type(_) => Err(Error::UnsupportedTypeArg),
        })?;
        Ok((
            vec![gas_builtin_type(), gas_type(1)],
            vec![success_types, vec![gas_builtin_type()]],
        ))
    }
}

struct SplitGasExtension {}

impl InvokeExtension for SplitGasExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Type>), Error> {
        if tmpl_args.len() <= 1 {
            return Err(Error::WrongNumberOfTypeArgs);
        }
        let mut res_types = vec![];
        let mut total = 0;
        tmpl_args.iter().try_for_each(|tmpl_arg| match tmpl_arg {
            TemplateArg::Value(v) => {
                res_types.push(gas_type(*v));
                total += v;
                Ok(())
            }
            TemplateArg::Type(_) => Err(Error::UnsupportedTypeArg),
        })?;
        Ok((vec![gas_type(total)], res_types))
    }
}

pub(super) fn register(registry: &mut ExtensionRegistry) {
    registry
        .jump_exts
        .insert("get_gas".to_string(), Box::new(GetGasExtension {}));
    registry
        .invoke_exts
        .insert("split_gas".to_string(), Box::new(SplitGasExtension {}));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_gas_legal_usage() {
        assert_eq!(
            GetGasExtension {}.get_signature(&vec![TemplateArg::Value(1), TemplateArg::Value(2)]),
            Ok((
                vec![gas_builtin_type(), gas_type(1)],
                vec![
                    vec![gas_builtin_type(), gas_type(1), gas_type(2)],
                    vec![gas_builtin_type()]
                ],
            ))
        );
    }

    #[test]
    fn get_gas_wrong_num_of_args() {
        assert_eq!(
            GetGasExtension {}.get_signature(&vec![]),
            Err(Error::WrongNumberOfTypeArgs)
        );
    }

    #[test]
    fn get_gas_wrong_arg_type() {
        assert_eq!(
            GetGasExtension {}.get_signature(&vec![TemplateArg::Type(gas_type(1))]),
            Err(Error::UnsupportedTypeArg)
        );
    }

    #[test]
    fn split_gas_legal_usage() {
        assert_eq!(
            SplitGasExtension {}.get_signature(&vec![TemplateArg::Value(1), TemplateArg::Value(2)]),
            Ok((vec![gas_type(3)], vec![gas_type(1), gas_type(2)],))
        );
    }

    #[test]
    fn split_gas_wrong_num_of_args() {
        assert_eq!(
            SplitGasExtension {}.get_signature(&vec![]),
            Err(Error::WrongNumberOfTypeArgs)
        );
        assert_eq!(
            SplitGasExtension {}.get_signature(&vec![TemplateArg::Value(1)]),
            Err(Error::WrongNumberOfTypeArgs)
        );
    }

    #[test]
    fn split_gas_wrong_arg_type() {
        assert_eq!(
            SplitGasExtension {}
                .get_signature(&vec![TemplateArg::Value(1), TemplateArg::Type(gas_type(1))]),
            Err(Error::UnsupportedTypeArg)
        );
    }
}
