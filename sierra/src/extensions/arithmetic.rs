use crate::{
    extensions::*,
    ref_value::{Op, RefValue},
    utils::{as_deferred, as_nonzero},
};

struct ArithmeticExtension {
    op: Op,
}

impl NonBranchImplementation for ArithmeticExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Type>), Error> {
        match tmpl_args.len() {
            1 => {
                let numeric_type = validate_numeric(single_type_arg(tmpl_args)?)?;
                Ok((
                    vec![numeric_type.clone(), numeric_type.clone()],
                    vec![as_deferred(numeric_type.clone())],
                ))
            }
            2 => {
                let (numeric_type, _) = type_value_args(tmpl_args)?;
                validate_numeric(numeric_type)?;
                Ok((
                    vec![numeric_type.clone()],
                    vec![as_deferred(numeric_type.clone())],
                ))
            }
            _ => Err(Error::WrongNumberOfTypeArgs),
        }
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        context: Context,
        arg_refs: Vec<RefValue>,
    ) -> Result<(Context, Vec<RefValue>), Error> {
        match tmpl_args.len() {
            1 => Ok((
                context,
                vec![RefValue::Op(
                    as_final(&arg_refs[0])?,
                    self.op,
                    as_final(&arg_refs[1])?,
                )],
            )),
            2 => {
                let (_, c) = type_value_args(tmpl_args)?;
                Ok((
                    context,
                    vec![RefValue::OpWithConst(as_final(&arg_refs[0])?, self.op, c)],
                ))
            }
            _ => Err(Error::WrongNumberOfTypeArgs),
        }
    }
}

struct DivExtension {
    op: Op,
}

impl NonBranchImplementation for DivExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Type>), Error> {
        match tmpl_args.len() {
            1 => {
                let numeric_type = validate_numeric(single_type_arg(tmpl_args)?)?;
                Ok((
                    vec![numeric_type.clone(), as_nonzero(numeric_type.clone())],
                    vec![as_deferred(numeric_type.clone())],
                ))
            }
            2 => {
                let (numeric_type, c) = type_value_args(tmpl_args)?;
                validate_numeric(numeric_type)?;
                if c == 0 {
                    Err(Error::UnsupportedTypeArg)
                } else {
                    Ok((
                        vec![numeric_type.clone()],
                        vec![as_deferred(numeric_type.clone())],
                    ))
                }
            }
            _ => Err(Error::WrongNumberOfTypeArgs),
        }
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        context: Context,
        arg_refs: Vec<RefValue>,
    ) -> Result<(Context, Vec<RefValue>), Error> {
        match tmpl_args.len() {
            1 => Ok((
                context,
                vec![RefValue::Op(
                    as_final(&arg_refs[0])?,
                    self.op,
                    as_final(&arg_refs[1])?,
                )],
            )),
            2 => {
                let (_, c) = type_value_args(tmpl_args)?;
                if c == 0 {
                    Err(Error::UnsupportedTypeArg)
                } else {
                    Ok((
                        context,
                        vec![RefValue::OpWithConst(as_final(&arg_refs[0])?, self.op, c)],
                    ))
                }
            }
            _ => Err(Error::WrongNumberOfTypeArgs),
        }
    }
}

struct DuplicateExtension {}

impl NonBranchImplementation for DuplicateExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Type>), Error> {
        let numeric_type = validate_numeric(single_type_arg(tmpl_args)?)?;
        Ok((
            vec![numeric_type.clone()],
            vec![numeric_type.clone(), numeric_type.clone()],
        ))
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        context: Context,
        arg_refs: Vec<RefValue>,
    ) -> Result<(Context, Vec<RefValue>), Error> {
        Ok((context, vec![arg_refs[0].clone(), arg_refs[0].clone()]))
    }
}

struct ConstantExtension {}

impl NonBranchImplementation for ConstantExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Type>), Error> {
        let (numeric_type, _) = type_value_args(tmpl_args)?;
        validate_numeric(numeric_type)?;
        Ok((vec![], vec![as_deferred(numeric_type.clone())]))
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        context: Context,
        _arg_refs: Vec<RefValue>,
    ) -> Result<(Context, Vec<RefValue>), Error> {
        let (_, c) = type_value_args(tmpl_args)?;
        Ok((context, vec![RefValue::Const(c)]))
    }
}

struct IgnoreExtension {}

impl NonBranchImplementation for IgnoreExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<(Vec<Type>, Vec<Type>), Error> {
        let numeric_type = validate_numeric(single_type_arg(tmpl_args)?)?;
        Ok((vec![numeric_type.clone()], vec![]))
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        context: Context,
        _arg_refs: Vec<RefValue>,
    ) -> Result<(Context, Vec<RefValue>), Error> {
        Ok((context, vec![]))
    }
}

fn validate_numeric<'a>(t: &'a Type) -> Result<&'a Type, Error> {
    if matches!(t.to_string().as_str(), "int" | "felt") {
        Ok(t)
    } else {
        Err(Error::UnsupportedTypeArg)
    }
}

struct ArithmeticTypeInfo {}

impl TypeInfoImplementation for ArithmeticTypeInfo {
    fn get_info(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        _: &TypeRegistry,
    ) -> Result<TypeInfo, Error> {
        validate_size_eq(tmpl_args, 0)?;
        Ok(TypeInfo { size: 1 })
    }
}

pub(super) fn extensions() -> [(String, ExtensionBox); 8] {
    [
        (
            "add".to_string(),
            wrap_non_branch(Box::new(ArithmeticExtension { op: Op::Add })),
        ),
        (
            "sub".to_string(),
            wrap_non_branch(Box::new(ArithmeticExtension { op: Op::Sub })),
        ),
        (
            "mul".to_string(),
            wrap_non_branch(Box::new(ArithmeticExtension { op: Op::Mul })),
        ),
        (
            "div".to_string(),
            wrap_non_branch(Box::new(DivExtension { op: Op::Div })),
        ),
        (
            "mod".to_string(),
            wrap_non_branch(Box::new(DivExtension { op: Op::Mod })),
        ),
        (
            "duplicate_num".to_string(),
            wrap_non_branch(Box::new(DuplicateExtension {})),
        ),
        (
            "constant_num".to_string(),
            wrap_non_branch(Box::new(ConstantExtension {})),
        ),
        (
            "ignore_num".to_string(),
            wrap_non_branch(Box::new(IgnoreExtension {})),
        ),
    ]
}

pub(super) fn types() -> [(String, TypeInfoBox); 2] {
    [
        ("int".to_string(), Box::new(ArithmeticTypeInfo {})),
        ("felt".to_string(), Box::new(ArithmeticTypeInfo {})),
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
            ArithmeticExtension { op: Op::Add }.get_signature(&vec![type_arg(ty.clone())]),
            Ok((vec![ty.clone(), ty.clone()], vec![as_deferred(ty.clone())],))
        );
        assert_eq!(
            ArithmeticExtension { op: Op::Add }
                .get_signature(&vec![type_arg(ty.clone()), val_arg(1)],),
            Ok((vec![ty.clone()], vec![as_deferred(ty.clone())],))
        );
        assert_eq!(
            DuplicateExtension {}.get_signature(&vec![type_arg(ty.clone())]),
            Ok((vec![ty.clone()], vec![ty.clone(), ty.clone()],))
        );
        assert_eq!(
            ConstantExtension {}.get_signature(&vec![type_arg(ty.clone()), val_arg(1)],),
            Ok((vec![], vec![as_deferred(ty.clone())],))
        );
        assert_eq!(
            IgnoreExtension {}.get_signature(&vec![type_arg(ty.clone())]),
            Ok((vec![ty.clone()], vec![]))
        );
    }

    #[test]
    fn wrong_num_of_args() {
        assert_eq!(
            ArithmeticExtension { op: Op::Add }.get_signature(&vec![]),
            Err(Error::WrongNumberOfTypeArgs)
        );
        assert_eq!(
            ConstantExtension {}.get_signature(&vec![]),
            Err(Error::WrongNumberOfTypeArgs)
        );
        assert_eq!(
            DuplicateExtension {}.get_signature(&vec![]),
            Err(Error::WrongNumberOfTypeArgs)
        );
        assert_eq!(
            IgnoreExtension {}.get_signature(&vec![]),
            Err(Error::WrongNumberOfTypeArgs)
        );
    }

    #[test]
    fn wrong_arg_type() {
        assert_eq!(
            ArithmeticExtension { op: Op::Add }.get_signature(&vec![type_arg(as_type("non-int"))]),
            Err(Error::UnsupportedTypeArg)
        );
        assert_eq!(
            ArithmeticExtension { op: Op::Add }.get_signature(&vec![val_arg(1)]),
            Err(Error::UnsupportedTypeArg)
        );
    }
}
