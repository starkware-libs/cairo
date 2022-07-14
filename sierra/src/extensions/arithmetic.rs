use crate::{
    extensions::*,
    ref_value::{Op, RefValue},
    utils::{as_deferred, as_nonzero},
};

struct ArithmeticExtension {
    op: Op,
}

impl ExtensionImplementation for ArithmeticExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        match tmpl_args.len() {
            1 => {
                let numeric_type = get_numeric_type(&tmpl_args[0])?;
                Ok(simple_invoke_ext_sign(
                    vec![numeric_type.clone(), numeric_type.clone()],
                    vec![as_deferred(numeric_type.clone())],
                ))
            }
            2 => {
                let (numeric_type, _) = get_type_value(tmpl_args)?;
                Ok(simple_invoke_ext_sign(
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
        mem_state: MemState,
        arg_refs: Vec<RefValue>,
    ) -> Result<Vec<(MemState, Vec<RefValue>)>, Error> {
        match tmpl_args.len() {
            1 => Ok(vec![(
                mem_state,
                vec![RefValue::Op(
                    as_final(&arg_refs[0])?,
                    self.op,
                    as_final(&arg_refs[1])?,
                )],
            )]),
            2 => {
                let (_, c) = get_type_value(tmpl_args)?;
                Ok(vec![(
                    mem_state,
                    vec![RefValue::OpWithConst(as_final(&arg_refs[0])?, self.op, c)],
                )])
            }
            _ => Err(Error::WrongNumberOfTypeArgs),
        }
    }
}

struct DivExtension {
    op: Op,
}

impl ExtensionImplementation for DivExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        match tmpl_args.len() {
            1 => {
                let numeric_type = get_numeric_type(&tmpl_args[0])?;
                Ok(simple_invoke_ext_sign(
                    vec![numeric_type.clone(), as_nonzero(numeric_type.clone())],
                    vec![as_deferred(numeric_type.clone())],
                ))
            }
            2 => {
                let (numeric_type, c) = get_type_value(tmpl_args)?;
                if c == 0 {
                    Err(Error::UnsupportedTypeArg)
                } else {
                    Ok(simple_invoke_ext_sign(
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
        mem_state: MemState,
        arg_refs: Vec<RefValue>,
    ) -> Result<Vec<(MemState, Vec<RefValue>)>, Error> {
        match tmpl_args.len() {
            1 => Ok(vec![(
                mem_state,
                vec![RefValue::Op(
                    as_final(&arg_refs[0])?,
                    self.op,
                    as_final(&arg_refs[1])?,
                )],
            )]),
            2 => {
                let (_, c) = get_type_value(tmpl_args)?;
                if c == 0 {
                    Err(Error::UnsupportedTypeArg)
                } else {
                    Ok(vec![(
                        mem_state,
                        vec![RefValue::OpWithConst(as_final(&arg_refs[0])?, self.op, c)],
                    )])
                }
            }
            _ => Err(Error::WrongNumberOfTypeArgs),
        }
    }
}

struct DuplicateExtension {}

impl ExtensionImplementation for DuplicateExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        let numeric_type = get_type(tmpl_args)?;
        Ok(simple_invoke_ext_sign(
            vec![numeric_type.clone()],
            vec![numeric_type.clone(), numeric_type.clone()],
        ))
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        mem_state: MemState,
        arg_refs: Vec<RefValue>,
    ) -> Result<Vec<(MemState, Vec<RefValue>)>, Error> {
        Ok(vec![(
            mem_state,
            vec![arg_refs[0].clone(), arg_refs[0].clone()],
        )])
    }
}

struct ConstantExtension {}

impl ExtensionImplementation for ConstantExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        if tmpl_args.len() != 2 {
            return Err(Error::WrongNumberOfTypeArgs);
        }
        let (numeric_type, _) = get_type_value(tmpl_args)?;
        Ok(simple_invoke_ext_sign(
            vec![],
            vec![as_deferred(numeric_type.clone())],
        ))
    }

    fn mem_change(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        mem_state: MemState,
        _arg_refs: Vec<RefValue>,
    ) -> Result<Vec<(MemState, Vec<RefValue>)>, Error> {
        let (_, c) = get_type_value(tmpl_args)?;
        Ok(vec![(mem_state, vec![RefValue::Const(c)])])
    }
}

struct IgnoreExtension {}

impl ExtensionImplementation for IgnoreExtension {
    fn get_signature(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
    ) -> Result<ExtensionSignature, Error> {
        let numeric_type = get_type(tmpl_args)?;
        Ok(simple_invoke_ext_sign(vec![numeric_type.clone()], vec![]))
    }

    fn mem_change(
        self: &Self,
        _tmpl_args: &Vec<TemplateArg>,
        _registry: &TypeRegistry,
        mem_state: MemState,
        _arg_refs: Vec<RefValue>,
    ) -> Result<Vec<(MemState, Vec<RefValue>)>, Error> {
        Ok(vec![(mem_state, vec![])])
    }
}

fn get_numeric_type<'a>(arg: &'a TemplateArg) -> Result<&'a Type, Error> {
    match arg {
        TemplateArg::Type(t) if matches!(t.to_string().as_str(), "int" | "felt") => Ok(t),
        _ => Err(Error::UnsupportedTypeArg),
    }
}

fn get_type_value<'a>(tmpl_args: &'a Vec<TemplateArg>) -> Result<(&'a Type, i64), Error> {
    let numeric_type = get_numeric_type(&tmpl_args[0])?;
    match &tmpl_args[1] {
        TemplateArg::Value(v) => Ok((numeric_type, *v)),
        _ => Err(Error::UnsupportedTypeArg),
    }
}

fn get_type<'a>(tmpl_args: &'a Vec<TemplateArg>) -> Result<&'a Type, Error> {
    if tmpl_args.len() != 1 {
        return Err(Error::WrongNumberOfTypeArgs);
    }
    get_numeric_type(&tmpl_args[0])
}

struct ArithmeticTypeInfo {}

impl TypeInfoImplementation for ArithmeticTypeInfo {
    fn get_info(
        self: &Self,
        tmpl_args: &Vec<TemplateArg>,
        _: &TypeRegistry,
    ) -> Result<TypeInfo, Error> {
        if !tmpl_args.is_empty() {
            return Err(Error::WrongNumberOfTypeArgs);
        }
        Ok(TypeInfo { size: 1 })
    }
}

pub(super) fn extensions() -> [(String, ExtensionBox); 8] {
    [
        (
            "add".to_string(),
            Box::new(ArithmeticExtension { op: Op::Add }),
        ),
        (
            "sub".to_string(),
            Box::new(ArithmeticExtension { op: Op::Sub }),
        ),
        (
            "mul".to_string(),
            Box::new(ArithmeticExtension { op: Op::Mul }),
        ),
        ("div".to_string(), Box::new(DivExtension { op: Op::Div })),
        ("mod".to_string(), Box::new(DivExtension { op: Op::Mod })),
        ("duplicate_num".to_string(), Box::new(DuplicateExtension {})),
        ("constant_num".to_string(), Box::new(ConstantExtension {})),
        ("ignore_num".to_string(), Box::new(IgnoreExtension {})),
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
            Ok(simple_invoke_ext_sign(
                vec![ty.clone(), ty.clone()],
                vec![as_deferred(ty.clone())],
            ))
        );
        assert_eq!(
            ArithmeticExtension { op: Op::Add }
                .get_signature(&vec![type_arg(ty.clone()), val_arg(1)],),
            Ok(simple_invoke_ext_sign(
                vec![ty.clone()],
                vec![as_deferred(ty.clone())],
            ))
        );
        assert_eq!(
            DuplicateExtension {}.get_signature(&vec![type_arg(ty.clone())]),
            Ok(simple_invoke_ext_sign(
                vec![ty.clone()],
                vec![ty.clone(), ty.clone()],
            ))
        );
        assert_eq!(
            ConstantExtension {}.get_signature(&vec![type_arg(ty.clone()), val_arg(1)],),
            Ok(simple_invoke_ext_sign(
                vec![],
                vec![as_deferred(ty.clone())],
            ))
        );
        assert_eq!(
            IgnoreExtension {}.get_signature(&vec![type_arg(ty.clone())]),
            Ok(simple_invoke_ext_sign(vec![ty.clone()], vec![]))
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
