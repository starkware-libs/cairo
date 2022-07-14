use crate::graph::*;

pub fn as_deferred(arg: Type) -> Type {
    Type {
        name: "Deferred".to_string(),
        args: vec![type_arg(arg)],
    }
}

pub fn as_nonzero(arg: Type) -> Type {
    Type {
        name: "NonZero".to_string(),
        args: vec![type_arg(arg)],
    }
}

pub fn type_arg(t: Type) -> TemplateArg {
    TemplateArg::Type(t)
}

pub fn val_arg(v: i64) -> TemplateArg {
    TemplateArg::Value(v)
}

pub fn as_type(name: &str) -> Type {
    Type {
        name: name.to_string(),
        args: vec![],
    }
}

pub fn as_tuple(args: Vec<TemplateArg>) -> Type {
    Type {
        name: "Tuple".to_string(),
        args: args,
    }
}

pub fn as_nullable(arg: Type) -> Type {
    Type {
        name: "Nullable".to_string(),
        args: vec![type_arg(arg)],
    }
}

pub fn gas_builtin_type() -> Type {
    as_type("GasBuiltin")
}

pub fn gas_type(v: i64) -> Type {
    Type {
        name: "Gas".to_string(),
        args: vec![val_arg(v)],
    }
}
