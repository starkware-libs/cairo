use super::{
    validate_no_args, Specialization, SpecializationBox, SpecializationError, Specializer,
    SpecializerBox,
};
use crate::program::{Identifier, TemplateArg, Type};

enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

fn get_op(id: &Identifier) -> Result<Op, SpecializationError> {
    match id {
        Identifier::Name(name) => match name.as_str() {
            "Add" => Ok(Op::Add),
            "Sub" => Ok(Op::Sub),
            "Mul" => Ok(Op::Mul),
            "Div" => Ok(Op::Div),
            "Mod" => Ok(Op::Mod),
            _ => Err(SpecializationError::UnsupportedTemplateArg),
        },
    }
}

struct IntOpSpecializer {}
impl Specializer for IntOpSpecializer {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<SpecializationBox, SpecializationError> {
        match tmpl_args {
            [TemplateArg::Type(Type { id, args })] if args.is_empty() => {
                Ok(Box::new(IntOp { _op: get_op(id)? }))
            }
            [TemplateArg::Type(Type { id, args }), TemplateArg::Value(c)] if args.is_empty() => {
                Ok(Box::new(IntOpWithConst { _op: get_op(id)?, _c: *c }))
            }
            [TemplateArg::Value(c)] => Ok(Box::new(IntConst { _c: *c })),
            _ => Err(SpecializationError::UnsupportedTemplateArg),
        }
    }
}
struct IntOp {
    _op: Op,
}
impl Specialization for IntOp {}

struct IntOpWithConst {
    _op: Op,
    _c: i64,
}
impl Specialization for IntOpWithConst {}

struct IntConst {
    _c: i64,
}
impl Specialization for IntConst {}

struct IgnoreSpecializer {}
impl Specializer for IgnoreSpecializer {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<SpecializationBox, SpecializationError> {
        validate_no_args(tmpl_args)?;
        Ok(Box::new(Ignore {}))
    }
}

struct Ignore {}
impl Specialization for Ignore {}

struct DuplicateSpecializer {}
impl Specializer for DuplicateSpecializer {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<SpecializationBox, SpecializationError> {
        validate_no_args(tmpl_args)?;
        Ok(Box::new(Duplicate {}))
    }
}

struct Duplicate {}
impl Specialization for Duplicate {}

struct JumpNzSpecializer {}
impl Specializer for JumpNzSpecializer {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<SpecializationBox, SpecializationError> {
        validate_no_args(tmpl_args)?;
        Ok(Box::new(JumpNz {}))
    }
}

struct JumpNz {}
impl Specialization for JumpNz {}

struct UnwrapNzSpecializer {}
impl Specializer for UnwrapNzSpecializer {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<SpecializationBox, SpecializationError> {
        validate_no_args(tmpl_args)?;
        Ok(Box::new(UnwrapNz {}))
    }
}

struct UnwrapNz {}
impl Specialization for UnwrapNz {}

pub(super) fn extensions() -> [(Identifier, SpecializerBox); 5] {
    [
        (Identifier::Name("int_op".into()), Box::new(IntOpSpecializer {})),
        (Identifier::Name("int_ignore".into()), Box::new(IgnoreSpecializer {})),
        (Identifier::Name("int_dup".into()), Box::new(DuplicateSpecializer {})),
        (Identifier::Name("int_jump_nz".into()), Box::new(JumpNzSpecializer {})),
        (Identifier::Name("int_unwrap_nz".into()), Box::new(UnwrapNzSpecializer {})),
    ]
}
