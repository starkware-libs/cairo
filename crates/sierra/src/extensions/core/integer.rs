use super::validate_no_args;
use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, Extension, ExtensionBox, SpecializationError,
};
use crate::program::{ExtensionId, TemplateArg};

#[derive(Copy, Clone, Debug)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

struct IntOpExtension {
    op: Op,
}
impl Extension for IntOpExtension {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        match tmpl_args {
            [] => Ok(Box::new(IntOp { _op: self.op })),
            [TemplateArg::Value(c)] => {
                if matches!(self.op, Op::Div | Op::Mod) && *c == 0 {
                    Err(SpecializationError::UnsupportedTemplateArg)
                } else {
                    Ok(Box::new(IntOpWithConst { _op: self.op, _c: *c }))
                }
            }
            _ => Err(SpecializationError::UnsupportedTemplateArg),
        }
    }
}
struct IntOp {
    _op: Op,
}
impl ConcreteExtension for IntOp {}

struct IntOpWithConst {
    _op: Op,
    _c: i64,
}
impl ConcreteExtension for IntOpWithConst {}

struct IntConstExtension {}
impl Extension for IntConstExtension {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        match tmpl_args {
            [TemplateArg::Value(c)] => Ok(Box::new(IntConst { _c: *c })),
            _ => Err(SpecializationError::UnsupportedTemplateArg),
        }
    }
}

struct IntConst {
    _c: i64,
}
impl ConcreteExtension for IntConst {}

struct IgnoreExtension {}
impl Extension for IgnoreExtension {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        validate_no_args(tmpl_args)?;
        Ok(Box::new(Ignore {}))
    }
}

struct Ignore {}
impl ConcreteExtension for Ignore {}

struct DuplicateExtension {}
impl Extension for DuplicateExtension {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        validate_no_args(tmpl_args)?;
        Ok(Box::new(Duplicate {}))
    }
}

struct Duplicate {}
impl ConcreteExtension for Duplicate {}

struct JumpNzExtension {}
impl Extension for JumpNzExtension {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        validate_no_args(tmpl_args)?;
        Ok(Box::new(JumpNz {}))
    }
}

struct JumpNz {}
impl ConcreteExtension for JumpNz {}

struct UnwrapNzExtension {}
impl Extension for UnwrapNzExtension {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        validate_no_args(tmpl_args)?;
        Ok(Box::new(UnwrapNz {}))
    }
}

struct UnwrapNz {}
impl ConcreteExtension for UnwrapNz {}

pub(super) fn extensions() -> [(ExtensionId, ExtensionBox); 10] {
    [
        (ExtensionId::Name("int_add".into()), Box::new(IntOpExtension { op: Op::Add })),
        (ExtensionId::Name("int_sub".into()), Box::new(IntOpExtension { op: Op::Sub })),
        (ExtensionId::Name("int_mul".into()), Box::new(IntOpExtension { op: Op::Mul })),
        (ExtensionId::Name("int_div".into()), Box::new(IntOpExtension { op: Op::Div })),
        (ExtensionId::Name("int_mod".into()), Box::new(IntOpExtension { op: Op::Mod })),
        (ExtensionId::Name("int_const".into()), Box::new(IntConstExtension {})),
        (ExtensionId::Name("int_ignore".into()), Box::new(IgnoreExtension {})),
        (ExtensionId::Name("int_dup".into()), Box::new(DuplicateExtension {})),
        (ExtensionId::Name("int_jump_nz".into()), Box::new(JumpNzExtension {})),
        (ExtensionId::Name("int_unwrap_nz".into()), Box::new(UnwrapNzExtension {})),
    ]
}
