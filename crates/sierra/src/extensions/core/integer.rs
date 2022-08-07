use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, Extension, ExtensionBox, NoArgsExtension,
    SpecializationError,
};
use crate::program::{ExtensionId, TemplateArg};

/// Possible operations for intergers.
#[derive(Copy, Clone, Debug)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

/// Extension for operations on integers.
struct IntOpExtension {
    op: Op,
}
impl Extension for IntOpExtension {
    fn specialize(
        &self,
        args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        match args {
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

/// Operations on two ints.
struct IntOp {
    _op: Op,
}
impl ConcreteExtension for IntOp {}

/// Operations between a int and a const.
struct IntOpWithConst {
    _op: Op,
    _c: i64,
}
impl ConcreteExtension for IntOpWithConst {}

/// Extension for creating a constant int.
struct IntConstExtension {}
impl Extension for IntConstExtension {
    fn specialize(
        &self,
        args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        match args {
            [TemplateArg::Value(c)] => Ok(Box::new(IntConst { _c: *c })),
            _ => Err(SpecializationError::UnsupportedTemplateArg),
        }
    }
}

struct IntConst {
    _c: i64,
}
impl ConcreteExtension for IntConst {}

/// Extension for ignoring an int.
struct IgnoreExtension {}
impl NoArgsExtension for IgnoreExtension {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(Ignore {})
    }
}

struct Ignore {}
impl ConcreteExtension for Ignore {}

/// Extension for duplicating an int.
struct DuplicateExtension {}
impl NoArgsExtension for DuplicateExtension {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(Duplicate {})
    }
}

struct Duplicate {}
impl ConcreteExtension for Duplicate {}

/// Extension for jump non-zero on an int's value, and returning a non-zero wrapped int in case of
/// success.
struct JumpNzExtension {}
impl NoArgsExtension for JumpNzExtension {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(JumpNz {})
    }
}

struct JumpNz {}
impl ConcreteExtension for JumpNz {}

/// Extension for unwrapping a non-zero int back into a regular int.
struct UnwrapNzExtension {}
impl NoArgsExtension for UnwrapNzExtension {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(UnwrapNz {})
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
