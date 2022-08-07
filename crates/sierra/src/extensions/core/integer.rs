use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, Extension, ExtensionBox, NoArgsExtension,
    SpecializationError,
};
use crate::program::{ExtensionId, TemplateArg};

/// Possible operators for integers.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

/// Extension for operations on integers.
struct IntOpExtension {
    operator: Operator,
}
impl Extension for IntOpExtension {
    fn specialize(
        &self,
        args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        match args {
            [] => Ok(Box::new(BinaryIntOpConcrete { _operator: self.operator })),
            [TemplateArg::Value(c)] => {
                if matches!(self.operator, Operator::Div | Operator::Mod) && *c == 0 {
                    Err(SpecializationError::UnsupportedTemplateArg)
                } else {
                    Ok(Box::new(IntOpWithConstConcrete { _operator: self.operator, _c: *c }))
                }
            }
            _ => Err(SpecializationError::UnsupportedTemplateArg),
        }
    }
}

/// Binary int operations.
struct BinaryIntOpConcrete {
    _operator: Operator,
}
impl ConcreteExtension for BinaryIntOpConcrete {}

/// Operations between a int and a const.
struct IntOpWithConstConcrete {
    _operator: Operator,
    _c: i64,
}
impl ConcreteExtension for IntOpWithConstConcrete {}

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
        Box::new(IgnoreConcrete {})
    }
}

struct IgnoreConcrete {}
impl ConcreteExtension for IgnoreConcrete {}

/// Extension for duplicating an int.
struct DuplicateExtension {}
impl NoArgsExtension for DuplicateExtension {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(DuplicateConcrete {})
    }
}

struct DuplicateConcrete {}
impl ConcreteExtension for DuplicateConcrete {}

/// Extension for jump non-zero on an int's value, and returning a non-zero wrapped int in case of
/// success.
struct JumpNotZeroExtension {}
impl NoArgsExtension for JumpNotZeroExtension {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(JumpNotZeroConcrete {})
    }
}

struct JumpNotZeroConcrete {}
impl ConcreteExtension for JumpNotZeroConcrete {}

/// Extension for unwrapping a non-zero int back into a regular int.
struct UnwrapNonZeroExtension {}
impl NoArgsExtension for UnwrapNonZeroExtension {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(UnwrapNonZeroConcrete {})
    }
}

struct UnwrapNonZeroConcrete {}
impl ConcreteExtension for UnwrapNonZeroConcrete {}

pub(super) fn extensions() -> [(ExtensionId, ExtensionBox); 10] {
    [
        ("int_add".into(), Box::new(IntOpExtension { operator: Operator::Add })),
        ("int_sub".into(), Box::new(IntOpExtension { operator: Operator::Sub })),
        ("int_mul".into(), Box::new(IntOpExtension { operator: Operator::Mul })),
        ("int_div".into(), Box::new(IntOpExtension { operator: Operator::Div })),
        ("int_mod".into(), Box::new(IntOpExtension { operator: Operator::Mod })),
        ("int_const".into(), Box::new(IntConstExtension {})),
        ("int_ignore".into(), Box::new(IgnoreExtension {})),
        ("int_dup".into(), Box::new(DuplicateExtension {})),
        ("int_jump_nz".into(), Box::new(JumpNotZeroExtension {})),
        ("int_unwrap_nz".into(), Box::new(UnwrapNonZeroExtension {})),
    ]
}
