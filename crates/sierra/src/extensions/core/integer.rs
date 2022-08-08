use super::{single_cell_identity, unpack_inputs};
use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, GenericExtension, GenericExtensionBox, InputError,
    NoGenericArgsGenericExtension, NonBranchConcreteExtension, SpecializationError,
};
use crate::ids::GenericExtensionId;
use crate::mem_cell::MemCell;
use crate::program::GenericArg;

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
struct OperationGeneric {
    operator: Operator,
}
impl GenericExtension for OperationGeneric {
    fn specialize(&self, args: &[GenericArg]) -> Result<ConcreteExtensionBox, SpecializationError> {
        match args {
            [] => Ok(Box::new(BinaryOperationConcrete { operator: self.operator })),
            [GenericArg::Value(c)] => {
                if matches!(self.operator, Operator::Div | Operator::Mod) && *c == 0 {
                    Err(SpecializationError::UnsupportedTemplateArg)
                } else {
                    Ok(Box::new(OperationWithConstConcrete { operator: self.operator, c: *c }))
                }
            }
            _ => Err(SpecializationError::UnsupportedTemplateArg),
        }
    }
}

struct BinaryOperationConcrete {
    operator: Operator,
}
impl NonBranchConcreteExtension for BinaryOperationConcrete {
    fn non_branch_simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<Vec<Vec<MemCell>>, InputError> {
        let [MemCell { value: lhs }, MemCell { value: rhs }] = unpack_inputs::<2>(inputs)?;
        Ok(vec![vec![
            match &self.operator {
                Operator::Add => lhs + rhs,
                Operator::Sub => lhs - rhs,
                Operator::Mul => lhs * rhs,
                Operator::Div => lhs / rhs,
                Operator::Mod => lhs % rhs,
            }
            .into(),
        ]])
    }
}

/// Operations between a int and a const.
struct OperationWithConstConcrete {
    operator: Operator,
    c: i64,
}
impl NonBranchConcreteExtension for OperationWithConstConcrete {
    fn non_branch_simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<Vec<Vec<MemCell>>, InputError> {
        let [MemCell { value }] = unpack_inputs::<1>(inputs)?;
        Ok(vec![vec![
            match &self.operator {
                Operator::Add => value + self.c,
                Operator::Sub => value - self.c,
                Operator::Mul => value * self.c,
                Operator::Div => value / self.c,
                Operator::Mod => value % self.c,
            }
            .into(),
        ]])
    }
}

/// Extension for creating a constant int.
struct ConstGeneric {}
impl GenericExtension for ConstGeneric {
    fn specialize(&self, args: &[GenericArg]) -> Result<ConcreteExtensionBox, SpecializationError> {
        match args {
            [GenericArg::Value(c)] => Ok(Box::new(ConstConcrete { c: *c })),
            _ => Err(SpecializationError::UnsupportedTemplateArg),
        }
    }
}

struct ConstConcrete {
    c: i64,
}
impl NonBranchConcreteExtension for ConstConcrete {
    fn non_branch_simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<Vec<Vec<MemCell>>, InputError> {
        unpack_inputs::<0>(inputs)?;
        Ok(vec![vec![self.c.into()]])
    }
}

/// Extension for ignoring an int.
struct IgnoreGeneric {}
impl NoGenericArgsGenericExtension for IgnoreGeneric {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(IgnoreConcrete {})
    }
}

struct IgnoreConcrete {}
impl NonBranchConcreteExtension for IgnoreConcrete {
    fn non_branch_simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<Vec<Vec<MemCell>>, InputError> {
        unpack_inputs::<1>(inputs)?;
        Ok(vec![])
    }
}

/// Extension for duplicating an int.
struct DuplicateGeneric {}
impl NoGenericArgsGenericExtension for DuplicateGeneric {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(DuplicateConcrete {})
    }
}

struct DuplicateConcrete {}
impl NonBranchConcreteExtension for DuplicateConcrete {
    fn non_branch_simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<Vec<Vec<MemCell>>, InputError> {
        let [MemCell { value }] = unpack_inputs::<1>(inputs)?;
        Ok(vec![vec![value.into()], vec![value.into()]])
    }
}

/// Extension for jump non-zero on an int's value, and returning a non-zero wrapped int in case of
/// success.
struct JumpNotZeroGeneric {}
impl NoGenericArgsGenericExtension for JumpNotZeroGeneric {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(JumpNotZeroConcrete {})
    }
}

struct JumpNotZeroConcrete {}
impl ConcreteExtension for JumpNotZeroConcrete {
    fn simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<(Vec<Vec<MemCell>>, usize), InputError> {
        let [MemCell { value }] = unpack_inputs::<1>(inputs)?;
        if value != 0 {
            // Non-zero - jumping to the success branch and providing a NonZero wrap to the given
            // value.
            Ok((vec![vec![value.into()]], 0))
        } else {
            // Zero - jumping to the failure branch.
            Ok((vec![], 1))
        }
    }
}

/// Extension for unwrapping a non-zero int back into a regular int.
struct UnwrapNonZeroGeneric {}
impl NoGenericArgsGenericExtension for UnwrapNonZeroGeneric {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(UnwrapNonZeroConcrete {})
    }
}

struct UnwrapNonZeroConcrete {}
impl NonBranchConcreteExtension for UnwrapNonZeroConcrete {
    fn non_branch_simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<Vec<Vec<MemCell>>, InputError> {
        single_cell_identity::<1>(inputs)
    }
}

pub(super) fn extensions() -> [(GenericExtensionId, GenericExtensionBox); 10] {
    [
        ("int_add".into(), Box::new(OperationGeneric { operator: Operator::Add })),
        ("int_sub".into(), Box::new(OperationGeneric { operator: Operator::Sub })),
        ("int_mul".into(), Box::new(OperationGeneric { operator: Operator::Mul })),
        ("int_div".into(), Box::new(OperationGeneric { operator: Operator::Div })),
        ("int_mod".into(), Box::new(OperationGeneric { operator: Operator::Mod })),
        ("int_const".into(), Box::new(ConstGeneric {})),
        ("int_ignore".into(), Box::new(IgnoreGeneric {})),
        ("int_dup".into(), Box::new(DuplicateGeneric {})),
        ("int_jump_nz".into(), Box::new(JumpNotZeroGeneric {})),
        ("int_unwrap_nz".into(), Box::new(UnwrapNonZeroGeneric {})),
    ]
}
