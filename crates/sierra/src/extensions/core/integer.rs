use super::{single_cell_identity, unpack_inputs};
use crate::extension_enum;
use crate::extensions::{
    ConcreteExtension, GenericExtension, InputError, NoGenericArgsGenericExtension,
    NonBranchConcreteExtension, SpecializationError,
};
use crate::ids::GenericExtensionId;
use crate::mem_cell::MemCell;
use crate::program::GenericArg;

extension_enum! {
    pub enum IntegerExtension {
        Operation(OperationGeneric),
        Const(ConstGeneric),
        Ignore(IgnoreGeneric),
        Duplicate(DuplicateGeneric),
        JumpNotZero(JumpNotZeroGeneric),
        UnwrapNonZero(UnwrapNonZeroGeneric)
    }, IntegerConcrete
}

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
pub struct OperationGeneric {
    operator: Operator,
}
impl GenericExtension for OperationGeneric {
    type Concrete = OperationConcrete;
    fn id() -> Option<GenericExtensionId> {
        None
    }
    fn new() -> Option<Self> {
        None
    }
    fn by_id(id: &GenericExtensionId) -> Option<Self> {
        if id == &"int_add".into() {
            return Some(OperationGeneric { operator: Operator::Add });
        }
        if id == &"int_sub".into() {
            return Some(OperationGeneric { operator: Operator::Sub });
        }
        if id == &"int_mul".into() {
            return Some(OperationGeneric { operator: Operator::Mul });
        }
        if id == &"int_div".into() {
            return Some(OperationGeneric { operator: Operator::Div });
        }
        if id == &"int_mod".into() {
            return Some(OperationGeneric { operator: Operator::Mod });
        }
        None
    }
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [] => {
                Ok(OperationConcrete::Binary(BinaryOperationConcrete { operator: self.operator }))
            }
            [GenericArg::Value(c)] => {
                if matches!(self.operator, Operator::Div | Operator::Mod) && *c == 0 {
                    Err(SpecializationError::UnsupportedTemplateArg)
                } else {
                    Ok(OperationConcrete::Const(OperationWithConstConcrete {
                        operator: self.operator,
                        c: *c,
                    }))
                }
            }
            _ => Err(SpecializationError::UnsupportedTemplateArg),
        }
    }
}

pub struct BinaryOperationConcrete {
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
pub struct OperationWithConstConcrete {
    operator: Operator,
    c: i64,
}
pub enum OperationConcrete {
    Binary(BinaryOperationConcrete),
    Const(OperationWithConstConcrete),
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

impl NonBranchConcreteExtension for OperationConcrete {
    fn non_branch_simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<Vec<Vec<MemCell>>, InputError> {
        match self {
            OperationConcrete::Binary(value) => value.non_branch_simulate(inputs),
            OperationConcrete::Const(value) => value.non_branch_simulate(inputs),
        }
    }
}

/// Extension for creating a constant int.
pub struct ConstGeneric {}
impl GenericExtension for ConstGeneric {
    type Concrete = ConstConcrete;
    fn id() -> Option<GenericExtensionId> {
        Some("int_const".into())
    }
    fn new() -> Option<Self> {
        Some(Self {})
    }
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [GenericArg::Value(c)] => Ok(ConstConcrete { c: *c }),
            _ => Err(SpecializationError::UnsupportedTemplateArg),
        }
    }
}

pub struct ConstConcrete {
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
pub struct IgnoreGeneric {}
impl NoGenericArgsGenericExtension for IgnoreGeneric {
    type Concrete = IgnoreConcrete;
    fn id() -> Option<GenericExtensionId> {
        Some("int_ignore".into())
    }
    fn new() -> Option<Self> {
        Some(Self {})
    }
    fn specialize(&self) -> Self::Concrete {
        IgnoreConcrete {}
    }
}

pub struct IgnoreConcrete {}
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
pub struct DuplicateGeneric {}
impl NoGenericArgsGenericExtension for DuplicateGeneric {
    type Concrete = DuplicateConcrete;
    fn id() -> Option<GenericExtensionId> {
        Some("int_dup".into())
    }
    fn new() -> Option<Self> {
        Some(Self {})
    }
    fn specialize(&self) -> Self::Concrete {
        DuplicateConcrete {}
    }
}

pub struct DuplicateConcrete {}
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
pub struct JumpNotZeroGeneric {}
impl NoGenericArgsGenericExtension for JumpNotZeroGeneric {
    type Concrete = JumpNotZeroConcrete;
    fn id() -> Option<GenericExtensionId> {
        Some("int_jump_nz".into())
    }
    fn new() -> Option<Self> {
        Some(Self {})
    }
    fn specialize(&self) -> Self::Concrete {
        JumpNotZeroConcrete {}
    }
}

pub struct JumpNotZeroConcrete {}
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
pub struct UnwrapNonZeroGeneric {}
impl NoGenericArgsGenericExtension for UnwrapNonZeroGeneric {
    type Concrete = UnwrapNonZeroConcrete;
    fn id() -> Option<GenericExtensionId> {
        Some("int_unwrap_nz".into())
    }
    fn new() -> Option<Self> {
        Some(Self {})
    }
    fn specialize(&self) -> Self::Concrete {
        UnwrapNonZeroConcrete {}
    }
}

pub struct UnwrapNonZeroConcrete {}
impl NonBranchConcreteExtension for UnwrapNonZeroConcrete {
    fn non_branch_simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<Vec<Vec<MemCell>>, InputError> {
        single_cell_identity::<1>(inputs)
    }
}
