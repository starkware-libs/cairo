use crate::extensions::{
    ConcreteLibFunc, GenericLibFunc, NamedLibFunc, NoGenericArgsGenericLibFunc,
    NonBranchConcreteLibFunc, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericLibFuncId};
use crate::program::GenericArg;
use crate::{define_concrete_libfunc_hierarchy, define_libfunc_hierarchy};

define_libfunc_hierarchy! {
    pub enum IntegerLibFunc {
        Operation(OperationGeneric),
        Const(ConstGeneric),
        Ignore(IgnoreGeneric),
        Duplicate(DuplicateGeneric),
        JumpNotZero(JumpNotZeroGeneric),
        UnwrapNonZero(UnwrapNonZeroGeneric),
    }, IntegerConcrete
}

/// Possible operators for integers.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

/// Libfunc for operations on integers.
pub struct OperationGeneric {
    pub operator: Operator,
}
impl GenericLibFunc for OperationGeneric {
    type Concrete = OperationConcrete;
    fn by_id(id: &GenericLibFuncId) -> Option<Self> {
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
                    Err(SpecializationError::UnsupportedGenericArg)
                } else {
                    Ok(OperationConcrete::Const(OperationWithConstConcrete {
                        operator: self.operator,
                        c: *c,
                    }))
                }
            }
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

pub struct BinaryOperationConcrete {
    pub operator: Operator,
}
impl NonBranchConcreteLibFunc for BinaryOperationConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec!["int".into(), "int".into()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec!["int".into()]
    }
}

/// Operations between a int and a const.
pub struct OperationWithConstConcrete {
    pub operator: Operator,
    pub c: i64,
}
define_concrete_libfunc_hierarchy! {
    pub enum OperationConcrete {
        Binary(BinaryOperationConcrete),
        Const(OperationWithConstConcrete),
    }
}

impl NonBranchConcreteLibFunc for OperationWithConstConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec!["int".into()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec!["int".into()]
    }
}

/// LibFunc for creating a constant int.
#[derive(Default)]
pub struct ConstGeneric {}
impl NamedLibFunc for ConstGeneric {
    type Concrete = ConstConcrete;
    const NAME: &'static str = "int_const";
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [GenericArg::Value(c)] => Ok(ConstConcrete { c: *c }),
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

pub struct ConstConcrete {
    pub c: i64,
}
impl NonBranchConcreteLibFunc for ConstConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec!["int".into()]
    }
}

/// LibFunc for ignoring an int.
#[derive(Default)]
pub struct IgnoreGeneric {}
impl NoGenericArgsGenericLibFunc for IgnoreGeneric {
    type Concrete = IgnoreConcrete;
    const NAME: &'static str = "int_ignore";
    fn specialize(&self) -> Self::Concrete {
        IgnoreConcrete {}
    }
}

pub struct IgnoreConcrete {}
impl NonBranchConcreteLibFunc for IgnoreConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec!["int".into()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
}

/// LibFunc for duplicating an int.
#[derive(Default)]
pub struct DuplicateGeneric {}
impl NoGenericArgsGenericLibFunc for DuplicateGeneric {
    type Concrete = DuplicateConcrete;
    const NAME: &'static str = "int_dup";

    fn specialize(&self) -> Self::Concrete {
        DuplicateConcrete {}
    }
}

pub struct DuplicateConcrete {}
impl NonBranchConcreteLibFunc for DuplicateConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec!["int".into()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec!["int".into(), "int".into()]
    }
}

/// LibFunc for jump non-zero on an int's value, and returning a non-zero wrapped int in case of
/// success.
#[derive(Default)]
pub struct JumpNotZeroGeneric {}
impl NoGenericArgsGenericLibFunc for JumpNotZeroGeneric {
    type Concrete = JumpNotZeroConcrete;
    const NAME: &'static str = "int_jump_nz";

    fn specialize(&self) -> Self::Concrete {
        JumpNotZeroConcrete {}
    }
}

pub struct JumpNotZeroConcrete {}
impl ConcreteLibFunc for JumpNotZeroConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec!["int".into()]
    }
    fn output_types(&self) -> Vec<Vec<ConcreteTypeId>> {
        vec![vec!["int".into()], vec![]]
    }
    fn fallthrough(&self) -> Option<usize> {
        Some(1)
    }
}

/// LibFunc for unwrapping a non-zero int back into a regular int.
#[derive(Default)]
pub struct UnwrapNonZeroGeneric {}
impl NoGenericArgsGenericLibFunc for UnwrapNonZeroGeneric {
    type Concrete = UnwrapNonZeroConcrete;
    const NAME: &'static str = "int_unwrap_nz";

    fn specialize(&self) -> Self::Concrete {
        UnwrapNonZeroConcrete {}
    }
}

pub struct UnwrapNonZeroConcrete {}
impl NonBranchConcreteLibFunc for UnwrapNonZeroConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec!["int_nonzero".into()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec!["int".into()]
    }
}
