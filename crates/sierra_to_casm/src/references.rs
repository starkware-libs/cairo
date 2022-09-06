use std::collections::HashMap;

use casm::ap_change::{ApChange, ApChangeError, ApplyApChange};
use casm::operand::{
    DerefOperand, DerefOrImmediate, DoubleDerefOperand, ImmediateOperand, Register,
};
use sierra::extensions::arithmetic::Operator;
use sierra::ids::{ConcreteTypeId, VarId};
use sierra::program::{Function, StatementIdx};
use thiserror::Error;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum ReferencesError {
    #[error(transparent)]
    ApChangeError(#[from] ApChangeError),
    #[error("Invalid function declaration.")]
    InvalidFunctionDeclaration(Function),
    #[error("DanglingReferences")]
    DanglingReferences(StatementIdx),
}

pub type StatementRefs = HashMap<VarId, ReferenceValue>;

/// A reference to a value.
/// Corresponds to an argument or return value of a sierra statement.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ReferenceValue {
    pub expression: ReferenceExpression,
    pub ty: ConcreteTypeId,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BinOpExpression {
    pub op: Operator,
    pub a: DerefOperand,
    pub b: DerefOrImmediate,
}
impl ApplyApChange for BinOpExpression {
    fn apply_ap_change(self, ap_change: ApChange) -> Result<Self, ApChangeError> {
        Ok(BinOpExpression {
            op: self.op,
            a: self.a.apply_ap_change(ap_change)?,
            b: self.b.apply_ap_change(ap_change)?,
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ReferenceExpression {
    Deref(DerefOperand),
    DoubleDeref(DoubleDerefOperand),
    Immediate(ImmediateOperand),
    BinOp(BinOpExpression),
}

impl ApplyApChange for ReferenceExpression {
    fn apply_ap_change(self, ap_change: ApChange) -> Result<Self, ApChangeError> {
        Ok(match self {
            ReferenceExpression::Deref(operand) => {
                ReferenceExpression::Deref(operand.apply_ap_change(ap_change)?)
            }
            ReferenceExpression::DoubleDeref(operand) => {
                ReferenceExpression::DoubleDeref(operand.apply_ap_change(ap_change)?)
            }
            ReferenceExpression::Immediate(operand) => {
                ReferenceExpression::Immediate(operand.apply_ap_change(ap_change)?)
            }
            ReferenceExpression::BinOp(operand) => {
                ReferenceExpression::BinOp(operand.apply_ap_change(ap_change)?)
            }
        })
    }
}

/// Builds the HashMap of references to the parameters of a function.
pub fn build_function_parameter_refs(func: &Function) -> Result<StatementRefs, ReferencesError> {
    let mut refs = HashMap::with_capacity(func.params.len());

    let mut offset = -2;
    for param in func.params.iter().rev() {
        if refs
            .insert(
                param.id.clone(),
                ReferenceValue {
                    expression: ReferenceExpression::Deref(DerefOperand {
                        register: Register::FP,
                        offset,
                    }),
                    ty: param.ty.clone(),
                },
            )
            .is_some()
        {
            return Err(ReferencesError::InvalidFunctionDeclaration(func.clone()));
        }
        // TODO(ilya, 10/10/2022): Get size from type.
        let size = 1;
        offset -= size;
    }

    Ok(refs)
}
