use std::collections::HashMap;

use casm::ap_change::{ApChange, ApChangeError, ApplyApChange};
use casm::operand::{
    DerefOperand, DerefOrImmediate, DoubleDerefOperand, ImmediateOperand, Register,
};
use sierra::extensions::wrapping_arithmetic::Operator;
use sierra::ids::{ConcreteTypeId, VarId};
use sierra::program::{Function, StatementIdx};
use thiserror::Error;

use crate::type_sizes::TypeSizeMap;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum ReferencesError {
    #[error("Invalid function declaration.")]
    InvalidFunctionDeclaration(Function),
    #[error("{var_id} is dangling at #{statement_idx}.")]
    DanglingReferences { statement_idx: StatementIdx, var_id: VarId },
    #[error(
        "One of the arguments does not match the expected type of the libfunc or return statement."
    )]
    InvalidReferenceTypeForArgument,
}

pub type StatementRefs = HashMap<VarId, ReferenceValue>;

/// A Sierra reference to a value.
/// Corresponds to an argument or return value of a Sierra statement.
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
    AllocateSegment,
    Deref(DerefOperand),
    DoubleDeref(DoubleDerefOperand),
    IntoSingleCellRef(DerefOperand),
    Immediate(ImmediateOperand),
    BinOp(BinOpExpression),
    Complex(Vec<ReferenceExpression>),
}

impl ApplyApChange for ReferenceExpression {
    fn apply_ap_change(self, ap_change: ApChange) -> Result<Self, ApChangeError> {
        Ok(match self {
            ReferenceExpression::AllocateSegment => ReferenceExpression::AllocateSegment,
            ReferenceExpression::Deref(operand) => {
                ReferenceExpression::Deref(operand.apply_ap_change(ap_change)?)
            }
            ReferenceExpression::DoubleDeref(operand) => {
                ReferenceExpression::DoubleDeref(operand.apply_ap_change(ap_change)?)
            }
            ReferenceExpression::IntoSingleCellRef(operand) => {
                ReferenceExpression::IntoSingleCellRef(operand.apply_ap_change(ap_change)?)
            }
            ReferenceExpression::Immediate(operand) => {
                ReferenceExpression::Immediate(operand.apply_ap_change(ap_change)?)
            }
            ReferenceExpression::BinOp(operand) => {
                ReferenceExpression::BinOp(operand.apply_ap_change(ap_change)?)
            }
            ReferenceExpression::Complex(elements) => {
                let res_vec: Result<Vec<ReferenceExpression>, ApChangeError> = elements
                    .into_iter()
                    .map(|ref_expr| ref_expr.apply_ap_change(ap_change))
                    .collect();
                ReferenceExpression::Complex(res_vec?)
            }
        })
    }
}

/// Builds the HashMap of references to the arguments of a function.
pub fn build_function_arguments_refs(
    func: &Function,
    type_sizes: &TypeSizeMap,
) -> Result<StatementRefs, ReferencesError> {
    let mut refs = HashMap::with_capacity(func.params.len());
    let mut offset = -3;
    for param in func.params.iter().rev() {
        let size = type_sizes
            .get(&param.ty)
            .ok_or_else(|| ReferencesError::InvalidFunctionDeclaration(func.clone()))?;
        if refs
            .insert(
                param.id.clone(),
                ReferenceValue {
                    expression: if *size == 1 {
                        ReferenceExpression::Deref(DerefOperand { register: Register::FP, offset })
                    } else {
                        ReferenceExpression::Complex(
                            ((offset - size + 1)..offset + 1)
                                .map(|i| {
                                    ReferenceExpression::Deref(DerefOperand {
                                        register: Register::FP,
                                        offset: i,
                                    })
                                })
                                .collect(),
                        )
                    },
                    ty: param.ty.clone(),
                },
            )
            .is_some()
        {
            return Err(ReferencesError::InvalidFunctionDeclaration(func.clone()));
        }
        offset -= size;
    }
    Ok(refs)
}

/// Checks that the list of references contains types matching the given types.
pub fn check_types_match(
    refs: &[ReferenceValue],
    types: &[ConcreteTypeId],
) -> Result<(), ReferencesError> {
    if itertools::equal(types.iter(), refs.iter().map(|r| &r.ty)) {
        Ok(())
    } else {
        Err(ReferencesError::InvalidReferenceTypeForArgument)
    }
}
