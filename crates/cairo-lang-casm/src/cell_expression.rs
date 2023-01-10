use cairo_lang_utils::try_extract_matches;
use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;

use crate::ap_change::ApplyApChange;
use crate::operand::{BinOpOperand, CellRef, DerefOrImmediate, Operation, ResOperand};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CellOperator {
    Add,
    Sub,
    Mul,
    Div,
}

/// The expression representing a cell in the casm memory.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CellExpression {
    Deref(CellRef),
    /// Represents an expression of the form `[[cell_ref] + offset]`.
    DoubleDeref(CellRef, i16),
    Immediate(BigInt),
    BinOp {
        op: CellOperator,
        a: CellRef,
        b: DerefOrImmediate,
    },
}
impl CellExpression {
    pub fn from_res_operand(operand: ResOperand) -> Self {
        match operand {
            ResOperand::Deref(cell) => Self::Deref(cell),
            ResOperand::DoubleDeref(cell, offset) => Self::DoubleDeref(cell, offset),
            ResOperand::Immediate(imm) => Self::Immediate(imm),
            ResOperand::BinOp(BinOpOperand { op, a, b }) => Self::BinOp {
                op: match op {
                    Operation::Add => CellOperator::Add,
                    Operation::Mul => CellOperator::Mul,
                },
                a,
                b,
            },
        }
    }

    /// Extract the cell reference from the cell expression.
    pub fn to_deref(&self) -> Option<CellRef> {
        try_extract_matches!(self, CellExpression::Deref).cloned()
    }

    /// Extract a deref or immediate from the cell expression.
    pub fn to_deref_or_immediate(&self) -> Option<DerefOrImmediate> {
        match self {
            CellExpression::Deref(cell) => Some(DerefOrImmediate::Deref(*cell)),
            CellExpression::Immediate(imm) => Some(DerefOrImmediate::Immediate(imm.clone())),
            _ => None,
        }
    }

    /// Given `[ref] + offset` returns `([ref], offset)`.
    pub fn to_deref_with_offset(&self) -> Option<(CellRef, i16)> {
        match self {
            CellExpression::Deref(cell) => Some((*cell, 0i16)),
            CellExpression::BinOp {
                op: CellOperator::Add,
                a: cell,
                b: DerefOrImmediate::Immediate(offset),
            } => Some((*cell, offset.to_i16()?)),
            _ => None,
        }
    }

    /// Returns the reference as a buffer with at least `required_slack` next cells that can be
    /// written as an instruction offset.
    pub fn to_buffer(&self, required_slack: i16) -> Option<CellExpression> {
        let (base, offset) = self.to_deref_with_offset()?;
        offset.checked_add(required_slack)?;
        if offset == 0 {
            Some(CellExpression::Deref(base))
        } else {
            Some(CellExpression::BinOp {
                op: CellOperator::Add,
                a: base,
                b: DerefOrImmediate::Immediate(offset.into()),
            })
        }
    }
}

impl ApplyApChange for CellExpression {
    fn apply_known_ap_change(self, ap_change: usize) -> Option<Self> {
        Some(match self {
            CellExpression::Deref(operand) => {
                CellExpression::Deref(operand.apply_known_ap_change(ap_change)?)
            }
            CellExpression::DoubleDeref(operand, offset) => {
                CellExpression::DoubleDeref(operand.apply_known_ap_change(ap_change)?, offset)
            }
            CellExpression::BinOp { op, a, b } => CellExpression::BinOp {
                op,
                a: a.apply_known_ap_change(ap_change)?,
                b: b.apply_known_ap_change(ap_change)?,
            },
            expr @ CellExpression::Immediate(_) => expr,
        })
    }

    fn can_apply_unknown(&self) -> bool {
        match self {
            CellExpression::Deref(operand) | CellExpression::DoubleDeref(operand, _) => {
                operand.can_apply_unknown()
            }
            CellExpression::Immediate(_) => true,
            CellExpression::BinOp { a, b, .. } => a.can_apply_unknown() && b.can_apply_unknown(),
        }
    }
}
