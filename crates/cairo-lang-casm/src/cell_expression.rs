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

impl core::fmt::Display for CellOperator {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            CellOperator::Add => write!(f, "+"),
            CellOperator::Sub => write!(f, "-"),
            CellOperator::Mul => write!(f, "*"),
            CellOperator::Div => write!(f, "/"),
        }
    }
}

/// The expression representing a cell in the casm memory.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CellExpression {
    Deref(CellRef),
    /// Represents an expression of the form `[[cell_ref] + offset]`.
    DoubleDeref(CellRef, i16),
    Immediate(BigInt),
    /// Represents an expression of the form `[cell_ref] + [cell_ref]` or `[cell_ref] + imm`.
    ///
    /// If `op` is [CellOperator::Div], `b` must not be zero.
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
            ResOperand::Immediate(imm) => Self::Immediate(imm.value),
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
            CellExpression::Immediate(imm) => Some(imm.clone().into()),
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
            } => Some((*cell, offset.value.to_i16()?)),
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

impl core::fmt::Display for CellExpression {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            CellExpression::Deref(cell) => write!(f, "{cell}"),
            CellExpression::DoubleDeref(cell, offset) => write!(f, "[{cell} + {offset}]"),
            CellExpression::Immediate(imm) => write!(f, "{}", imm),
            CellExpression::BinOp { op, a, b } => write!(f, "{a} {op} {b}"),
        }
    }
}
