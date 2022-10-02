use std::fmt::Display;

use thiserror::Error;

use crate::operand::{BinOpOperand, CellRef, DerefOrImmediate, Register, ResOperand};

#[cfg(test)]
#[path = "ap_change_test.rs"]
mod test;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum ApChange {
    Known(i16),
    Unknown,
}
impl Display for ApChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ApChange::Known(change) => write!(f, "ApChange::Known({})", change),
            ApChange::Unknown => write!(f, "ApChange::Unknown"),
        }
    }
}

#[derive(Debug, Error, Eq, PartialEq)]
pub enum ApChangeError {
    #[error("Unknown ap change")]
    UnknownApChange,
    #[error("Offset overflow")]
    OffsetOverflow,
}

pub trait ApplyApChange: Sized {
    fn apply_ap_change(self, ap_change: ApChange) -> Result<Self, ApChangeError>;
}

impl ApplyApChange for ResOperand {
    fn apply_ap_change(self, ap_change: ApChange) -> Result<Self, ApChangeError> {
        Ok(match self {
            ResOperand::Deref(operand) | ResOperand::DoubleDeref(operand, _) => {
                ResOperand::Deref(operand.apply_ap_change(ap_change)?)
            }
            ResOperand::Immediate(operand) => ResOperand::Immediate(operand),
            ResOperand::BinOp(operand) => ResOperand::BinOp(operand.apply_ap_change(ap_change)?),
        })
    }
}

impl ApplyApChange for CellRef {
    fn apply_ap_change(self, ap_change: ApChange) -> Result<Self, ApChangeError> {
        match self {
            CellRef { register: Register::AP, offset } => match ap_change {
                ApChange::Unknown => Err(ApChangeError::UnknownApChange),
                ApChange::Known(ap_change) => Ok(CellRef {
                    register: Register::AP,
                    offset: offset.checked_sub(ap_change).ok_or(ApChangeError::OffsetOverflow)?,
                }),
            },
            CellRef { register: Register::FP, offset: _ } => Ok(self),
        }
    }
}

impl ApplyApChange for DerefOrImmediate {
    fn apply_ap_change(self, ap_change: ApChange) -> Result<Self, ApChangeError> {
        Ok(match self {
            DerefOrImmediate::Deref(operand) => {
                DerefOrImmediate::Deref(operand.apply_ap_change(ap_change)?)
            }
            DerefOrImmediate::Immediate(operand) => DerefOrImmediate::Immediate(operand),
        })
    }
}

impl ApplyApChange for BinOpOperand {
    fn apply_ap_change(self, ap_change: ApChange) -> Result<Self, ApChangeError> {
        Ok(BinOpOperand {
            op: self.op,
            a: self.a.apply_ap_change(ap_change)?,
            b: self.b.apply_ap_change(ap_change)?,
        })
    }
}
