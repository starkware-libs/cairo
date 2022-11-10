use std::fmt::Display;

use thiserror::Error;
use utils::casts::usize_as_i16;

use crate::operand::{BinOpOperand, CellRef, DerefOrImmediate, Register, ResOperand};

#[cfg(test)]
#[path = "ap_change_test.rs"]
mod test;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum ApChange {
    Known(usize),
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

/// Trait for applying ap changes.
pub trait ApplyApChange: Sized {
    /// Attempts to apply ap change, fail on overflow only.
    fn apply_known_ap_change(self, ap_change: usize) -> Option<Self>;
    /// Can unknown ap change be applied.
    fn can_apply_unknown(&self) -> bool;

    /// Attempts to apply ap change.
    fn apply_ap_change(self, ap_change: ApChange) -> Result<Self, ApChangeError> {
        match ap_change {
            ApChange::Unknown if self.can_apply_unknown() => Ok(self),
            ApChange::Unknown => Err(ApChangeError::UnknownApChange),
            ApChange::Known(ap_change) => {
                self.apply_known_ap_change(ap_change).ok_or(ApChangeError::OffsetOverflow)
            }
        }
    }

    /// Same as [Self::apply_known_ap_change] but unchecked.
    fn unchecked_apply_known_ap_change(self, ap_change: usize) -> Self {
        self.apply_known_ap_change(ap_change).unwrap()
    }
}

impl ApplyApChange for ResOperand {
    fn apply_known_ap_change(self, ap_change: usize) -> Option<Self> {
        Some(match self {
            ResOperand::Deref(operand) => {
                ResOperand::Deref(operand.apply_known_ap_change(ap_change)?)
            }
            ResOperand::DoubleDeref(operand, offset) => {
                ResOperand::DoubleDeref(operand.apply_known_ap_change(ap_change)?, offset)
            }
            ResOperand::Immediate(value) => ResOperand::Immediate(value),
            ResOperand::BinOp(operand) => {
                ResOperand::BinOp(operand.apply_known_ap_change(ap_change)?)
            }
        })
    }

    fn can_apply_unknown(&self) -> bool {
        match self {
            ResOperand::Deref(operand) => operand.can_apply_unknown(),
            ResOperand::DoubleDeref(operand, ..) => operand.can_apply_unknown(),
            ResOperand::Immediate(_) => true,
            ResOperand::BinOp(operand) => operand.can_apply_unknown(),
        }
    }
}

impl ApplyApChange for CellRef {
    fn apply_known_ap_change(self, ap_change: usize) -> Option<Self> {
        Some(match &self.register {
            Register::AP => CellRef {
                register: Register::AP,
                offset: self.offset.checked_sub(usize_as_i16(ap_change))?,
            },
            Register::FP => self,
        })
    }

    fn can_apply_unknown(&self) -> bool {
        match &self.register {
            Register::AP => false,
            Register::FP => true,
        }
    }
}

impl ApplyApChange for DerefOrImmediate {
    fn apply_known_ap_change(self, ap_change: usize) -> Option<Self> {
        Some(match self {
            DerefOrImmediate::Deref(operand) => {
                DerefOrImmediate::Deref(operand.apply_known_ap_change(ap_change)?)
            }
            DerefOrImmediate::Immediate(operand) => DerefOrImmediate::Immediate(operand),
        })
    }

    fn can_apply_unknown(&self) -> bool {
        match self {
            DerefOrImmediate::Deref(operand) => operand.can_apply_unknown(),
            DerefOrImmediate::Immediate(_) => true,
        }
    }
}

impl ApplyApChange for BinOpOperand {
    fn apply_known_ap_change(self, ap_change: usize) -> Option<Self> {
        Some(BinOpOperand {
            op: self.op,
            a: self.a.apply_known_ap_change(ap_change)?,
            b: self.b.apply_known_ap_change(ap_change)?,
        })
    }

    fn can_apply_unknown(&self) -> bool {
        self.a.can_apply_unknown() && self.b.can_apply_unknown()
    }
}
