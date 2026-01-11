use core::fmt::Display;

use num_traits::ToPrimitive;

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
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            ApChange::Known(change) => write!(f, "ApChange::Known({change})"),
            ApChange::Unknown => write!(f, "ApChange::Unknown"),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ApChangeError {
    UnknownApChange,
    OffsetOverflow,
}

impl Display for ApChangeError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            ApChangeError::UnknownApChange => write!(f, "Unknown ap change"),
            ApChangeError::OffsetOverflow => write!(f, "Offset overflow"),
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for ApChangeError {}

/// Trait for applying AP changes.
pub trait ApplyApChange: Sized {
    /// Attempts to apply an AP change; fails only on overflow.
    fn apply_known_ap_change(&mut self, ap_change: usize) -> bool;
    /// Can unknown AP change be applied.
    fn can_apply_unknown(&self) -> bool;

    /// Attempts to apply an AP change.
    fn apply_ap_change(&mut self, ap_change: ApChange) -> Result<(), ApChangeError> {
        match ap_change {
            ApChange::Unknown if self.can_apply_unknown() => Ok(()),
            ApChange::Unknown => Err(ApChangeError::UnknownApChange),
            ApChange::Known(ap_change) => {
                if self.apply_known_ap_change(ap_change) {
                    Ok(())
                } else {
                    Err(ApChangeError::OffsetOverflow)
                }
            }
        }
    }

    /// Same as [Self::apply_known_ap_change] but unchecked.
    fn unchecked_apply_known_ap_change(mut self, ap_change: usize) -> Self {
        assert!(self.apply_known_ap_change(ap_change));
        self
    }
}

impl ApplyApChange for ResOperand {
    fn apply_known_ap_change(&mut self, ap_change: usize) -> bool {
        match self {
            ResOperand::Deref(operand) => operand.apply_known_ap_change(ap_change),
            ResOperand::DoubleDeref(operand, _) => operand.apply_known_ap_change(ap_change),
            ResOperand::Immediate(_) => true,
            ResOperand::BinOp(operand) => operand.apply_known_ap_change(ap_change),
        }
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
    fn apply_known_ap_change(&mut self, ap_change: usize) -> bool {
        match &self.register {
            Register::FP => true,
            Register::AP => {
                if let Some(offset) =
                    ap_change.to_i16().and_then(|ap_change| self.offset.checked_sub(ap_change))
                {
                    self.offset = offset;
                    true
                } else {
                    false
                }
            }
        }
    }

    fn can_apply_unknown(&self) -> bool {
        match &self.register {
            Register::AP => false,
            Register::FP => true,
        }
    }
}

impl ApplyApChange for DerefOrImmediate {
    fn apply_known_ap_change(&mut self, ap_change: usize) -> bool {
        match self {
            DerefOrImmediate::Deref(operand) => operand.apply_known_ap_change(ap_change),
            DerefOrImmediate::Immediate(_) => true,
        }
    }

    fn can_apply_unknown(&self) -> bool {
        match self {
            DerefOrImmediate::Deref(operand) => operand.can_apply_unknown(),
            DerefOrImmediate::Immediate(_) => true,
        }
    }
}

impl ApplyApChange for BinOpOperand {
    fn apply_known_ap_change(&mut self, ap_change: usize) -> bool {
        self.a.apply_known_ap_change(ap_change) && self.b.apply_known_ap_change(ap_change)
    }

    fn can_apply_unknown(&self) -> bool {
        self.a.can_apply_unknown() && self.b.can_apply_unknown()
    }
}
