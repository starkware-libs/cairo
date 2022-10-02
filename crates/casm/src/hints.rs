use std::fmt::Display;

use crate::operand::DerefOperand;

#[cfg(test)]
#[path = "hints_test.rs"]
mod test;

// Represents a cairo hint.
#[derive(Debug, Eq, PartialEq)]
pub enum Hint {
    AllocSegment { dst: DerefOperand },
}

impl Display for Hint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{{ ")?;
        match self {
            Hint::AllocSegment { dst } => write!(f, "memory{} = segments.add()", dst)?,
        }
        write!(f, " %}}")
    }
}
