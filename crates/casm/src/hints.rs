use std::fmt::{Display, Formatter};

use crate::operand::{CellRef, DerefOrImmediate};

#[cfg(test)]
#[path = "hints_test.rs"]
mod test;

// Represents a cairo hint.
#[derive(Debug, Eq, PartialEq)]
pub enum Hint {
    AllocSegment { dst: CellRef },
    TestLessThan { lhs: DerefOrImmediate, rhs: DerefOrImmediate },
}

impl Display for Hint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let fmt_access_or_const = |f: &mut Formatter<'_>, v: &DerefOrImmediate| match v {
            DerefOrImmediate::Deref(d) => write!(f, "memory{d}"),
            DerefOrImmediate::Immediate(i) => write!(f, "{i}"),
        };
        write!(f, "%{{ ")?;
        match self {
            Hint::AllocSegment { dst } => write!(f, "memory{dst} = segments.add()")?,
            Hint::TestLessThan { lhs, rhs } => {
                write!(f, "memory[ap + 0] = ")?;
                fmt_access_or_const(f, lhs)?;
                write!(f, " < ")?;
                fmt_access_or_const(f, rhs)?;
            }
        }
        write!(f, " %}}")
    }
}
